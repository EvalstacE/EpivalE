#' Pull Daily Historical Weather from Open-Meteo
#'
#' @description Queries Open-Meteo's Historical Weather API directly via `httr2`
#'   and returns a join-ready daily tibble keyed by `date_clean`, including
#'   daily maximum snow depth on the ground and a derived snow-cover flag.
#'   Note: Open-Meteo's historical archive typically lags real time by 2-5 days;
#'   recent unfinalized dates return `NA`.
#'
#' @param start_date Start date (coercible via `lubridate::as_date()`).
#' @param end_date End date. Defaults to today in `"America/Denver"`.
#' @param location Numeric vector `c(latitude, longitude)` in WGS84. Defaults to
#'   Helena, MT (`c(46.58, -112.04)`).
#' @param units Unit system: `"imperial"` (`fahrenheit`, `mph`, `inch`; default)
#'   or `"metric"` (`celsius`, `kmh`, `mm` / `cm`).
#' @param snow_cover_min Optional numeric minimum daily max snow depth required
#'   to classify `wx_snow_cover` as `TRUE` (in inches for `"imperial"` or cm for
#'   `"metric"`). Defaults to `0.5` in (NWS measurable snow depth) or `1.0` cm.
#' @param timezone IANA timezone string. Defaults to `"America/Denver"`.
#' @param timeout_sec HTTP request timeout in seconds (default `60`).
#'
#' @return A tibble with `date_clean` and `wx_*` daily weather columns, or
#'   `NULL` if the Open-Meteo request fails.
#' @importFrom rlang .data
#' @export
wx_get_daily <- function(
    start_date,
    end_date       = lubridate::today(tzone = "America/Denver"),
    location       = c(46.58, -112.04),
    units          = c("imperial", "metric"),
    snow_cover_min = NULL,
    timezone       = "America/Denver",
    timeout_sec    = 60
) {
  rlang::check_installed("httr2", reason = "to query the Open-Meteo weather API.")
  
  units   <- match.arg(units)
  start_d <- lubridate::as_date(start_date)
  end_d   <- lubridate::as_date(end_date)
  
  # Fail-fast input checks
  if (is.na(start_d) || is.na(end_d) || start_d > end_d) {
    rlang::abort("`start_date` and `end_date` must be valid dates with `start_date <= end_date`.")
  }
  if (!is.numeric(location) || length(location) != 2L || anyNA(location) ||
      location[1] < -90 || location[1] > 90 ||
      location[2] < -180 || location[2] > 180) {
    rlang::abort("`location` must be a valid numeric `c(latitude, longitude)` vector.")
  }
  if (!is.character(timezone) || length(timezone) != 1L || !nzchar(trimws(timezone))) {
    rlang::abort("`timezone` must be a single non-empty IANA timezone string.")
  }
  if (!is.numeric(timeout_sec) || length(timeout_sec) != 1L || !is.finite(timeout_sec) || timeout_sec <= 0) {
    rlang::abort("`timeout_sec` must be a single positive number.")
  }
  
  if (is.null(snow_cover_min)) {
    snow_cover_min <- if (units == "imperial") 0.5 else 1.0
  }
  if (!is.numeric(snow_cover_min) || length(snow_cover_min) != 1L ||
      !is.finite(snow_cover_min) || snow_cover_min < 0) {
    rlang::abort("`snow_cover_min` must be `NULL` or a single non-negative number.")
  }
  
  daily_vars <- c(
    "temperature_2m_max",
    "temperature_2m_min",
    "relative_humidity_2m_mean",
    "wind_speed_10m_max",
    "wind_speed_10m_mean",
    "snowfall_sum",
    "precipitation_sum"
  )
  
  unit_params <- if (units == "imperial") {
    list(temperature_unit = "fahrenheit", wind_speed_unit = "mph", precipitation_unit = "inch")
  } else {
    list(temperature_unit = "celsius", wind_speed_unit = "kmh", precipitation_unit = "mm")
  }
  
  body <- tryCatch({
    httr2::request("https://archive-api.open-meteo.com/v1/archive") %>%
      httr2::req_url_query(
        latitude           = location[1],
        longitude          = location[2],
        start_date         = format(start_d, "%Y-%m-%d"),
        end_date           = format(end_d,   "%Y-%m-%d"),
        daily              = paste(daily_vars, collapse = ","),
        hourly             = "snow_depth",
        timezone           = trimws(timezone),
        temperature_unit   = unit_params$temperature_unit,
        wind_speed_unit    = unit_params$wind_speed_unit,
        precipitation_unit = unit_params$precipitation_unit
      ) %>%
      httr2::req_user_agent("EpivalE (https://github.com/EvalstacE/EpivalE)") %>%
      httr2::req_timeout(timeout_sec) %>%
      httr2::req_retry(max_tries = 3L, retry_on_failure = TRUE) %>%
      httr2::req_perform() %>%
      httr2::resp_body_json(simplifyVector = FALSE)
  }, error = function(e) {
    warning("Failed to fetch Open-Meteo weather data: ", conditionMessage(e), call. = FALSE)
    NULL
  })
  
  if (is.null(body) || is.null(body$daily) || is.null(body$daily$time)) {
    return(NULL)
  }
  
  # Safely convert JSON arrays that may contain NULLs (e.g., unfinalized recent days)
  to_dbl <- function(x, n) {
    if (is.null(x)) return(rep(NA_real_, n))
    vapply(x, function(v) if (is.null(v) || is.na(v)) NA_real_ else as.numeric(v), numeric(1))
  }
  
  dates <- as.Date(vapply(body$daily$time, as.character, character(1)))
  n     <- length(dates)
  d     <- body$daily
  
  out <- tibble::tibble(
    date_clean  = dates,
    wx_temp_max = round(to_dbl(d$temperature_2m_max,        n), 1),
    wx_temp_min = round(to_dbl(d$temperature_2m_min,        n), 1),
    wx_rh_mean  = round(to_dbl(d$relative_humidity_2m_mean, n), 1),
    wx_wind_max = round(to_dbl(d$wind_speed_10m_max,        n), 1),
    wx_wind_avg = round(to_dbl(d$wind_speed_10m_mean,       n), 1),
    wx_snowfall = round(to_dbl(d$snowfall_sum,              n), 2),
    wx_precip   = round(to_dbl(d$precipitation_sum,         n), 2)
  )
  
  # Roll up hourly snow_depth (returned in meters by Open-Meteo) to daily max
  if (!is.null(body$hourly) && !is.null(body$hourly$time) && !is.null(body$hourly$snow_depth)) {
    h_time  <- vapply(body$hourly$time, as.character, character(1))
    h_depth <- to_dbl(body$hourly$snow_depth, length(h_time))
    
    raw_unit <- if (!is.null(body$hourly_units) && !is.null(body$hourly_units$snow_depth)) {
      tolower(as.character(body$hourly_units$snow_depth))
    } else {
      "m"
    }
    
    # Convert to inches (imperial) or cm (metric, matching snowfall_sum)
    h_depth_conv <- if (units == "imperial") {
      dplyr::case_when(
        raw_unit == "m"              ~ h_depth * 39.3700787,
        raw_unit == "cm"             ~ h_depth / 2.54,
        raw_unit == "mm"             ~ h_depth / 25.4,
        raw_unit %in% c("inch", "in") ~ h_depth,
        TRUE                         ~ h_depth * 39.3700787
      )
    } else {
      dplyr::case_when(
        raw_unit == "m"              ~ h_depth * 100,
        raw_unit == "cm"             ~ h_depth,
        raw_unit == "mm"             ~ h_depth / 10,
        raw_unit %in% c("inch", "in") ~ h_depth * 2.54,
        TRUE                         ~ h_depth * 100
      )
    }
    
    snow_daily <- tibble::tibble(
      date_clean = as.Date(substr(h_time, 1L, 10L)),
      snow_depth = h_depth_conv
    ) %>%
      dplyr::summarise(
        wx_snow_depth = if (all(is.na(.data$snow_depth))) {
          NA_real_
        } else {
          round(max(.data$snow_depth, na.rm = TRUE), 2)
        },
        .by = "date_clean"
      )
    
    out <- dplyr::left_join(out, snow_daily, by = "date_clean")
  } else {
    out$wx_snow_depth <- NA_real_
  }
  
  out %>%
    dplyr::mutate(
      wx_snow_cover = dplyr::if_else(
        is.na(.data$wx_snow_depth),
        NA,
        .data$wx_snow_depth >= snow_cover_min
      )
    )
}


#' Empty daily weather template for schema-safe fallback joins
#'
#' @param dates Optional vector of `Date` values.
#' @return A tibble with the exact column schema of [wx_get_daily()], filled with `NA`.
#' @export
wx_empty_daily <- function(dates = as.Date(character())) {
  n <- length(dates)
  tibble::tibble(
    date_clean    = lubridate::as_date(dates),
    wx_temp_max   = rep(NA_real_, n),
    wx_temp_min   = rep(NA_real_, n),
    wx_rh_mean    = rep(NA_real_, n),
    wx_wind_max   = rep(NA_real_, n),
    wx_wind_avg   = rep(NA_real_, n),
    wx_snowfall   = rep(NA_real_, n),
    wx_precip     = rep(NA_real_, n),
    wx_snow_depth = rep(NA_real_, n),
    wx_snow_cover = rep(NA, n)
  )
}