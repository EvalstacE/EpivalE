#' Pull Daily Historical Weather from Open-Meteo
#'
#' @description Queries Open-Meteo's Historical Weather API directly via `httr2`
#'   and returns a join-ready daily tibble keyed by `date_clean`.
#'
#' @param start_date Start date (coercible via `lubridate::as_date()`).
#' @param end_date End date. Defaults to today in `"America/Denver"`.
#' @param location Numeric vector `c(latitude, longitude)` in WGS84. Defaults to
#'   Helena, MT (`c(46.58, -112.04)`).
#' @param units Unit system: `"imperial"` (`fahrenheit`, `mph`, `inch`; default)
#'   or `"metric"` (`celsius`, `kmh`, `mm` / `cm`).
#' @param timezone IANA timezone string. Defaults to `"America/Denver"`.
#' @param timeout_sec HTTP request timeout in seconds (default `60`).
#'
#' @return A tibble with `date_clean` and `wx_*` daily weather columns, or
#'   `NULL` if the Open-Meteo request fails.
#' @export
wx_get_daily <- function(
    start_date,
    end_date    = lubridate::today(tzone = "America/Denver"),
    location    = c(46.58, -112.04),
    units       = c("imperial", "metric"),
    timezone    = "America/Denver",
    timeout_sec = 60
) {
  units   <- match.arg(units)
  start_d <- lubridate::as_date(start_date)
  end_d   <- lubridate::as_date(end_date)
  
  if (is.na(start_d) || is.na(end_d) || start_d > end_d) {
    stop("`start_date` and `end_date` must be valid dates with start_date <= end_date.", call. = FALSE)
  }
  if (!is.numeric(location) || length(location) != 2L || anyNA(location)) {
    stop("`location` must be a numeric vector of length 2: c(latitude, longitude).", call. = FALSE)
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
        timezone           = timezone,
        temperature_unit   = unit_params$temperature_unit,
        wind_speed_unit    = unit_params$wind_speed_unit,
        precipitation_unit = unit_params$precipitation_unit
      ) %>%
      httr2::req_user_agent("EpivalE (https://github.com/EvalstacE/EpivalE)") %>%
      httr2::req_timeout(timeout_sec) %>%
      httr2::req_retry(max_tries = 3L, retry_on_failure = TRUE) %>%
      httr2::req_perform() %>%
      httr2::resp_body_string() %>%
      jsonlite::fromJSON(simplifyVector = FALSE)
  }, error = function(e) {
    warning("Failed to fetch Open-Meteo weather data: ", conditionMessage(e), call. = FALSE)
    NULL
  })
  
  if (is.null(body) || is.null(body$daily) || is.null(body$daily$time)) {
    return(NULL)
  }
  
  # Safely convert JSON arrays that may contain NULLs (e.g., today's incomplete day)
  to_dbl <- function(x, n) {
    if (is.null(x)) return(rep(NA_real_, n))
    vapply(x, function(v) if (is.null(v)) NA_real_ else as.numeric(v), numeric(1))
  }
  
  dates <- as.Date(vapply(body$daily$time, as.character, character(1)))
  n     <- length(dates)
  
  d <- body$daily
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
  
  out
}

#' Empty daily weather template for schema-safe fallback joins
#'
#' @param dates Optional vector of `Date` values.
#' @return A tibble with the exact column schema of `wx_get_daily()`, filled with `NA_real_`.
#' @export
wx_empty_daily <- function(dates = as.Date(character())) {
  n <- length(dates)
  tibble::tibble(
    date_clean  = lubridate::as_date(dates),
    wx_temp_max = rep(NA_real_, n),
    wx_temp_min = rep(NA_real_, n),
    wx_rh_mean  = rep(NA_real_, n),
    wx_wind_max = rep(NA_real_, n),
    wx_wind_avg = rep(NA_real_, n),
    wx_snowfall = rep(NA_real_, n),
    wx_precip   = rep(NA_real_, n)
  )
}