#' Standard Montana DEQ PM2.5 field sets
#'
#' @param set Either `"minimal"` (fastest for daily summaries) or `"full"`
#'   (includes DEQ station metadata, nowcast, AQI, and health category).
#' @return A character vector of DEQ FeatureServer field names.
#' @export
deq_pm25_fields <- function(set = c("minimal", "full")) {
  set <- match.arg(set)
  
  switch(
    set,
    minimal = c("parameter", "instrumenttype", "date", "rawvalue"),
    full = c(
      "logger_id", "sitename", "aqs_code", "parameter", "instrumenttype",
      "date", "hour", "rawvalue", "stan", "nowcast", "aqi_value", "healthcategory"
    )
  )
}


#' Base SQL WHERE clause for Montana DEQ PM2.5 regulatory channels
#'
#' @param logger_id Single DEQ station logger ID (defaults to `"RP"` for
#'   Helena Rossiter Pump House).
#' @return A single SQL `WHERE` string (without date filtering).
#' @export
deq_pm25_where <- function(logger_id = "RP") {
  stopifnot(is.character(logger_id), length(logger_id) == 1L, nzchar(logger_id))
  
  sprintf(
    paste(
      "logger_id = '%s'",
      "AND ((parameter = 'PM25' AND instrumenttype = 'Regulatory')",
      "OR (parameter = 'PM25_COL' AND instrumenttype = 'Regulatory_2'))"
    ),
    logger_id
  )
}



#' Pull and Summarize MT DEQ Hourly PM2.5
#'
#' @description Reads in MT DEQ's hourly air quality API hosted via ArcGIS Online
#'   and calculates daily summaries pertinent to regulatory season (4-hr, 8-hr,
#'   and 24-hr averages plus QA/QC and completeness flags).
#'   NOTE: The API service only maintains hourly records from a rolling 13-month window.
#'
#' @param start_date Start date of query range (coercible via `lubridate::as_date()`).
#' @param end_date End date of query range. Defaults to today in `"America/Denver"`.
#' @param fields Character vector of DEQ fields to pull. Defaults to
#'   `deq_pm25_fields("minimal")`.
#' @param where_base Base SQL `WHERE` filter (station and channels). Defaults to
#'   `deq_pm25_where("RP")`.
#' @param deq_lyr Optional pre-opened `FeatureLayer` from `arcgislayers::arc_open()`,
#'   a FeatureServer layer URL string, or `NULL` (default) to use MT DEQ's public layer.
#' @param chunk_days Maximum number of days per batch pull (default `14L`) to stay
#'   under DEQ's 1,000-record API limit.
#' @param output One of `"daily"` (default), `"hourly"`, or `"both"`.
#' @param publish_lag Minimum lag after an hour closes before DEQ finalizes and
#'   publishes (e.g., `lubridate::minutes(20)`). Defaults to `lubridate::minutes(0)`.
#' @param pause_sec Brief pause in seconds between batch requests (default `0.25`).
#' @param verbose Logical; print progress messages?
#'
#' @return A tibble when `output` is `"daily"` or `"hourly"`, a named list
#'   (`list(hourly = ..., daily = ...)`) when `output = "both"`, or `NULL` if no
#'   records are returned.
#' @importFrom rlang .data
#' @export
aq_get_deq_pm25 <- function(
    start_date,
    end_date    = lubridate::today(tzone = "America/Denver"),
    fields      = deq_pm25_fields("minimal"),
    where_base  = deq_pm25_where("RP"),
    deq_lyr     = NULL,
    chunk_days  = 14L,
    output      = c("daily", "hourly", "both"),
    publish_lag = lubridate::minutes(0),
    pause_sec   = 0.25,
    verbose     = TRUE
) {
  rlang::check_installed(
    "arcgislayers",
    reason = "to query Montana DEQ FeatureServer layers."
  )
  
  output  <- match.arg(output)
  now_mt  <- lubridate::with_tz(Sys.time(), "America/Denver")
  start_d <- lubridate::as_date(start_date)
  end_d   <- lubridate::as_date(end_date)
  
# Fail-fast input checks
  if (is.na(start_d) || is.na(end_d) || start_d > end_d) {
    stop("`start_date` and `end_date` must be valid dates with start_date <= end_date.", call. = FALSE)
  }
  
  chunk_days <- as.integer(chunk_days)
  if (is.na(chunk_days) || chunk_days < 1L) {
    stop("`chunk_days` must be a positive integer.", call. = FALSE)
  }
  
  required_fields <- c("parameter", "instrumenttype", "date", "rawvalue")
  fields          <- unique(as.character(fields))
  missing_fields  <- setdiff(required_fields, fields)
  if (length(missing_fields) > 0L) {
    stop(
      "`fields` is missing required columns: ",
      paste(missing_fields, collapse = ", "),
      call. = FALSE
    )
  }
  
  if (!is.character(where_base) || length(where_base) != 1L || !nzchar(trimws(where_base))) {
    stop("`where_base` must be a single, non-empty SQL WHERE clause string.", call. = FALSE)
  }
  
#----------------------------------------------------------------------------#  
#----------------------------------------------------------------------------#
# Internal helpers
#----------------------------------------------------------------------------#
  first_valid <- function(x) {
    v <- x[!is.na(x)]
    if (length(v) == 0L) NA_real_ else v[1]
  }
  
  calc_reg_avg <- function(ts, vals, day_date, cutoff_hour, window_hrs) {
    cutoff_ts <- as.POSIXct(
      sprintf("%s %02d:00:00", day_date, cutoff_hour),
      tz = "America/Denver"
    )
    if (now_mt < cutoff_ts + publish_lag) return(NA_real_)
    
    window_vals <- vals[ts >= (cutoff_ts - lubridate::dhours(window_hrs)) & ts < cutoff_ts]
    if (all(is.na(window_vals))) return(NA_real_)
    
    round(mean(window_vals, na.rm = TRUE), 2)
  }
  
  count_reg_hrs <- function(ts, vals, day_date, cutoff_hour, window_hrs) {
    cutoff_ts <- as.POSIXct(
      sprintf("%s %02d:00:00", day_date, cutoff_hour),
      tz = "America/Denver"
    )
    if (now_mt < cutoff_ts + publish_lag) return(NA_integer_)
    
    sum(!is.na(vals[ts >= (cutoff_ts - lubridate::dhours(window_hrs)) & ts < cutoff_ts]))
  }
  
  build_completeness_note <- function(day_complete, n_valid, n_expected,
                                      n_9am_4, n_9am_8, n_4pm_4, n_4pm_8) {
    if (day_complete && n_valid == 0L) {
      return(sprintf("No valid hourly readings recorded (0/%d hrs)", n_expected))
    }
    
    notes <- character()
    
    if (day_complete && n_valid < 18L) {
      notes <- c(
        notes,
        sprintf("24-hr stats suppressed (%d/%d valid hrs; below 18-hr minimum)", n_valid, n_expected)
      )
    }
    
    win_names   <- c("9AM 4hr", "9AM 8hr", "4PM 4hr", "4PM 8hr")
    win_counts  <- c(n_9am_4, n_9am_8, n_4pm_4, n_4pm_8)
    win_targets <- c(4L, 8L, 4L, 8L)
    
    missing_wins <- win_names[!is.na(win_counts) & win_counts == 0L]
    if (length(missing_wins) > 0L) {
      notes <- c(notes, paste0("No data for ", paste(missing_wins, collapse = ", ")))
    }
    
    partial_idx <- which(!is.na(win_counts) & win_counts > 0L & win_counts < win_targets)
    if (length(partial_idx) > 0L) {
      partial_txt <- sprintf(
        "%s (%d/%d hrs)",
        win_names[partial_idx], win_counts[partial_idx], win_targets[partial_idx]
      )
      notes <- c(notes, paste0("Partial coverage: ", paste(partial_txt, collapse = ", ")))
    }
    
    if (length(notes) == 0L) NA_character_ else paste(notes, collapse = "; ")
  }
  
#----------------------------------------------------------------------------#
#----------------------------------------------------------------------------#  
#----------------------------------------------------------------------------#
# Open DEQ FeatureServer layer (supports NULL, URL string, or FeatureLayer)
#----------------------------------------------------------------------------#
  default_url <- "https://gis.mtdeq.us/mapping/rest/services/Montana_Air_Quality_Monitoring_Stations/FeatureServer/0"
  
  if (is.null(deq_lyr)) {
    deq_lyr <- arcgislayers::arc_open(default_url, token = NULL)
  } else if (is.character(deq_lyr) && length(deq_lyr) == 1L) {
    deq_lyr <- arcgislayers::arc_open(deq_lyr, token = NULL)
  }
  
#----------------------------------------------------------------------------#
# Fetch chunks
#----------------------------------------------------------------------------#
  chunk_starts <- seq(start_d, end_d, by = paste(chunk_days, "days"))
  chunk_ends   <- pmin(chunk_starts + lubridate::days(chunk_days), end_d + lubridate::days(1))
  n_chunks     <- length(chunk_starts)
  
  fetch_chunk <- function(c_start, c_end, max_tries = 3L) {
    chunk_where <- sprintf(
      "(%s) AND date >= TIMESTAMP '%s 00:00:00' AND date < TIMESTAMP '%s 00:00:00'",
      trimws(where_base),
      format(c_start, "%Y-%m-%d"),
      format(c_end,   "%Y-%m-%d")
    )
    
    for (attempt in seq_len(max_tries)) {
      res <- tryCatch(
        arcgislayers::arc_select(
          deq_lyr,
          where    = chunk_where,
          fields   = fields,
          geometry = FALSE,
          token    = NULL
        ),
        error = function(e) e
      )
      
      if (!inherits(res, "error")) return(res)
      
      if (attempt < max_tries) {
        if (verbose) message("  Retry ", attempt, " for ", c_start, ": ", conditionMessage(res))
        Sys.sleep(2^attempt)
      } else {
        warning(
          "Chunk ", c_start, " to ", c_end - lubridate::days(1),
          " failed: ", conditionMessage(res),
          call. = FALSE
        )
        return(NULL)
      }
    }
  }
  
  batches <- vector("list", n_chunks)
  
  for (i in seq_len(n_chunks)) {
    if (verbose) {
      message(sprintf(
        "[%d/%d] Fetching %s to %s...",
        i, n_chunks, chunk_starts[i], chunk_ends[i] - lubridate::days(1)
      ))
    }
    
    res <- fetch_chunk(chunk_starts[i], chunk_ends[i])
    if (!is.null(res) && nrow(res) > 0L) {
      batches[[i]] <- res
    }
    
    if (i < n_chunks && pause_sec > 0) Sys.sleep(pause_sec)
  }
  
  aq_raw_hourly <- dplyr::bind_rows(batches)
  
  if (nrow(aq_raw_hourly) == 0L) {
    warning("No records returned for ", start_d, " to ", end_d, ".", call. = FALSE)
    return(NULL)
  }
  
#----------------------------------------------------------------------------#
#----------------------------------------------------------------------------#   
#----------------------------------------------------------------------------#
# Deduplicate channels + QA/QC checks
#----------------------------------------------------------------------------#
  aq_dedup <- aq_raw_hourly |>
    dplyr::mutate(
      date_mt   = lubridate::with_tz(.data$date, "America/Denver"),
      raw_clean = pmax(.data$rawvalue, 0)
    ) |>
    dplyr::summarise(
      reg2 = first_valid(.data$raw_clean[.data$parameter == "PM25_COL" & .data$instrumenttype == "Regulatory_2"]),
      reg1 = first_valid(.data$raw_clean[.data$parameter == "PM25"     & .data$instrumenttype == "Regulatory"]),
      .by = "date_mt"
    ) |>
    dplyr::mutate(
      raw_clean = dplyr::coalesce(.data$reg2, .data$reg1),
      other_val = dplyr::if_else(!is.na(.data$reg2), .data$reg1, .data$reg2),
      parameter = dplyr::case_when(
        !is.na(.data$reg2) ~ "PM25_COL",
        !is.na(.data$reg1) ~ "PM25",
        TRUE               ~ NA_character_
      ),
      pm25 = round(.data$raw_clean)
    )
  
  hour_spine <- tibble::tibble(
    date_mt = seq(
      from = as.POSIXct(sprintf("%s 00:00:00", start_d), tz = "America/Denver"),
      to   = as.POSIXct(sprintf("%s 23:00:00", end_d),   tz = "America/Denver"),
      by   = "1 hour"
    )
  )
  
  aq_hourly <- hour_spine |>
    dplyr::left_join(aq_dedup, by = "date_mt") |>
    dplyr::arrange(.data$date_mt) |>
    dplyr::mutate(
      day_mt      = lubridate::as_date(.data$date_mt),
      hour_mt     = lubridate::hour(.data$date_mt),
      date_end_mt = .data$date_mt + lubridate::dhours(1),
      date_clean  = lubridate::as_date(.data$date_end_mt),
      date_txt    = sub(" 0", " ", format(.data$date_clean, "%b %d")),
      year_txt    = format(.data$date_clean, "%Y"),
      hr_txt      = sub("^0", "", format(.data$date_end_mt, "%I %p")),
      
      prev_val     = dplyr::lag(.data$raw_clean, 1),
      next_val     = dplyr::lead(.data$raw_clean, 1),
      neighbor_ref = dplyr::case_when(
        !is.na(.data$prev_val) & !is.na(.data$next_val) ~ pmax(.data$prev_val, .data$next_val),
        !is.na(.data$prev_val)                          ~ .data$prev_val,
        !is.na(.data$next_val)                          ~ .data$next_val,
        TRUE                                            ~ NA_real_
      ),
      
      flag_collocated = dplyr::coalesce(
        !is.na(.data$raw_clean) & !is.na(.data$other_val) &
          (.data$raw_clean - .data$other_val) >= 35 &
          .data$raw_clean >= 3 * pmax(.data$other_val, 5),
        FALSE
      ),
      flag_spike = dplyr::coalesce(
        !is.na(.data$raw_clean) & !is.na(.data$neighbor_ref) &
          .data$raw_clean >= 50 &
          (.data$raw_clean - .data$neighbor_ref) >= 50 &
          .data$raw_clean >= 3 * pmax(.data$neighbor_ref, 5),
        FALSE
      ),
      flag_extreme = dplyr::coalesce(.data$raw_clean >= 500, FALSE),
      
      qa_flag   = .data$flag_collocated | .data$flag_spike | .data$flag_extreme,
      qa_reason = dplyr::case_when(
        .data$flag_collocated & .data$flag_spike ~ "Collocated mismatch & isolated spike",
        .data$flag_collocated                    ~ "Collocated mismatch",
        .data$flag_spike                         ~ "Isolated 1-hr spike",
        .data$flag_extreme                       ~ "Extreme value (>= 500)",
        TRUE                                     ~ NA_character_
      ),
      
      qa_subbed = dplyr::coalesce(
        .data$parameter == "PM25_COL" &
          .data$flag_collocated &
          .data$flag_spike &
          !is.na(.data$reg1),
        FALSE
      )
    ) |>
    dplyr::mutate(
      raw_clean = dplyr::if_else(.data$qa_subbed, .data$reg1, .data$raw_clean),
      pm25      = round(.data$raw_clean),
      parameter = dplyr::if_else(.data$qa_subbed, "PM25", .data$parameter)
    ) |>
    dplyr::select(-dplyr::all_of(c("prev_val", "next_val", "neighbor_ref")))
  
  # Re-attach any extra requested fields (e.g., deq_pm25_fields("full"))
  extra_fields <- setdiff(fields, required_fields)
  if (length(extra_fields) > 0L) {
    aq_meta <- aq_raw_hourly |>
      dplyr::mutate(date_mt = lubridate::with_tz(.data$date, "America/Denver")) |>
      dplyr::arrange(is.na(.data$rawvalue)) |>
      dplyr::select(dplyr::all_of(c("date_mt", "parameter")), dplyr::any_of(extra_fields))
    
    aq_meta <- aq_meta[!duplicated(aq_meta[c("date_mt", "parameter")]), , drop = FALSE]
    
    aq_hourly <- aq_hourly |>
      dplyr::left_join(aq_meta, by = c("date_mt", "parameter"), na_matches = "never")
  }
  
  if (output == "hourly") return(aq_hourly)
  
#----------------------------------------------------------------------------#
#----------------------------------------------------------------------------#   
#----------------------------------------------------------------------------#  
#----------------------------------------------------------------------------#
# 4-hr, 8-hr, and 24-hr daily summaries + QA audit trail
#----------------------------------------------------------------------------#
  daily_cols <- c(
    "date_clean", "date_txt", "year_txt",
    "avg_9AM4hr", "avg_9AM8hr", "avg_4PM4hr", "avg_4PM8hr",
    "avg_24hr", "max_24hr", "n_valid",
    "qa_flag", "qa_flagged_hrs", "qa_subbed_hrs", "qa_details",
    "completeness_flag", "completeness_note",
    "instrument_hrs_REG", "instrument_hrs_REG2",
    "qa_note_display"
  )
  
  aq_daily_avg <- aq_hourly |>
    dplyr::group_by(.data$day_mt) |>
    dplyr::summarise(
      day_end_ts   = as.POSIXct(
        sprintf("%s 00:00:00", dplyr::first(.data$day_mt) + lubridate::days(1)),
        tz = "America/Denver"
      ),
      day_complete = now_mt >= (.data$day_end_ts + publish_lag),
      n_expected   = dplyr::n(),
      n_valid      = sum(!is.na(.data$raw_clean)),
      
      avg_9AM4hr = calc_reg_avg(.data$date_mt, .data$raw_clean, dplyr::first(.data$day_mt),  9L, 4L),
      avg_9AM8hr = calc_reg_avg(.data$date_mt, .data$raw_clean, dplyr::first(.data$day_mt),  9L, 8L),
      avg_4PM4hr = calc_reg_avg(.data$date_mt, .data$raw_clean, dplyr::first(.data$day_mt), 16L, 4L),
      avg_4PM8hr = calc_reg_avg(.data$date_mt, .data$raw_clean, dplyr::first(.data$day_mt), 16L, 8L),
      
      n_9am_4 = count_reg_hrs(.data$date_mt, .data$raw_clean, dplyr::first(.data$day_mt),  9L, 4L),
      n_9am_8 = count_reg_hrs(.data$date_mt, .data$raw_clean, dplyr::first(.data$day_mt),  9L, 8L),
      n_4pm_4 = count_reg_hrs(.data$date_mt, .data$raw_clean, dplyr::first(.data$day_mt), 16L, 4L),
      n_4pm_8 = count_reg_hrs(.data$date_mt, .data$raw_clean, dplyr::first(.data$day_mt), 16L, 8L),
      
      avg_24hr = if (.data$day_complete && .data$n_valid >= 18L) round(mean(.data$raw_clean, na.rm = TRUE), 2) else NA_real_,
      max_24hr = if (.data$day_complete && .data$n_valid >= 18L) round(max(.data$raw_clean, na.rm = TRUE), 2)  else NA_real_,
      
      qa_flagged_hrs = sum(.data$qa_flag, na.rm = TRUE),
      qa_subbed_hrs  = sum(.data$qa_subbed, na.rm = TRUE),
      qa_details     = if (any(.data$qa_flag, na.rm = TRUE)) {
        idx <- which(.data$qa_flag)
        paste(
          dplyr::if_else(
            .data$qa_subbed[idx],
            sprintf(
              "%s: %.1f -> %.1f (substituted PM25; %s)",
              .data$hr_txt[idx], .data$reg2[idx], .data$raw_clean[idx], .data$qa_reason[idx]
            ),
            sprintf(
              "%s: %.1f (%s)",
              .data$hr_txt[idx], .data$raw_clean[idx], .data$qa_reason[idx]
            )
          ),
          collapse = "; "
        )
      } else {
        NA_character_
      },
      qa_flag = any(.data$qa_flag, na.rm = TRUE),
      
      completeness_note = build_completeness_note(
        .data$day_complete, .data$n_valid, .data$n_expected,
        .data$n_9am_4, .data$n_9am_8, .data$n_4pm_4, .data$n_4pm_8
      ),
      completeness_flag = !is.na(.data$completeness_note),
      
      instrument_hrs_REG  = sum(.data$parameter == "PM25"     & !is.na(.data$raw_clean), na.rm = TRUE),
      instrument_hrs_REG2 = sum(.data$parameter == "PM25_COL" & !is.na(.data$raw_clean), na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      date_clean      = .data$day_mt,
      date_txt        = sub(" 0", " ", format(.data$day_mt, "%b %d")),
      year_txt        = format(.data$day_mt, "%Y"),
      qa_note_display = dplyr::case_when(
        !is.na(.data$qa_details) & !is.na(.data$completeness_note) ~ paste0("QA/QC: ", .data$qa_details, " | Incomplete: ", .data$completeness_note),
        !is.na(.data$qa_details)                                   ~ paste0("QA/QC: ", .data$qa_details),
        !is.na(.data$completeness_note)                            ~ paste0("Incomplete: ", .data$completeness_note),
        TRUE                                                       ~ NA_character_
      )
    ) |>
    dplyr::select(dplyr::all_of(daily_cols)) |>
    dplyr::arrange(dplyr::desc(.data$date_clean))
  
  if (output == "daily") return(aq_daily_avg)
  
  list(hourly = aq_hourly, daily = aq_daily_avg)
}