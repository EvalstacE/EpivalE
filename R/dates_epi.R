#' Create Fiscal Year and Fiscal Quarter Columns
#'
#' @description This function takes a dataframe and a date column to append 
#' fiscal quarter (Q1-Q4) and fiscal year based on a July 1st start date.
#'
#' @param df A data frame or tibble.
#' @param date_col The unquoted name of the date column.
#'
#' @return A modified data frame with additional fiscal columns.
#' @export

create_fiscal_yr_Q <- function(df, date_col) {
  df %>%
    dplyr::mutate(
      month_abr =  lubridate::month({{ date_col }}, label = TRUE, abbr = TRUE),
      cal_year  =  lubridate::year({{ date_col }}),
      fiscal_Q  =  dplyr::case_when(
        month_abr %in% c("Jul", "Aug", "Sep") ~ "Q1",
        month_abr %in% c("Oct", "Nov", "Dec") ~ "Q2",
        month_abr %in% c("Jan", "Feb", "Mar") ~ "Q3",
        month_abr %in% c("Apr", "May", "Jun") ~ "Q4",
        TRUE ~ NA_character_
      ),
      fiscal_year = dplyr::case_when(
        # Standard LHD fiscal year logic: July starts the next fiscal year
        # e.g., July 2023 is FY 2024
        fiscal_Q %in% c("Q1", "Q2") ~ cal_year + 1,
        fiscal_Q %in% c("Q3", "Q4") ~ cal_year,
        TRUE ~ NA_integer_
      )
    )
}

#' Create Calendar Year and Calendar Quarter Columns
#'
#' @description Appends calendar quarter and human-readable quarter labels to a dataframe.
#'
#' @param df A data frame or tibble.
#' @param date_col The unquoted name of the date column.
#'
#' @return A modified data frame with additional calendar quarter columns.
#' @export

create_cal_yr_Q <- function(df, date_col) {
  df %>%
    dplyr::mutate(
      month_abr = lubridate::month({{ date_col }}, label = TRUE, abbr = TRUE),
      month_num = lubridate::month({{ date_col }}, label = FALSE, abbr = FALSE),
      cal_year  = lubridate::year({{ date_col }}),
      cal_Q = dplyr::case_when(
        month_abr %in% c("Jan", "Feb", "Mar") ~ "Q1",
        month_abr %in% c("Apr", "May", "Jun") ~ "Q2",
        month_abr %in% c("Jul", "Aug", "Sep") ~ "Q3",
        month_abr %in% c("Oct", "Nov", "Dec") ~ "Q4",
        TRUE ~ NA_character_
      ),
      cal_Q_lbl = dplyr::case_when(
        cal_Q == "Q1" ~ "Jan-Mar",
        cal_Q == "Q2" ~ "Apr-Jun",
        cal_Q == "Q3" ~ "Jul-Sep",
        cal_Q == "Q4" ~ "Oct-Dec",
        TRUE ~ NA_character_
      )
    )
}