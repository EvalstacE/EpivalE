# ==============================================================================
# EpivalE Plotly Theme & General Time-Series Chart Builder
# ==============================================================================

#' Create a Reusable EpivalE Plotly Theme
#'
#' Centralizes typography, canvas colors, gridlines, palette, and toolbar
#' defaults across Shiny dashboards and Quarto reports.
#'
#' @param font_family CSS font stack for all chart text.
#' @param text_color Primary text color (titles, axes, legend).
#' @param muted_color Secondary text color (subtitles, reference labels).
#' @param plot_bg Background color inside the plot axes.
#' @param paper_bg Outer canvas color (`"transparent"` blends into cards/pages).
#' @param grid_color Subtle horizontal gridline color.
#' @param palette Character vector of hex colors (named or unnamed). Defaults to
#'   an accessible Mako-inspired teal, slate, and warm accent sequence.
#' @param base_size Base axis/body font size in px.
#' @param hover_bg Default hover tooltip background color.
#' @param hover_text_color Default hover tooltip text color.
#' @param legend_pos Legend position: `"top"`, `"bottom"`, `"right"`, or `"none"`.
#' @param modebar_remove Plotly modebar buttons to hide by default.
#'
#' @return An object of class `"epi_plot_theme"`.
#' @export
epi_plot_theme <- function(
    font_family      = "Inter, system-ui, -apple-system, 'Segoe UI', sans-serif",
    text_color       = "#1f2933",
    muted_color      = "#52606d",
    plot_bg          = "#ffffff",
    paper_bg         = "transparent",
    grid_color       = "#e6e4dc",
    palette          = c("#2c5f7c", "#45a19e", "#d9822b", "#7a5195", "#c44536", "#5c7a4e"),
    base_size        = 13,
    hover_bg         = "#1f2933",
    hover_text_color = "#ffffff",
    legend_pos       = c("top", "bottom", "right", "none"),
    modebar_remove   = c("lasso2d", "select2d", "autoScale2d")
) {
  legend_pos <- match.arg(legend_pos)
  
  if (!is.character(palette) || length(palette) < 1L) {
    rlang::abort("`palette` must be a non-empty character vector of colors.")
  }
  if (!is.numeric(base_size) || length(base_size) != 1L || base_size <= 0) {
    rlang::abort("`base_size` must be a single positive number.")
  }
  
  structure(
    list(
      font_family      = font_family,
      text_color       = text_color,
      muted_color      = muted_color,
      plot_bg          = plot_bg,
      paper_bg         = paper_bg,
      grid_color       = grid_color,
      palette          = palette,
      base_size        = base_size,
      hover_bg         = hover_bg,
      hover_text_color = hover_text_color,
      legend_pos       = legend_pos,
      modebar_remove   = modebar_remove
    ),
    class = "epi_plot_theme"
  )
}

#' Apply an EpivalE Theme to Any Plotly Object
#'
#' Shared layout and toolbar wrapper used by both generic builders (like
#' [epi_plot_series()]) and domain-specific charts (like `aq_plot_hourly()`).
#'
#' @param p A `plotly` htmlwidget.
#' @param theme An `"epi_plot_theme"` object from [epi_plot_theme()].
#' @param title,subtitle Optional chart title and muted subtitle.
#' @param x_title,y_title Optional axis labels.
#' @param x_type Optional Plotly x-axis type (`"date"`, `"category"`, `"linear"`).
#' @param y_zero Logical; force y-axis to include 0?
#' @param y_tickformat,y_ticksuffix Optional Plotly y-axis tick formatting.
#' @param y_range Optional numeric length-2 vector for fixed y-axis limits.
#' @param show_rangeslider Logical; display bottom x-axis range slider?
#' @param show_legend Logical; display legend?
#' @param margin Optional named list overriding default `list(l, r, b, t)`.
#'
#' @return A themed `plotly` htmlwidget.
#' @export
epi_apply_plotly_theme <- function(
    p,
    theme            = epi_plot_theme(),
    title            = NULL,
    subtitle         = NULL,
    x_title          = NULL,
    y_title          = NULL,
    x_type           = NULL,
    y_zero           = TRUE,
    y_tickformat     = NULL,
    y_ticksuffix     = NULL,
    y_range          = NULL,
    show_rangeslider = FALSE,
    show_legend      = TRUE,
    margin           = NULL
) {
  rlang::check_installed("plotly", reason = "to build interactive charts.")
  
  if (!inherits(p, "plotly")) {
    rlang::abort("`p` must be a plotly object.")
  }
  if (!inherits(theme, "epi_plot_theme")) {
    rlang::abort("`theme` must be created with `epi_plot_theme()`.")
  }
  
  has_title    <- !is.null(title) && nzchar(title)
  has_subtitle <- !is.null(subtitle) && nzchar(subtitle)
  
  title_html <- dplyr::case_when(
    has_title && has_subtitle ~ paste0(
      "<b>", title, "</b><br>",
      "<span style='font-size:", round(theme$base_size * 0.9), "px;color:",
      theme$muted_color, ";'>", subtitle, "</span>"
    ),
    has_title    ~ paste0("<b>", title, "</b>"),
    has_subtitle ~ paste0("<span style='color:", theme$muted_color, ";'>", subtitle, "</span>"),
    TRUE         ~ NA_character_
  )
  
  # Legend configuration
  use_legend <- isTRUE(show_legend) && theme$legend_pos != "none"
  legend_cfg <- dplyr::case_when(
    theme$legend_pos == "top"    ~ "top",
    theme$legend_pos == "bottom" ~ "bottom",
    TRUE                         ~ "right"
  )
  
  legend_list <- switch(
    legend_cfg,
    "top" = list(
      orientation = "h",
      x = 0, xanchor = "left",
      y = 1.02, yanchor = "bottom",
      bgcolor = "rgba(0,0,0,0)"
    ),
    "bottom" = list(
      orientation = "h",
      x = 0, xanchor = "left",
      y = -0.22, yanchor = "top",
      bgcolor = "rgba(0,0,0,0)"
    ),
    "right" = list(
      orientation = "v",
      x = 1.02, xanchor = "left",
      y = 1, yanchor = "top",
      bgcolor = "rgba(0,0,0,0)"
    )
  )
  
  # Give title + top legend breathing room automatically
  top_pad <- 25L +
    (if (has_title) 28L else 0L) +
    (if (has_subtitle) 18L else 0L) +
    (if (use_legend && legend_cfg == "top") 26L else 0L)
  
  bottom_pad <- 55L +
    (if (use_legend && legend_cfg == "bottom") 35L else 0L) +
    (if (isTRUE(show_rangeslider)) 25L else 0L)
  
  default_margin <- list(l = 65, r = 25, b = bottom_pad, t = top_pad)
  final_margin   <- utils::modifyList(default_margin, margin %||% list())
  
  xaxis_list <- list(
    title       = list(
      text     = x_title %||% "",
      font     = list(size = theme$base_size, color = theme$text_color),
      standoff = 12
    ),
    type        = x_type,
    showgrid    = FALSE,
    zeroline    = FALSE,
    linecolor   = theme$grid_color,
    tickfont    = list(size = theme$base_size - 1, color = theme$text_color),
    rangeslider = list(visible = isTRUE(show_rangeslider))
  )
  
  yaxis_list <- list(
    title      = list(
      text     = y_title %||% "",
      font     = list(size = theme$base_size, color = theme$text_color),
      standoff = 12
    ),
    showgrid   = TRUE,
    gridcolor  = theme$grid_color,
    zeroline   = FALSE,
    rangemode  = if (isTRUE(y_zero) && is.null(y_range)) "tozero" else "normal",
    range      = y_range,
    tickformat = y_tickformat,
    ticksuffix = y_ticksuffix,
    tickfont   = list(size = theme$base_size - 1, color = theme$text_color)
  )
  
  p <- plotly::layout(
    p,
    font = list(
      family = theme$font_family,
      size   = theme$base_size,
      color  = theme$text_color
    ),
    title = if (!is.na(title_html)) {
      list(
        text    = title_html,
        x       = 0,
        xanchor = "left",
        font    = list(size = theme$base_size + 3, color = theme$text_color)
      )
    } else {
      NULL
    },
    plot_bgcolor  = theme$plot_bg,
    paper_bgcolor = theme$paper_bg,
    margin        = final_margin,
    showlegend    = use_legend,
    legend        = legend_list,
    hoverlabel    = list(
      bgcolor     = theme$hover_bg,
      bordercolor = theme$hover_bg,
      font        = list(
        family = theme$font_family,
        size   = theme$base_size - 1,
        color  = theme$hover_text_color
      )
    ),
    xaxis = xaxis_list,
    yaxis = yaxis_list
  )
  
  plotly::config(
    p,
    displaylogo            = FALSE,
    responsive             = TRUE,
    modeBarButtonsToRemove = theme$modebar_remove
  )
}

#' Build a Standard Time-Series Line or Bar Chart
#'
#' General-purpose interactive Plotly builder for surveillance counts, sums,
#' proportions, and age-adjusted rates over time. Accepts either bare column
#' names (`x = year`) or character strings (`x = input$x_var`).
#'
#' @param data A data frame or tibble.
#' @param x Time column (Date, POSIXct, numeric year/week, or ordered string/factor).
#' @param y Numeric metric column (count, sum, rate, percent).
#' @param group Optional grouping column for multiple lines or grouped/stacked bars.
#' @param trace Trace type: `"line"` or `"bar"`.
#' @param bar_mode For `trace = "bar"` with `group`: `"group"` or `"stack"`.
#' @param ci_low,ci_high Optional numeric lower/upper confidence interval columns.
#'   Renders as shaded ribbons for `"line"` and error bars for `"bar"`.
#' @param ref_y Optional numeric y-value for a dashed horizontal reference line
#'   (e.g., Healthy People 2030 target or state baseline).
#' @param ref_label Optional label displayed at the right end of `ref_y`.
#' @param ref_color Color for the horizontal reference line.
#' @param value_format Formatting mode for axes and tooltips:
#'   `"number"` (default 0 decimals), `"rate"` (default 1 decimal),
#'   `"percent"` (0–100 scale, appends `%`), or `"proportion"` (0–1 scale, multiplies by 100).
#' @param digits Optional decimal places override.
#' @param unit Optional unit suffix appended in tooltips (e.g., `"cases"`, `"per 100,000"`).
#' @param title,subtitle,x_title,y_title Optional labels.
#' @param show_markers Logical; draw point markers on `"line"` traces?
#' @param connect_gaps Logical; connect across `NA` values in `"line"` traces?
#' @param show_rangeslider Logical; show bottom x-axis range slider?
#' @param y_zero Logical; anchor y-axis at 0?
#' @param theme An `"epi_plot_theme"` object from [epi_plot_theme()].
#'
#' @return A `plotly` htmlwidget.
#' @export
epi_plot_series <- function(
    data,
    x,
    y,
    group            = NULL,
    trace            = c("line", "bar"),
    bar_mode         = c("group", "stack"),
    ci_low           = NULL,
    ci_high          = NULL,
    ref_y            = NULL,
    ref_label        = NULL,
    ref_color        = "#6b7280",
    value_format     = c("number", "rate", "percent", "proportion"),
    digits           = NULL,
    unit             = NULL,
    title            = NULL,
    subtitle         = NULL,
    x_title          = NULL,
    y_title          = NULL,
    show_markers     = TRUE,
    connect_gaps     = FALSE,
    show_rangeslider = FALSE,
    y_zero           = TRUE,
    theme            = epi_plot_theme()
) {
  rlang::check_installed("plotly", reason = "to build interactive charts.")
  
  trace        <- match.arg(trace)
  bar_mode     <- match.arg(bar_mode)
  value_format <- match.arg(value_format)
  
  # --- 1. Fail-fast input validation ---
  if (!is.data.frame(data) || nrow(data) == 0L) {
    rlang::abort("`data` must be a non-empty data frame.")
  }
  if (!inherits(theme, "epi_plot_theme")) {
    rlang::abort("`theme` must be created with `epi_plot_theme()`.")
  }
  
  x_col   <- .resolve_col(data, rlang::enquo(x),       "x",       required = TRUE)
  y_col   <- .resolve_col(data, rlang::enquo(y),       "y",       required = TRUE)
  grp_col <- .resolve_col(data, rlang::enquo(group),   "group",   required = FALSE)
  lo_col  <- .resolve_col(data, rlang::enquo(ci_low),  "ci_low",  required = FALSE)
  hi_col  <- .resolve_col(data, rlang::enquo(ci_high), "ci_high", required = FALSE)
  
  if (!is.numeric(data[[y_col]])) {
    rlang::abort(paste0("Column `", y_col, "` must be numeric."))
  }
  if (xor(is.null(lo_col), is.null(hi_col))) {
    rlang::abort("Both `ci_low` and `ci_high` must be provided together.")
  }
  has_ci <- !is.null(lo_col) && !is.null(hi_col)
  
  if (has_ci && (!is.numeric(data[[lo_col]]) || !is.numeric(data[[hi_col]]))) {
    rlang::abort("`ci_low` and `ci_high` columns must both be numeric.")
  }
  if (has_ci && trace == "bar" && bar_mode == "stack") {
    rlang::abort("Confidence intervals cannot be drawn on stacked bars; use `bar_mode = \"group\"`.")
  }
  
  # Resolve sensible default decimals by metric type
  if (is.null(digits)) {
    digits <- dplyr::case_when(
      value_format == "number" ~ 0L,
      TRUE                     ~ 1L
    )
  }
  
  # --- 2. Standardize working columns ---
  x_raw <- data[[x_col]]
  
  # Lock POSIXct to local wall-clock strings so Plotly.js doesn't shift MT -> UTC
  x_is_posix <- inherits(x_raw, "POSIXct")
  x_is_date  <- inherits(x_raw, "Date")
  x_is_cat   <- is.character(x_raw) || is.factor(x_raw)
  
  x_type <- dplyr::case_when(
    x_is_posix || x_is_date ~ "date",
    x_is_cat                ~ "category",
    TRUE                    ~ NA_character_
  )
  
  work <- tibble::tibble(
    x_sort = x_raw,
    x_plot = if (x_is_posix) format(x_raw, "%Y-%m-%d %H:%M:%S") else x_raw,
    x_lbl  = dplyr::case_when(
      x_is_posix ~ format(x_raw, "%b %d, %Y %I:%M %p"),
      x_is_date  ~ format(x_raw, "%b %d, %Y"),
      TRUE       ~ as.character(x_raw)
    ),
    y_val  = data[[y_col]],
    grp    = if (!is.null(grp_col)) data[[grp_col]] else factor("All"),
    ci_lo  = if (has_ci) data[[lo_col]] else NA_real_,
    ci_hi  = if (has_ci) data[[hi_col]] else NA_real_
  )
  
  # Preserve factor order for groups; sort alphabetically only if character
  grp_levels <- if (is.factor(work$grp)) {
    levels(droplevels(work$grp))
  } else {
    unique(as.character(work$grp[!is.na(work$grp)]))
  }
  work$grp <- as.character(work$grp)
  
  if (!x_is_cat) {
    work <- dplyr::arrange(work, .data$x_sort)
  }
  
  col_map <- .resolve_series_colors(grp_levels, theme$palette)
  
  # Pre-build clean hover text
  y_fmt <- .fmt_epi_values(work$y_val, value_format, digits, unit)
  ci_txt <- if (has_ci) {
    lo_fmt <- .fmt_epi_values(work$ci_lo, value_format, digits, unit = NULL)
    hi_fmt <- .fmt_epi_values(work$ci_hi, value_format, digits, unit = NULL)
    dplyr::if_else(
      !is.na(work$ci_lo) & !is.na(work$ci_hi),
      paste0("<br><b>95% CI:</b> ", lo_fmt, " \u2013 ", hi_fmt),
      ""
    )
  } else {
    rep("", nrow(work))
  }
  
  grp_prefix <- if (!is.null(grp_col)) paste0("<b>", work$grp, "</b><br>") else ""
  y_hover_lbl <- y_title %||% y_col
  
  work$hover_txt <- paste0(
    grp_prefix,
    "<b>", x_title %||% x_col, ":</b> ", work$x_lbl, "<br>",
    "<b>", y_hover_lbl, ":</b> ", y_fmt,
    ci_txt
  )
  
  # --- 3. Assemble Plotly traces ---
  p <- plotly::plot_ly()
  
  for (g in grp_levels) {
    sub_df <- work[work$grp == g, , drop = FALSE]
    if (nrow(sub_df) == 0L) next
    
    g_col <- col_map[[g]]
    
    if (trace == "line") {
      # Shaded CI ribbon sits underneath the main line
      if (has_ci && any(!is.na(sub_df$ci_lo) & !is.na(sub_df$ci_hi))) {
        ribbon_fill <- .hex_to_rgba(g_col, alpha = 0.18)
        
        p <- p %>%
          plotly::add_trace(
            x           = sub_df$x_plot,
            y           = sub_df$ci_hi,
            type        = "scatter",
            mode        = "lines",
            line        = list(width = 0),
            connectgaps = isTRUE(connect_gaps),
            showlegend  = FALSE,
            hoverinfo   = "skip",
            legendgroup = g
          ) %>%
          plotly::add_trace(
            x           = sub_df$x_plot,
            y           = sub_df$ci_lo,
            type        = "scatter",
            mode        = "lines",
            line        = list(width = 0),
            fill        = "tonexty",
            fillcolor   = ribbon_fill,
            connectgaps = isTRUE(connect_gaps),
            showlegend  = FALSE,
            hoverinfo   = "skip",
            legendgroup = g
          )
      }
      
      p <- p %>%
        plotly::add_trace(
          x           = sub_df$x_plot,
          y           = sub_df$y_val,
          name        = g,
          type        = "scatter",
          mode        = if (isTRUE(show_markers)) "lines+markers" else "lines",
          connectgaps = isTRUE(connect_gaps),
          line        = list(color = g_col, width = 2.2),
          marker      = if (isTRUE(show_markers)) {
            list(size = 6.5, color = g_col, line = list(color = "#ffffff", width = 1))
          } else {
            NULL
          },
          text        = sub_df$hover_txt,
          hoverinfo   = "text",
          legendgroup = g
        )
    } else {
      err_cfg <- if (has_ci) {
        list(
          type       = "data",
          symmetric  = FALSE,
          array      = sub_df$ci_hi - sub_df$y_val,
          arrayminus = sub_df$y_val - sub_df$ci_lo,
          color      = theme$text_color,
          thickness  = 1.4,
          width      = 4
        )
      } else {
        NULL
      }
      
      p <- p %>%
        plotly::add_trace(
          x         = sub_df$x_plot,
          y         = sub_df$y_val,
          name      = g,
          type      = "bar",
          marker    = list(color = g_col, line = list(color = "rgba(0,0,0,0)", width = 0)),
          error_y   = err_cfg,
          text      = sub_df$hover_txt,
          hoverinfo = "text"
        )
    }
  }
  
  # --- 4. Optional bar mode & horizontal reference line ---
  shapes <- list()
  annotations <- list()
  
  if (!is.null(ref_y)) {
    if (!is.numeric(ref_y) || length(ref_y) != 1L) {
      rlang::abort("`ref_y` must be a single numeric value.")
    }
    shapes[[1L]] <- list(
      type = "line",
      x0 = 0, x1 = 1, xref = "paper",
      y0 = ref_y, y1 = ref_y, yref = "y",
      line = list(color = ref_color, width = 1.5, dash = "dash")
    )
    if (!is.null(ref_label) && nzchar(ref_label)) {
      annotations[[1L]] <- list(
        x = 0.99, xref = "paper",
        y = ref_y, yref = "y",
        text = ref_label,
        showarrow = FALSE,
        xanchor = "right",
        yanchor = "bottom",
        font = list(size = theme$base_size - 2, color = ref_color)
      )
    }
  }
  
  y_tickformat <- dplyr::case_when(
    value_format == "proportion" ~ paste0(".", digits, "%"),
    value_format == "number"     ~ ",",
    TRUE                         ~ NA_character_
  )
  y_ticksuffix <- dplyr::case_when(
    value_format == "percent" ~ "%",
    TRUE                      ~ NA_character_
  )
  
  p <- epi_apply_plotly_theme(
    p                = p,
    theme            = theme,
    title            = title,
    subtitle         = subtitle,
    x_title          = x_title %||% x_col,
    y_title          = y_title %||% y_col,
    x_type           = if (is.na(x_type)) NULL else x_type,
    y_zero           = y_zero,
    y_tickformat     = if (is.na(y_tickformat)) NULL else y_tickformat,
    y_ticksuffix     = if (is.na(y_ticksuffix)) NULL else y_ticksuffix,
    show_rangeslider = show_rangeslider,
    show_legend      = !is.null(grp_col)
  )
  
  p %>%
    plotly::layout(
      barmode     = if (trace == "bar") bar_mode else NULL,
      shapes      = if (length(shapes) > 0L) shapes else NULL,
      annotations = if (length(annotations) > 0L) annotations else NULL
    )
}


#' Build a Standard Categorical Bar Chart
#'
#' General-purpose interactive Plotly bar builder for comparing counts, sums,
#' proportions, or rates across categories (age groups, communities, conditions,
#' programs). Accepts bare column names or Shiny string inputs.
#'
#' @param data A data frame or tibble.
#' @param category Categorical column displayed along the category axis.
#' @param value Numeric metric column (count, sum, rate, percent).
#' @param group Optional grouping column for grouped or stacked bars.
#' @param orientation Bar direction: `"h"` (horizontal, default—best for long
#'   category labels) or `"v"` (vertical).
#' @param bar_mode For multi-group charts: `"group"` or `"stack"`.
#' @param sort_by_value Logical; sort categories descending by metric value?
#'   Set to `FALSE` to preserve ordered categories (such as age groups).
#' @param ci_low,ci_high Optional numeric lower/upper confidence interval columns
#'   (rendered as error bars; incompatible with `bar_mode = "stack"`).
#' @param ref_val Optional numeric benchmark value for a dashed reference line
#'   (e.g., county average, state rate, or Healthy People target).
#' @param ref_label Optional label displayed next to `ref_val`.
#' @param ref_color Color for the reference line and label.
#' @param show_values Logical; display formatted value labels directly on bars?
#' @param value_format Formatting mode for axes and tooltips:
#'   `"number"` (default 0 decimals), `"rate"` (default 1 decimal),
#'   `"percent"` (0–100 scale), or `"proportion"` (0–1 scale, multiplies by 100).
#' @param digits Optional decimal places override.
#' @param unit Optional unit suffix appended in tooltips (e.g., `"cases"`, `"per 100,000"`).
#' @param title,subtitle,category_title,value_title Optional labels.
#' @param value_zero Logical; anchor the numeric value axis at 0?
#' @param theme An `"epi_plot_theme"` object from [epi_plot_theme()].
#'
#' @return A `plotly` htmlwidget.
#' @export
epi_plot_compare <- function(
    data,
    category,
    value,
    group          = NULL,
    orientation    = c("h", "v"),
    bar_mode       = c("group", "stack"),
    sort_by_value  = TRUE,
    ci_low         = NULL,
    ci_high        = NULL,
    ref_val        = NULL,
    ref_label      = NULL,
    ref_color      = "#6b7280",
    show_values    = FALSE,
    value_format   = c("number", "rate", "percent", "proportion"),
    digits         = NULL,
    unit           = NULL,
    title          = NULL,
    subtitle       = NULL,
    category_title = NULL,
    value_title    = NULL,
    value_zero     = TRUE,
    theme          = epi_plot_theme()
) {
  rlang::check_installed("plotly", reason = "to build interactive charts.")
  
  orientation  <- match.arg(orientation)
  bar_mode     <- match.arg(bar_mode)
  value_format <- match.arg(value_format)
  
  # --- 1. Fail-fast input validation ---
  if (!is.data.frame(data) || nrow(data) == 0L) {
    rlang::abort("`data` must be a non-empty data frame.")
  }
  if (!inherits(theme, "epi_plot_theme")) {
    rlang::abort("`theme` must be created with `epi_plot_theme()`.")
  }
  
  cat_col <- .resolve_col(data, rlang::enquo(category), "category", required = TRUE)
  val_col <- .resolve_col(data, rlang::enquo(value),    "value",    required = TRUE)
  grp_col <- .resolve_col(data, rlang::enquo(group),    "group",    required = FALSE)
  lo_col  <- .resolve_col(data, rlang::enquo(ci_low),   "ci_low",   required = FALSE)
  hi_col  <- .resolve_col(data, rlang::enquo(ci_high),  "ci_high",  required = FALSE)
  
  if (!is.numeric(data[[val_col]])) {
    rlang::abort(paste0("Column `", val_col, "` must be numeric."))
  }
  if (xor(is.null(lo_col), is.null(hi_col))) {
    rlang::abort("Both `ci_low` and `ci_high` must be provided together.")
  }
  has_ci <- !is.null(lo_col) && !is.null(hi_col)
  
  if (has_ci && (!is.numeric(data[[lo_col]]) || !is.numeric(data[[hi_col]]))) {
    rlang::abort("`ci_low` and `ci_high` columns must both be numeric.")
  }
  if (has_ci && !is.null(grp_col) && bar_mode == "stack") {
    rlang::abort("Confidence intervals cannot be drawn on stacked bars; use `bar_mode = \"group\"`.")
  }
  
  if (is.null(digits)) {
    digits <- dplyr::case_when(
      value_format == "number" ~ 0L,
      TRUE                     ~ 1L
    )
  }
  
  # --- 2. Standardize columns & resolve category/group order ---
  cat_raw <- data[[cat_col]]
  grp_raw <- if (!is.null(grp_col)) data[[grp_col]] else factor("All")
  
  work <- tibble::tibble(
    cat   = dplyr::coalesce(as.character(cat_raw), "(Missing)"),
    val   = data[[val_col]],
    grp   = dplyr::coalesce(as.character(grp_raw), "(Missing)"),
    ci_lo = if (has_ci) data[[lo_col]] else NA_real_,
    ci_hi = if (has_ci) data[[hi_col]] else NA_real_
  )
  
  # Preserve factor order when sort_by_value = FALSE; otherwise sort descending
  if (isTRUE(sort_by_value)) {
    cat_order_df <- work %>%
      dplyr::summarise(
        sort_score = if (!is.null(grp_col) && bar_mode == "stack") {
          sum(.data$val, na.rm = TRUE)
        } else {
          suppressWarnings(max(.data$val, na.rm = TRUE))
        },
        .by = "cat"
      ) %>%
      dplyr::mutate(
        sort_score = dplyr::if_else(is.finite(.data$sort_score), .data$sort_score, -Inf)
      ) %>%
      dplyr::arrange(dplyr::desc(.data$sort_score), .data$cat)
    
    cat_levels <- cat_order_df$cat
  } else if (is.factor(cat_raw)) {
    cat_levels <- levels(droplevels(cat_raw))
    if (any(work$cat == "(Missing)")) {
      cat_levels <- c(cat_levels, "(Missing)")
    }
  } else {
    cat_levels <- unique(work$cat)
  }
  
  grp_levels <- if (is.factor(grp_raw)) {
    lvls <- levels(droplevels(grp_raw))
    if (any(work$grp == "(Missing)")) c(lvls, "(Missing)") else lvls
  } else {
    unique(work$grp)
  }
  
  col_map <- .resolve_series_colors(grp_levels, theme$palette)
  
  # Pre-build crisp bar labels and rich tooltips
  val_fmt_full  <- .fmt_epi_values(work$val, value_format, digits, unit)
  val_fmt_short <- dplyr::if_else(
    is.na(work$val),
    "",
    .fmt_epi_values(work$val, value_format, digits, unit = NULL)
  )
  
  ci_txt <- if (has_ci) {
    lo_fmt <- .fmt_epi_values(work$ci_lo, value_format, digits, unit = NULL)
    hi_fmt <- .fmt_epi_values(work$ci_hi, value_format, digits, unit = NULL)
    dplyr::if_else(
      !is.na(work$ci_lo) & !is.na(work$ci_hi),
      paste0("<br><b>95% CI:</b> ", lo_fmt, " \u2013 ", hi_fmt),
      ""
    )
  } else {
    rep("", nrow(work))
  }
  
  grp_prefix <- if (!is.null(grp_col)) paste0("<b>", work$grp, "</b><br>") else ""
  cat_lbl    <- category_title %||% cat_col
  val_lbl    <- value_title %||% val_col
  
  work$bar_txt   <- val_fmt_short
  work$hover_txt <- paste0(
    grp_prefix,
    "<b>", cat_lbl, ":</b> ", work$cat, "<br>",
    "<b>", val_lbl, ":</b> ", val_fmt_full,
    ci_txt
  )
  
  # --- 3. Assemble Plotly bar traces ---
  is_horiz <- orientation == "h"
  txt_pos  <- dplyr::case_when(
    !isTRUE(show_values)                    ~ "none",
    !is.null(grp_col) && bar_mode == "stack" ~ "inside",
    TRUE                                    ~ "outside"
  )
  txt_col <- if (txt_pos == "inside") "#ffffff" else theme$text_color
  
  p <- plotly::plot_ly()
  
  for (g in grp_levels) {
    sub_df <- work[work$grp == g, , drop = FALSE]
    if (nrow(sub_df) == 0L) next
    
    g_col <- col_map[[g]]
    
    err_cfg <- if (has_ci) {
      list(
        type       = "data",
        symmetric  = FALSE,
        array      = sub_df$ci_hi - sub_df$val,
        arrayminus = sub_df$val - sub_df$ci_lo,
        color      = theme$text_color,
        thickness  = 1.4,
        width      = 4
      )
    } else {
      NULL
    }
    
    p <- p %>%
      plotly::add_trace(
        x            = if (is_horiz) sub_df$val else sub_df$cat,
        y            = if (is_horiz) sub_df$cat else sub_df$val,
        name         = g,
        type         = "bar",
        orientation  = orientation,
        marker       = list(color = g_col, line = list(color = "rgba(0,0,0,0)", width = 0)),
        error_x      = if (is_horiz) err_cfg else NULL,
        error_y      = if (!is_horiz) err_cfg else NULL,
        text         = if (isTRUE(show_values)) sub_df$bar_txt else NULL,
        textposition = txt_pos,
        textfont     = list(size = theme$base_size - 2, color = txt_col),
        cliponaxis   = FALSE,
        hovertext    = sub_df$hover_txt,
        hoverinfo    = "text"
      )
  }
  
  # --- 4. Optional benchmark reference line ---
  shapes      <- list()
  annotations <- list()
  
  if (!is.null(ref_val)) {
    if (!is.numeric(ref_val) || length(ref_val) != 1L) {
      rlang::abort("`ref_val` must be a single numeric value.")
    }
    
    if (is_horiz) {
      shapes[[1L]] <- list(
        type = "line",
        x0 = ref_val, x1 = ref_val, xref = "x",
        y0 = 0, y1 = 1, yref = "paper",
        line = list(color = ref_color, width = 1.5, dash = "dash")
      )
      if (!is.null(ref_label) && nzchar(ref_label)) {
        annotations[[1L]] <- list(
          x = ref_val, xref = "x",
          y = 1, yref = "paper",
          text = paste0(" ", ref_label),
          showarrow = FALSE,
          xanchor = "left",
          yanchor = "bottom",
          font = list(size = theme$base_size - 2, color = ref_color)
        )
      }
    } else {
      shapes[[1L]] <- list(
        type = "line",
        x0 = 0, x1 = 1, xref = "paper",
        y0 = ref_val, y1 = ref_val, yref = "y",
        line = list(color = ref_color, width = 1.5, dash = "dash")
      )
      if (!is.null(ref_label) && nzchar(ref_label)) {
        annotations[[1L]] <- list(
          x = 0.99, xref = "paper",
          y = ref_val, yref = "y",
          text = ref_label,
          showarrow = FALSE,
          xanchor = "right",
          yanchor = "bottom",
          font = list(size = theme$base_size - 2, color = ref_color)
        )
      }
    }
  }
  
  # --- 5. Apply shared theme + orientation-aware axis rules ---
  val_tickformat <- dplyr::case_when(
    value_format == "proportion" ~ paste0(".", digits, "%"),
    value_format == "number"     ~ ",",
    TRUE                         ~ NA_character_
  )
  val_ticksuffix <- dplyr::case_when(
    value_format == "percent" ~ "%",
    TRUE                      ~ NA_character_
  )
  
  # Give outside bar labels a little right-hand breathing room on horizontal bars
  extra_margin <- if (is_horiz && isTRUE(show_values) && txt_pos == "outside") {
    list(r = 55)
  } else {
    NULL
  }
  
  p <- epi_apply_plotly_theme(
    p            = p,
    theme        = theme,
    title        = title,
    subtitle     = subtitle,
    x_title      = if (is_horiz) val_lbl else cat_lbl,
    y_title      = if (is_horiz) cat_lbl else val_lbl,
    y_zero       = if (!is_horiz) value_zero else FALSE,
    y_tickformat = if (!is_horiz && !is.na(val_tickformat)) val_tickformat else NULL,
    y_ticksuffix = if (!is_horiz && !is.na(val_ticksuffix)) val_ticksuffix else NULL,
    show_legend  = !is.null(grp_col),
    margin       = extra_margin
  )
  
  # Plotly draws horizontal y-categories bottom-to-top, so reverse to put #1 at top
  if (is_horiz) {
    p <- p %>%
      plotly::layout(
        barmode = if (!is.null(grp_col)) bar_mode else NULL,
        xaxis = list(
          showgrid   = TRUE,
          gridcolor  = theme$grid_color,
          rangemode  = if (isTRUE(value_zero)) "tozero" else "normal",
          tickformat = if (is.na(val_tickformat)) NULL else val_tickformat,
          ticksuffix = if (is.na(val_ticksuffix)) NULL else val_ticksuffix
        ),
        yaxis = list(
          type          = "category",
          categoryorder = "array",
          categoryarray = rev(cat_levels),
          showgrid      = FALSE,
          rangemode     = "normal",
          automargin    = TRUE
        ),
        shapes      = if (length(shapes) > 0L) shapes else NULL,
        annotations = if (length(annotations) > 0L) annotations else NULL
      )
  } else {
    p <- p %>%
      plotly::layout(
        barmode = if (!is.null(grp_col)) bar_mode else NULL,
        xaxis = list(
          type          = "category",
          categoryorder = "array",
          categoryarray = cat_levels,
          automargin    = TRUE
        ),
        shapes      = if (length(shapes) > 0L) shapes else NULL,
        annotations = if (length(annotations) > 0L) annotations else NULL
      )
  }
  
  p
}



# ==============================================================================
# Internal Helpers (not exported)
# ==============================================================================

#' Resolve bare column name, string literal, or Shiny string variable
#' @noRd
.resolve_col <- function(df, quo, arg_name, required = TRUE) {
  if (rlang::quo_is_null(quo)) {
    if (required) {
      rlang::abort(paste0("`", arg_name, "` must be provided."))
    }
    return(NULL)
  }
  
  expr <- rlang::quo_get_expr(quo)
  
  col_name <- if (rlang::is_string(expr)) {
    expr
  } else if (rlang::is_symbol(expr)) {
    sym_str <- rlang::as_string(expr)
    if (sym_str %in% names(df)) {
      sym_str
    } else {
      val <- tryCatch(rlang::eval_tidy(quo), error = function(e) NULL)
      if (rlang::is_string(val)) val else sym_str
    }
  } else {
    val <- rlang::eval_tidy(quo)
    if (!rlang::is_string(val)) {
      rlang::abort(paste0("`", arg_name, "` must be a bare column name or a single character string."))
    }
    val
  }
  
  if (!col_name %in% names(df)) {
    rlang::abort(paste0("Column `", col_name, "` (supplied to `", arg_name, "`) not found in `data`."))
  }
  
  col_name
}

#' Format numeric metric values consistently for tooltips
#' @noRd
.fmt_epi_values <- function(x, value_format, digits, unit = NULL) {
  scaled <- if (value_format == "proportion") x * 100 else x
  num_str <- formatC(scaled, format = "f", digits = digits, big.mark = ",")
  
  suffix <- dplyr::case_when(
    value_format %in% c("percent", "proportion") ~ "%",
    !is.null(unit) && nzchar(unit)               ~ paste0(" ", unit),
    TRUE                                         ~ ""
  )
  
  dplyr::if_else(is.na(x), "N/A", paste0(num_str, suffix))
}

#' Map group levels to palette colors (honors named palettes, recycles safely)
#' @noRd
.resolve_series_colors <- function(groups, palette) {
  pal_names <- names(palette)
  
  if (!is.null(pal_names) && all(groups %in% pal_names)) {
    return(stats::setNames(unname(palette[groups]), groups))
  }
  
  cols <- rep_len(unname(palette), length(groups))
  stats::setNames(cols, groups)
}

#' Convert a hex color string to rgba() for translucent CI ribbons
#' @noRd
.hex_to_rgba <- function(hex, alpha = 0.2) {
  rgb_mat <- grDevices::col2rgb(hex)
  sprintf("rgba(%d, %d, %d, %.2f)", rgb_mat[1, 1], rgb_mat[2, 1], rgb_mat[3, 1], alpha)
}