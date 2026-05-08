#' Forrest plot
#'
#' @noRd
plot_rr <- function(
    x,
    ...,
    ratio = 0.75,
    colors = c("white", "black"),
    ordering_var = "rr_order",
    x_axis_scale = "log10",
    x_axis_title = "Risk ratio",
    x_axis_limits = NULL,
    x_axis_breaks = NULL,
    x_axis_labels = ggplot2::waiver(),
    title = "",
    base_size = 14,
    point_size = 5,
    point_border_thickness = 1,
    point_border_color = "black",
    vert_line_x_position = 1) {
  n <- nrow(x) + 1L
  if (missing(ratio)) {
    ratio <- 0.09*n + 0.56
  }
  if (missing(base_size)) {
    base_size <- if (n >= 10) 10 else (19 - n)
  }
  if (missing(point_size)) {
    point_size <- if (n >= 10) 3.5 else if (n <= 5) 4.5 else 4
  }
  #x_axis_breaks <- x_axis_breaks %||% abers:::breaks_linear()(c(x$rr_lower, x$rr_upper))
  out <- plot_forest(
    x,
    estimate = "rr",
    lower = "rr_lower",
    upper = "rr_upper",
    y_var = "study",
    point_color_var = "is_meta",
    label_digits = 2L,
    point_color = colors,
    point_border_thickness = point_border_thickness,
    point_border_color = point_border_color,
    aspect_ratio = ratio,
    x_axis_scale = x_axis_scale,
    x_axis_title = x_axis_title,
    x_axis_limits = x_axis_limits,
    x_axis_breaks = x_axis_breaks,
    x_axis_labels = x_axis_labels,
    odd_stripe_colors = rep_len("#22222222", length.out = nrow(x)),
    even_stripe_colors = rep_len("#FFFFFF00", length.out = nrow(x)),
    y_axis_labels = NULL,
    vert_line_x_position = vert_line_x_position,
    ordering_var = ordering_var,
    base_size = base_size,
    point_size = point_size,
    estimate_title = NULL,
    ...
  )
  out <- out + ggplot2::ggtitle(title)
  right_cols <- list(".estimate_label")
  names(right_cols) <- estimate_title %||% "Risk ratio (95% CI)"
  out <- add_column_table(
    out,
    plot_margin = ggplot2::margin(),
    right_cols = right_cols,
    left_cols = list(Study = ".y_var"),
    left_args = list(label_hjust = 1)
  )
  out <- reorder_y_axis(out, dplyr::desc(.x))
  out
}
