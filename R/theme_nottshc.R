#' CDU data science team ggplot2 theme
#'
#' A ggplot2 theme with a white panel background, no grid lines,
#' large axis and legend titles,
#' and increased text padding for better readability.
#'
#' @param base_size Numeric. Base font size; other font sizes and margins
#' are adjusted relative to this.
#' @param base_family Character. Base font family.
#' @param box Logical. Indicates whether to draw a box around the plot.
#'
#' @details Some of this theme is borrowed with thanks from
#' https://github.com/crsh/papaja/blob/master/R/theme_apa.R
#'
#' @export
#'

theme_nottshc <- function(base_size = 12, base_family = "", box = FALSE) {
  adapted_theme <- ggplot2::theme_bw(base_size, base_family) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(size = ggplot2::rel(1.1),
                                         margin = ggplot2::margin(0, 0, ggplot2::rel(14), 0))

    )

  if(box) {
    adapted_theme <- adapted_theme + ggplot2::theme(panel.border = ggplot2::element_rect(color = "black"))
  } else {
    adapted_theme <- adapted_theme + ggplot2::theme(panel.border = ggplot2::element_blank())
  }

  adapted_theme
}
