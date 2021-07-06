#' NOTTSHC CDU data science team ggplot2 theme
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
#' https://github.com/crsh/papaja, see https://github.com/crsh/papaja/blob/master/R/theme_apa.R.
#'
#' @export
#'
theme_nottshc <- function(base_size = 12, base_family = "", box = FALSE) {

  adapted_theme <- ggplot2::theme_bw(base_size, base_family) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(size = ggplot2::rel(1.1),
                                         margin = ggplot2::margin(0, 0, ggplot2::rel(14), 0)))

  if(box) {

    adapted_theme <- adapted_theme + ggplot2::theme(panel.border = ggplot2::element_rect(color = "black"))

  } else {

    adapted_theme <- adapted_theme + ggplot2::theme(panel.border = ggplot2::element_blank())

  }

  adapted_theme
}


#' Convert ggplot object into plotly
#'
#' @description There are some problems with x and y axis labels when converting
#' ggplot2 to plotly, especially when using facets. This fun has a workaround that is not perfect.
#'
#'
#' @param ggplot ggplot2 object
#' @param xtitle String, x lab title
#' @param ytitle String, y lab title
#' @param aes_txt_tooltip Logical, specifying whether or not to use tooltip
#' @param display_mode_bar Logical, specifying whether to show the annoying
#' mode bar from plotly, FALSE by default!
#'
#' @return
#' @export
#'
#' @examples
ggplotly_nottshc <- function(ggplot, xtitle = NULL, ytitle = NULL,
                             aes_txt_tooltip = TRUE,
                             display_mode_bar = FALSE) {

  if (aes_txt_tooltip == TRUE) {
    ggplotly_nottshc <- ggplot %>%
      plotly::ggplotly(tooltip = "text")
  }

  if (display_mode_bar == FALSE) {
    ggplotly_nottshc <- ggplotly_nottshc %>%
      plotly::config(displayModeBar = FALSE)
  }


  x <- list(title = xtitle)
  y <- list(title = ytitle)

  ggplotly_nottshc <- ggplotly_nottshc %>%
    plotly::layout(xaxis = x, yaxis = y)

  return(ggplotly_nottshc)

}
