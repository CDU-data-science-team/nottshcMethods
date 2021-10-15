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


#' NOTTSHC CDU data science team ggplot2 theme details
#'
#'  A ggplot2 theme with extended details
#'
#' @description Inspired from the chart layout
#' used by TraffordDataLab https://github.com/traffordDataLab/assets/tree/master/theme/ggplot2
#'
#' @param left_label Logical. Indicates whether to include the x axis labels and
#' ticks which appear on the left of the chart. Default is to include and useful to exclude
#' on charts specifically needed to be 'clean' for example Smoking Prevalence.
#'
#' @section Last updated by:
#' Zoë Turner
#' @section Last updated date:
#' 2021-04-01
#'
#' @return
#' @export
#' @examples
#'# load some data
#'df <- data.frame(
#'  religion = c("Christian", "Buddhist", "Hindu", "Jewish", "Muslim", "Sikh",
#'  "Other Religion", "No Religion", "Not Stated"),
#'  count = c(143639, 768, 2271, 2413, 12994, 1652, 566, 47968, 14307)
#')
#'
#'# create a ggplot object
#'plot <- df %>%
#'  dplyr::arrange(count) %>%
#'  dplyr::mutate(religion = factor(religion,
#'                                  levels = religion)) %>%
#'  ggplot2::ggplot(ggplot2::aes(religion, count)) +
#'  ggplot2::geom_col(fill = "#fc6721",
#'                    alpha = 0.8,
#'                    show.legend = FALSE) +
#'  ggplot2::coord_flip() +
#'  ggplot2::labs(x = NULL, y = "Residents",
#'                title = "This is the title of the chart",
#'                subtitle = "This is the subtitle",
#'                caption = "Source: Table KS209EW, Census 2011  |  @DataScienceNott")
#'
#'# style with the ds_theme putting y axis label and ticks back
#'plot + nottshcMethods::ds_theme(left_label = FALSE)

ds_theme <- function(left_label = TRUE){

  theme <- ggplot2::theme(plot.margin = ggplot2::unit(rep(0.5, 4), "cm"),
                          panel.grid.major = ggplot2::element_blank(),
                          panel.grid.minor = ggplot2::element_blank(),
                          plot.title.position = "plot",
                          plot.title = ggplot2::element_text(size = 14,
                                                             face = "bold"),
                          plot.subtitle = ggtext::element_markdown(size = 12,
                                                                   margin = ggplot2::margin(b = 20)),
                          plot.caption = ggplot2::element_text(colour = "grey60",
                                                               margin = ggplot2::margin(t = 20, b = -10)),
                          legend.position = "bottom",
                          axis.ticks.x = ggplot2::element_line(colour = "#333333",
                                                               size = 0.5),
                          axis.line.x = ggplot2::element_line(colour = 'grey',
                                                              size = 0.5))

  if(left_label) {

    theme <- theme + ggplot2::theme(axis.title.y = ggplot2::element_blank(),
                                    axis.text.y = ggplot2::element_blank(),
                                    axis.ticks.y = ggplot2::element_blank())
  theme
  }
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
#' @export
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



