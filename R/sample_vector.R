#' CDU data science team ggplot2 theme
#'
#' A ggplot2 theme with a white panel background, no grid lines,
#' large axis and legend titles,
#' and increased text padding for better readability.
#'
#' @param values Vector- the thing you want to sample from
#' @param weights vector of integers the same length as values- the proportions
#' with which you want to sample
#' @param length integer- how long you want the return value to be- e.g. the
#' number of rows in a dataframe
#'
#' @export
sample_vector <- function(values, weights, length){

  sample(values, length, replace = TRUE, prob = weights / sum(weights))
}

sample_vector(values = c(NA, "Male", "Female", "Other"),
              weights = c(10, 50, 50, 2),
              length = 100)
