#' Categorise continuous age into age groups
#'
#' @param var Variable name
#' @param min Numeric, minimum age
#' @param max Numeric, maximum age
#' @param by Numeric, increment of the age categories
#'
#' @return
#' @export
as_age_groups <- function(var, min = 0, max = 100, by = 10) {

  labs <- c(paste(seq(min, max - by, by = by),
                  seq(min + by - 1, max - 1, by = by),
                  sep = "-"), paste(max, "+", sep = ""))

  cut({{var}},
      breaks = c(seq(min, max, by = by), Inf),
      labels = labs,
      right = FALSE)

}
