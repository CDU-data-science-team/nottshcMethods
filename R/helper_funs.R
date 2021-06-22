#' Categorise continuous age into age groups
#'
#' @param var Variable name
#' @param min Numeric, minimum age
#' @param max Numeric, maximum age
#' @param by Numeric, increment of the age categories
#' @param method
#'
#' @return
#' @export
as_age_groups <- function(var, min = 0, max = 100, by = 10,
                          grouping_method = c("user_defined", "ons_1", "ons_2", "nhs_survey")) {

  grouping_method <- match.arg(grouping_method)

  if (grouping_method != "user_defined") {

    message(paste0("The age groupings are based on guidelines defined in: '", grouping_method, "'."))
    message(paste0("User defined arguments (`min`, `max`, and `by`) will be overwritten."))

  }

  if (grouping_method == "user_defined") {

    labs <- c(paste(seq(min, max - by, by = by),
                    seq(min + by - 1, max - 1, by = by),
                    sep = "-"), paste(max, "+", sep = ""))

    cut({{var}},
        breaks = c(seq(min, max, by = by), Inf),
        labels = labs,
        right = FALSE)

  }



}
