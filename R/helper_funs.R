#' Categorise continuous age into age groups
#'
#' @param var Name of variable or vector
#' @param min Numeric, specifying the minimum age of the first age group.
#' Age values lower than this will be returned as missing values (NA)
#' @param max Numeric, specifying the upper end of the last age group
#' @param by Numeric, increment of the age categories
#' @param method String, specyfing the method to be used for grouping age into categories.
#' Details about the different methods are available here:
#' "user_defined", "ons_1", "ons_2", "nhs_survey" ... TODO
#'
#'
#' @return
#' @export
#' @examples
#' # Example using a vector:
#' set.seed(123)
#' age <- sample(1:100, 100, replace = T)
#' as_age_groups(age)
#'
#' # Example using a data frame
#' tibble::tibble(age = sample(1:115, 100, replace = T)) %>%
#'   dplyr::mutate(age_groups = as_age_groups(age, min = 10, max = 100))
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
