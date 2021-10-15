#' Categorise continuous age into age groups
#'
#' @param var Name of variable or vector
#' @param min Numeric, specifying the minimum age of the first age group.
#' Age values lower than this will be returned as missing values (NA)
#' @param max Numeric, specifying the upper end of the last age group
#' @param by Numeric, increment of the age categories
#' @param grouping_method String, specifying the method to be used for grouping age into categories.
#' Details about the different methods are available here:
#' "user_defined", "ons_1", "ons_2", "nhs_survey" ... TODO
#'
#'
#' @export
#' @examples
#' # Example using a vector:
#' set.seed(123)
#' age <- sample(1:100, 100, replace = TRUE)
#' as_age_groups(age)
#'
#' # Example using a data frame
#' tibble::tibble(age = sample(1:115, 100, replace = TRUE)) %>%
#'   dplyr::mutate(age_groups = as_age_groups(age, min = 10, max = 100))
as_age_groups <- function(var, min = 0, max = 120, by = 10,
                          grouping_method = c("user_defined",
                                              "ons_1",
                                              "ons_2",
                                              "nhs_survey")) {

  grouping_method <- match.arg(grouping_method)

  if (grouping_method != "user_defined") {

    message(paste0("The age groupings are based on guidelines defined in: '",
                   grouping_method, "'."))
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



#' Calculate frequencies of groups per month
#'
#' @param data Data
#' @param by Grouping variables used in group_by
#' @param date_var_name String specifying date variable, needs to be class date
#' @param .drop_latest_month Logical, specifying whether or not to drop the most
#' recent month
#' @param .calc_year_month_day_vars Logical, specifying whether to calculate
#' separate year, month, and day variable
#'
#' @export
calc_monthly_freq <- function(data,
                              by,
                              date_var_name,
                              .drop_latest_month = TRUE,
                              .calc_year_month_day_vars = TRUE) {

  data <- data %>%
    dplyr::mutate(floor_date_m = lubridate::floor_date( {{date_var_name}} ,
                                                        unit = "month"),
                  floor_date_m = lubridate::as_date(.data$floor_date_m))

  if (.drop_latest_month) {

    max_date_m <- max(data$floor_date_m, na.rm = TRUE)

    data <- data %>%
      dplyr::filter(.data$floor_date_m != max_date_m)

  }

  data <- data %>%
    dplyr::group_by(dplyr::across(c( {{by}}, .data$floor_date_m))) %>%
    dplyr::summarise(n = dplyr::n())

  if (.calc_year_month_day_vars) {

    data <- data %>%
      dplyr::mutate(year = lubridate::year(.data$floor_date_m),
                    month = lubridate::month(.data$floor_date_m,
                                             label = TRUE,
                                             abbr = TRUE),
                    day = lubridate::wday(.data$floor_date_m,
                                          label = TRUE,
                                          abbr = TRUE)) %>%
      dplyr::relocate(.data$floor_date_m) %>%
      dplyr::relocate(.data$year,
                      .data$month,
                      .data$day,
                      .after = .data$floor_date_m)

  }

  data %>%
    dplyr::ungroup()

}
