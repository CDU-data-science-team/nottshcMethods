#' Function to give the 'yyyy' string to a date column (of formats date, int and character)
#'
#' @param data Dataset
#' @param date_col Date, the date column to find the latest/earliest. If this
#' an integer (sk synthetic key) converts to date format
#' @param type String, latest gives the most recent year in the column whilst earliest gives the
#' first
#'
#' @return
#' @export
#'
#' @examples
#'test <- data.frame(
#'  date = sample(seq(as.Date('2007/01/01'), as.Date('2021/01/01'), by = "day"), 12)) %>%
#'  dplyr::mutate(date_sk = as.integer(format(as.Date(date), "%Y%m%d"))) %>%
#'  dplyr::mutate(charc_date = as.character(date))
#'
#'year_date(test, date, type = "latest")
#'year_date(test, date, type = "earliest")
#'
#'year_date(test, date_sk, type = "latest")
#'year_date(test, date_sk, type = "earliest")
#'
#'year_date(test, charc_date, type = "latest")
#'year_date(test, charc_date, type = "earliest")
#'

year_date <- function(data,
                      date_col,
                      type = c("earliest", "latest")) {

  # Check function arguments
  type <- match.arg(type)

  df <- data %>%
    dplyr::select({{ date_col }}) %>%
    unique()

  if(type == "latest"){

    df_return <- df %>%
      dplyr::filter({{ date_col }} == max({{ date_col }}))

    df_return

  }

  if(type == "earliest"){

    df_return <- df %>%
      dplyr::filter({{ date_col }} == min({{ date_col }}))

    df_return
  }

  # Return yyyy string
  df_return %>%
    dplyr::mutate(year = lubridate::ymd({{ date_col }})) %>%
    dplyr::summarise(year = substring(year, 1, 4)) %>%
    unique()
}

