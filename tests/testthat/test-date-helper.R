# Ensure only 4 characters for date ---------------------------------------

testthat::test_that("Earliest year output is of 4 characters", {

  test <- data.frame(
    date = sample(seq(as.Date('2007/01/01'), as.Date('2021/01/01'), by = "day"), 12))

  df <- year_date(test, date, type = "earliest")

  testthat::expect_equal(stringi::stri_length(df$year), 4)

})


testthat::test_that("Latest year output is of 4 characters", {

  test <- data.frame(
    date = sample(seq(as.Date('2007/01/01'), as.Date('2021/01/01'), by = "day"), 12))

  df <- year_date(test, date, type = "latest")

  testthat::expect_equal(stringi::stri_length(df$year), 4)

})


# Fixed start and end date tests ------------------------------------------

testthat::test_that("Earliest starts equals 2007", {

  test <- data.frame(
    date = seq(as.Date('2007/01/01'), as.Date('2021/01/01'), by = "year"), 12)

  df <- year_date(test, date, type = "earliest")

  testthat::expect_equal(df$year, "2007")

})


testthat::test_that("Latest year latest equals 2021", {

  test <- data.frame(
    date = seq(as.Date('2007/01/01'), as.Date('2021/01/01'), by = "year"), 12)

  df <- year_date(test, date, type = "latest")

  testthat::expect_equal(df$year, "2021")

})

# Fixed start and end date tests for integers------------------------------------------

testthat::test_that("Earliest starts equals 2007 if date is an integer", {

  test <- data.frame(
    date = seq(as.Date('2007/01/01'), as.Date('2021/01/01'), by = "year"), 12) %>%
    dplyr::mutate(date_sk = as.integer(format(as.Date(date), "%Y%m%d")))

  df <- year_date(test, date, type = "earliest")

  testthat::expect_equal(df$year, "2007")

})


testthat::test_that("Latest year latest equals 2021 if date is an integer", {

  test <- data.frame(
    date = seq(as.Date('2007/01/01'), as.Date('2021/01/01'), by = "year"), 12) %>%
    dplyr::mutate(date_sk = as.integer(format(as.Date(date), "%Y%m%d")))

  df <- year_date(test, date, type = "latest")

  testthat::expect_equal(df$year, "2021")

})

# Fixed start and end date tests for characters------------------------------------------

testthat::test_that("Earliest starts equals 2007 if date is an characters", {

  test <- data.frame(
    date = seq(as.Date('2007/01/01'), as.Date('2021/01/01'), by = "year"), 12) %>%
    dplyr::mutate(date = as.character(date))

  df <- year_date(test, date, type = "earliest")

  testthat::expect_equal(df$year, "2007")

})


testthat::test_that("Latest year latest equals 2021 if date is an characters", {

  test <- data.frame(
    date = seq(as.Date('2007/01/01'), as.Date('2021/01/01'), by = "year"), 12) %>%
    dplyr::mutate(charc_date = as.character(date))

  df <- year_date(test, date, type = "latest")

  testthat::expect_equal(df$year, "2021")

})
