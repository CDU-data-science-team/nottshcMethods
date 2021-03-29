
test_that("Hash works", {
  expect_equal("28febb13cf0fe5fb4cf567aa0915bccf", hash("Secret stuff"))
})

test_that("Skip NA with hash", {
  expect_equal(c("28febb13cf0fe5fb4cf567aa0915bccf", NA,
                 "a97295876bee21de98ddc134ed0c7196"),
               hash(c("Secret stuff", NA, "More secret stuff")))
})

test_that("Correct number of characaters", {
  expect_equal("28febb13", substr(hash("Secret stuff"), 1, 8))
})
