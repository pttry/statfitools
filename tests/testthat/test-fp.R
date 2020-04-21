test_that("fp works", {
  expect_equal(fp(cp = c(1, 2, 3), pp = c(NA, NA, NA), time = c(2014,2015,2016), year = 2015),
               c(NA_real_, NA_real_, NA_real_))
  expect_equal(fp(cp = c(1, 2, 3), pp = c(1, 2, 4), time = c(2014,2015,2016), year = 2015),
               c(1,2,4))
  expect_equal(fp(cp = c(1, 2, 3), pp = c(NA, 2, 4), time = c(2014,2015,2016), year = 2015),
               c(1,2,4))
  expect_equal(fp(cp = c(NA, 2, 3), pp = c(NA, NA, 4), time = c(2014,2015,2016), year = 2015),
               c(NA_real_,2,4))

})
