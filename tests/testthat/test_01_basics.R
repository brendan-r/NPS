
# Test the Net Promoter Categories ----------------------------------------

options(nps.100 = FALSE)

test_that("Warnings are given about strange-looking input data", {

  # This should be fine
  expect_silent(npc(0:10))

  # This should produce a warning about non-integer input
  expect_warning(npc(c(1/3, 0:10)), "integer")

  # This should produce a warning that values outside the range have been
  # coerced to NA
  expect_warning(npc(-1:10), "outside.*range")
  expect_warning(npc(0:11),  "outside.*range")

})


test_that("Errors are thrown on bad input data", {

  # It would be real bad if the answer was 0
  expect_error(npc(c()))

  # Iterative tests aren't supported in the two sample case at the moment
  expect_error(nps_test_(1:3, 1:3, test = "iterative"))

})

test_that("Output data types & handling of missing data as expected", {


})


test_that("Valid sig tests return S3 objects", {

})


test_that("Statistical functions give expected answers", {

  # nps_
  expect_equal(nps_(1:3), 1 / 3)
  expect_equal(nps_(c(1, 0, 0)), -1)
  expect_equal(nps_(c(0, 0, 1)), 1)
  expect_equal(nps_(c(0, 1, 0)), 0)

  # nps
  expect_equal(nps(c(0, 7, 7, 9, 9, 9)), 1 / 3)
  expect_equal(nps(0), -1)
  expect_equal(nps(10), 1)
  expect_equal(nps(7), 0)

  # nps_var_
  expect_equal(nps_var_(1:3), 5 / 9)
  expect_equal(nps_var_(c(1, 0, 0)), 0)
  expect_equal(nps_var_(c(0, 1, 0)), 0)
  expect_equal(nps_var_(c(0, 0, 1)), 0)
  expect_equal(nps_var_(c(1, 0, 1)), 1)
  expect_equal(nps_var_(c(0, 1, 1)), 0.25)
  expect_equal(nps_var_(c(1, 1, 1)), 2 / 3)

  # nps_var
  expect_equal(nps_var(c(0, 7, 7, 9, 9, 9)), 5 / 9)
  expect_equal(nps_var(1), 0)
  expect_equal(nps_var(7), 0)
  expect_equal(nps_var(9), 0)
  expect_equal(nps_var(c(1, 9)), 1)
  expect_equal(nps_var(c(7, 9)), 0.25)
  expect_equal(nps_var(c(0, 7, 9)), 2 / 3)

  # nps_se_
  expect_equal(nps_se_(1:3), 0.304, tolerance = 0.001)
  expect_equal(nps_se_(c(1, 0, 0)), 0)
  expect_equal(nps_se_(c(0, 1, 0)), 0)
  expect_equal(nps_se_(c(0, 0, 1)), 0)
  expect_equal(nps_se_(c(1, 0, 1)), 0.707, tolerance = 0.001)
  expect_equal(nps_se_(c(0, 1, 1)), 0.354, tolerance = 0.001)
  expect_equal(nps_se_(c(1, 1, 1)), 0.471, tolerance = 0.001)

  # nps_se
  expect_equal(nps_se(c(0, 7, 7, 9, 9, 9)), 0.304, tolerance = 0.001)
  expect_equal(nps_se(1), 0)
  expect_equal(nps_se(7), 0)
  expect_equal(nps_se(9), 0)
  expect_equal(nps_se(c(1, 9)), 0.707, tolerance = 0.001)
  expect_equal(nps_se(c(7, 9)), 0.354, tolerance = 0.001)
  expect_equal(nps_se(c(0, 7, 9)), 0.471, tolerance = 0.001)

})


test_that("Statistical functions agree with themselves", {

  # nps and nps_
  expect_equal(nps_(c(1, 2, 3)), nps(c(0, 7, 7, 9, 9, 9)))
  expect_equal(nps_(c(1, 0, 0)), nps(0))
  expect_equal(nps_(c(0, 0, 1)), nps(10))
  expect_equal(nps_(c(0, 1, 0)), nps(7))

  # nps_var and nps_var_
  expect_equal(nps_var_(1:3), nps_var(c(0, 7, 7, 9, 9, 9)))
  expect_equal(nps_var_(c(1, 0, 0)), nps_var(1))
  expect_equal(nps_var_(c(0, 1, 0)), nps_var(7))
  expect_equal(nps_var_(c(0, 0, 1)), nps_var(9))
  expect_equal(nps_var_(c(1, 0, 1)), nps_var(c(1, 9)))
  expect_equal(nps_var_(c(0, 1, 1)), nps_var(c(7, 9)))
  expect_equal(nps_var_(c(1, 1, 1)), nps_var(c(0, 7, 9)))

})


test_that("Formatting options work", {

  options(nps.100 = TRUE)

  # nps_
  expect_equal(nps_(1:3), 1 / 3 * 100)
  expect_equal(nps_(c(1, 0, 0)) , -1 * 100)
  expect_equal(nps_(c(0, 0, 1)), 1 * 100)
  expect_equal(nps_(c(0, 1, 0)), 0 * 100)

  # nps
  expect_equal(nps(c(0, 7, 7, 9, 9, 9)), 1 / 3 * 100)
  expect_equal(nps(0), -1 * 100)
  expect_equal(nps(10), 1 * 100)
  expect_equal(nps(7), 0 * 100)

  # nps_var_
  expect_equal(nps_var_(1:3), 5 / 9 * 100)
  expect_equal(nps_var_(c(1, 0, 0)), 0 * 100)
  expect_equal(nps_var_(c(0, 1, 0)), 0 * 100)
  expect_equal(nps_var_(c(0, 0, 1)), 0 * 100)
  expect_equal(nps_var_(c(1, 0, 1)), 1 * 100)
  expect_equal(nps_var_(c(0, 1, 1)), 0.25 * 100)
  expect_equal(nps_var_(c(1, 1, 1)), 2 / 3 * 100)

  # nps_var
  expect_equal(nps_var(c(0, 7, 7, 9, 9, 9)), 5 / 9 * 100)
  expect_equal(nps_var(1), 0 * 100)
  expect_equal(nps_var(7), 0 * 100)
  expect_equal(nps_var(9), 0 * 100)
  expect_equal(nps_var(c(1, 9)), 1 * 100)
  expect_equal(nps_var(c(7, 9)), 0.25 * 100)
  expect_equal(nps_var(c(0, 7, 9)), 2 / 3 * 100)

  # nps_se_
  expect_equal(nps_se_(1:3), 0.304 * 100, tolerance = 0.1)
  expect_equal(nps_se_(c(1, 0, 0)), 0 * 100)
  expect_equal(nps_se_(c(0, 1, 0)), 0 * 100)
  expect_equal(nps_se_(c(0, 0, 1)), 0 * 100)
  expect_equal(nps_se_(c(1, 0, 1)), 0.707 * 100, tolerance = 0.1)
  expect_equal(nps_se_(c(0, 1, 1)), 0.35 * 100, tolerance = 0.1)
  expect_equal(nps_se_(c(1, 1, 1)), 0.471 * 100, tolerance = 0.1)

  # nps_se
  expect_equal(nps_se(c(0, 7, 7, 9, 9, 9)), 0.304 * 100, tolerance = 0.1)
  expect_equal(nps_se(1), 0 * 100)
  expect_equal(nps_se(7), 0 * 100)
  expect_equal(nps_se(9), 0 * 100)
  expect_equal(nps_se(c(1, 9)), 0.707 * 100, tolerance = 0.1)
  expect_equal(nps_se(c(7, 9)), 0.354 * 100, tolerance = 0.1)
  expect_equal(nps_se(c(0, 7, 9)), 0.471 * 100, tolerance = 0.1)

  options(nps.100 = FALSE)

})


test_that("Novel break points work", {
  expect_equal(nps(1:15, breaks = c(0, 6, 8, 15)), 2 / 30)
  expect_equal(nps_var(1:15, breaks = c(0, 6, 8, 15)), 0.8622, tolerance = .002)
})


test_that("Depricated and new functions are consistent, but throw warnings", {
  # nps_var_ and nps.var
  expect_warning(expect_equal(nps_var_(1:3), nps.var(c(0, 7, 7, 9, 9, 9))))
  expect_warning(expect_equal(nps_var_(c(1, 0, 0)), nps.var(1)))
  expect_warning(expect_equal(nps_var_(c(0, 1, 0)), nps.var(7)))
  expect_warning(expect_equal(nps_var_(c(0, 0, 1)), nps.var(9)))
  expect_warning(expect_equal(nps_var_(c(1, 0, 1)), nps.var(c(1, 9))))
  expect_warning(expect_equal(nps_var_(c(0, 1, 1)), nps.var(c(7, 9))))
  expect_warning(expect_equal(nps_var_(c(1, 1, 1)), nps.var(c(0, 7, 9))))
})
