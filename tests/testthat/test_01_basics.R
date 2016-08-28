
# Test the Net Promoter Categories ----------------------------------------

context("Basic Stats")

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

})

test_that("Statistical functions agree with themselves", {

})

test_that("Formatting options work", {

})

test_that("Novel break points work", {

})
