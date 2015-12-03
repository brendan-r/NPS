
# Test the Net Promoter Categories ----------------------------------------

context("Basic Stats")

test_that("Credentials are in the local repo", {

  # Detractors, Passives, and Promoters for 0:10 data
  ppd010 <- c(rep("Detractor", 7), rep("Passive", 2), rep("Promoter", 2))

  expected <- c(NA, NA,  ppd010, NA, NA)

  # User should be warned if they're entering (some) invalid data
  expect_warning(
    cats <- npc(-2:12),
    "values outside specified"
  )

  # Does it NA in the right way?
  expect_true(all(is.na(expected) == is.na(cats)))

  # When it outputs numbers, are they the right ones?
  expect_true(all(na.omit(cats) == na.omit(expected)))

})
