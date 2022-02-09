test_that("check_message_print", {

  options(SpatialKDE.suppres_message = NULL)

  r <- check_message_print()

  expect_true(r)

  options(SpatialKDE.suppres_message = TRUE)

  r <- check_message_print()

  expect_false(r)

  options(SpatialKDE.suppres_message = "a")

  r <- check_message_print()

  expect_true(r)
})
