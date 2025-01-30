test_that("codes2names adds '_name' and '_code' columns correctly", {
  # Create sample data
  x <- data.frame(a = c("a1", "a2"), b = c("b1", "b2"))
  cn <- list(a = c("a1" = "first", "a2" = "second"),
             b = c("b1" = "other", "b2" = "something"))

  # Run the function
  result <- codes2names(x, cn)

  # Check that '_name' and '_code' columns are added
  expect_true("a_name" %in% names(result))
  expect_true("b_name" %in% names(result))
  expect_true("a_code" %in% names(result))
  expect_true("b_code" %in% names(result))

  # Check that the values in the name columns are correct
  expect_equal(result$a_name, factor(c("first", "second")))
  expect_equal(result$b_name, factor(c("other", "something")))

  # Check that the original codes are preserved in '_code' columns
  expect_equal(result$a_code, factor(c("a1", "a2")))
  expect_equal(result$b_code, factor(c("b1", "b2")))
})

test_that("codes2names supports custom suffixes", {
  # Create sample data
  x <- data.frame(a = c("a1", "a2"), b = c("b1", "b2"))
  cn <- list(a = c("a1" = "first", "a2" = "second"),
             b = c("b1" = "other", "b2" = "something"))

  # Run the function with custom suffixes
  result <- codes2names(x, cn, name_suffix = "_label", code_suffix = "_identifier")

  # Check that custom suffixes are applied
  expect_true("a_label" %in% names(result))
  expect_true("b_label" %in% names(result))
  expect_true("a_identifier" %in% names(result))
  expect_true("b_identifier" %in% names(result))

  # Check that the values in the custom name columns are correct
  expect_equal(result$a_label, factor(c("first", "second")))
  expect_equal(result$b_label, factor(c("other", "something")))

  # Check that the original codes are preserved in custom '_identifier' columns
  expect_equal(result$a_identifier, factor(c("a1", "a2")))
  expect_equal(result$b_identifier, factor(c("b1", "b2")))
})

test_that("codes2names works with specified columns only", {
  # Create sample data
  x <- data.frame(a = c("a1", "a2"), b = c("b1", "b2"))
  cn <- list(a = c("a1" = "first", "a2" = "second"),
             b = c("b1" = "other", "b2" = "something"))

  # Run the function with only one column specified
  result <- codes2names(x, cn, to_name = "a")

  # Check that only the specified column has '_name' and '_code' suffixes
  expect_true("a_name" %in% names(result))
  expect_true("a_code" %in% names(result))
  expect_false("b_name" %in% names(result))
  expect_false("b_code" %in% names(result))

  # Check that the values in the name columns are correct
  expect_equal(result$a_name, factor(c("first", "second")))
  expect_equal(result$a_code, factor(c("a1", "a2")))
})

test_that("codes2names returns unchanged data when to_name is NULL", {
  # Create sample data
  x <- data.frame(a = c("a1", "a2"), b = c("b1", "b2"))
  cn <- list(a = c("a1" = "first", "a2" = "second"),
             b = c("b1" = "other", "b2" = "something"))

  # Run the function with to_name = NULL
  result <- codes2names(x, cn, to_name = NULL)

  # Check that the result is identical to the input
  expect_equal(result, x)
})


test_that("codes2names translates codes to names and keeps original column names when code_suffix is empty", {
  # Example data
  x <- data.frame(a = c("a1", "a2"), b = c("b1", "b2"))
  cn <- list(a = c("a1" = "first", "a2" = "second"),
             b = c("b1" = "other", "b2" = "something"))

  # Set code_suffix to empty string
  result <- codes2names(x, cn, code_suffix = "")

  # Check structure: names should be added, but original code columns should not be renamed
  expect_true("a" %in% colnames(result))  # Original column remains
  expect_true("b" %in% colnames(result))  # Original column remains
  expect_true("a_name" %in% colnames(result))  # Name column exists
  expect_true("b_name" %in% colnames(result))  # Name column exists

  # Check values
  expect_equal(result$a_name, factor(c("first", "second")))
  expect_equal(result$b_name, factor(c("other", "something")))
  expect_equal(result$a, factor(c("a1", "a2")))  # Original column remains unchanged
  expect_equal(result$b, factor(c("b1", "b2")))  # Original column remains unchanged
})

test_that("codes2names replaces original columns when name_suffix is empty", {
  # Example data
  x <- data.frame(a = c("a1", "a2"), b = c("b1", "b2"))
  cn <- list(a = c("a1" = "first", "a2" = "second"),
             b = c("b1" = "other", "b2" = "something"))

  # check that gives an error as name suffix can't be empty. Otherwise it will override code column.
  expect_error(codes2names(x, cn, name_suffix = ""))


})
