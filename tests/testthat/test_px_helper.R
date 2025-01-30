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

test_that("codes2names handles to_name = TRUE correctly", {
  # Example data
  x <- data.frame(a = c("a1", "a2"),
                  b = c("b1", "b2"),
                  c = c("c1", "c2"),  # Character column
                  d = factor(c("d1", "d2")))  # Factor column

  cn <- list(a = c("a1" = "first", "a2" = "second"),
             b = c("b1" = "other", "b2" = "something"),
             c = c("c1" = "group1", "c2" = "group2"),
             d = c("d1" = "category1", "d2" = "category2"))

  # Apply function with to_name = TRUE (should apply to all mapped variables)
  result <- codes2names(x, cn, to_name = TRUE)

  # Check if all relevant columns got _name and _code suffixes
  expect_true(all(c("a_code", "a_name", "b_code", "b_name", "c_code", "c_name", "d_code", "d_name") %in% colnames(result)))

  # Check value replacements
  expect_equal(result$a_name, factor(c("first", "second")))
  expect_equal(result$d_name, factor(c("category1", "category2")))
})

test_that("codes2names handles to_name = NULL correctly (should return unmodified data)", {
  # Example data
  x <- data.frame(a = c("a1", "a2"),
                  b = c("b1", "b2"),
                  d = factor(c("d1", "d2")))

  cn <- list(a = c("a1" = "first", "a2" = "second"),
             b = c("b1" = "other", "b2" = "something"),
             d = c("d1" = "category1", "d2" = "category2"))

  # Apply function with to_name = NULL
  result <- codes2names(x, cn, to_name = NULL)

  # Expect the original dataframe to be unchanged
  expect_equal(result, x)
})

test_that("codes2names handles to_name = FALSE correctly (should return unmodified data)", {
  # Example data
  x <- data.frame(a = c("a1", "a2"),
                  b = c("b1", "b2"),
                  d = factor(c("d1", "d2")))

  cn <- list(a = c("a1" = "first", "a2" = "second"),
             b = c("b1" = "other", "b2" = "something"),
             d = c("d1" = "category1", "d2" = "category2"))

  # Apply function with to_name = FALSE
  result <- codes2names(x, cn, to_name = FALSE)

  # Expect the original dataframe to be unchanged
  expect_equal(result, x)
})

test_that("codes2names handles to_name as a character vector correctly", {
  # Example data
  x <- data.frame(a = c("a1", "a2"),
                  b = c("b1", "b2"),
                  d = factor(c("d1", "d2")))

  cn <- list(a = c("a1" = "first", "a2" = "second"),
             b = c("b1" = "other", "b2" = "something"),
             d = c("d1" = "category1", "d2" = "category2"))

  # Apply function with to_name = "a" (only column "a" should be modified)
  result <- codes2names(x, cn, to_name = "a")

  # Check that only "a" is modified
  expect_true(all(c("a_code", "a_name") %in% colnames(result)))
  expect_false(any(c("b_code", "b_name", "d_code", "d_name") %in% colnames(result)))

  # Check value replacements
  expect_equal(result$a_name, factor(c("first", "second")))
})

test_that("codes2names handles extra entries in codes_names gracefully", {
  # Example data
  x <- data.frame(a = c("a1", "a2"),
                  b = c("b1", "b2"),
                  d = factor(c("d1", "d2")))

  # codes_names includes an extra key "extra_column" not in x
  cn <- list(a = c("a1" = "first", "a2" = "second"),
             b = c("b1" = "other", "b2" = "something"),
             d = c("d1" = "category1", "d2" = "category2"),
             extra_column = c("e1" = "extra1", "e2" = "extra2"))  # Extra column not in x

  # Apply function
  result <- codes2names(x, cn, to_name = TRUE)

  # Ensure result structure is correct (no errors, no extra columns added)
  expect_false("extra_column" %in% colnames(result))
  expect_false("extra_column_name" %in% colnames(result))
  expect_false("extra_column_code" %in% colnames(result))

  # Ensure expected columns were correctly transformed
  expect_true(all(c("a_code", "a_name", "b_code", "b_name", "d_code", "d_name") %in% colnames(result)))

  # Check values
  expect_equal(result$a_name, factor(c("first", "second")))
  expect_equal(result$b_name, factor(c("other", "something")))
  expect_equal(result$d_name, factor(c("category1", "category2")))
})
