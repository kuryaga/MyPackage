# Testing if extract_values_from_nc returns a list
test_that("extract_values_from_nc returns a list", {

  result <- extract_values_from_nc(data_directory = "./data",
                                   file_pattern = ".nc",
                                   location_ids = c(327, 328, 329))

  expect_is(result, "list")
})


# Testing if extract_values_from_nc handles invalid directory
test_that("extract_values_from_nc handles invalid directory", {

  invalid_result <- extract_values_from_nc(data_directory = "./invalid_directory",
                                           file_pattern = ".nc",
                                           location_ids = c(327, 328, 329))

  expect_is(invalid_result, "NULL")
})
