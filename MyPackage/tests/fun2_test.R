library(testthat)
library(advR)

# Creating sample data for testing
sample_data <- extract_values_from_nc(location_ids = c(327, 328, 329))

test_that("calculate_idf function calculates IDF quantiles and generates plot", {
  #checking if the output is a list with 'data' and 'plot' elements
  expect_that(calculate_idf(x_list = sample_data)$idf_results, is_a("data.table"))
  expect_that(calculate_idf(x_list = sample_data)$idf_plot, is_a("ggplot"))

  #checking if the output data table has the expected columns
  expect_equal(colnames(calculate_idf(x_list = sample_data)$idf_results), c("cell_id", "dur", "rp", "value"))

  #checking if the generated plot is not NULL
  expect_false(is.null(calculate_idf(x_list = sample_data)$idf_plot))
})
