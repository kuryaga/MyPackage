library(testthat)


test_that("calculate_idf function calculates IDF quantiles and generates plot", {

  expect_that(calculate_idf(data_list = sample_data)$data, is_a("data.table"))
  expect_that(calculate_idf(data_list = sample_data)$plot, is_a("ggplot"))

})
