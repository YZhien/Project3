test_that("my_knn_cv works", {
  expect_type(my_rf_cv(5, my_penguins), 'double')
  expect_type(my_rf_cv(10, my_penguins), 'double')
})

test_that("non-suitable input for parameters throws error", {
  expect_error(my_rf_cv(1, my_penguins))
  expect_error(my_rf_cv(1, "my"))
  expect_error(my_rf_cv(1, 000))



})

