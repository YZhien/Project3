test_that("my_rf_cv works", {
  expect_type(my_knn_cv(my_gapminder[4],unlist(my_gapminder[1]), 5, 5), 'list')
})

test_that("non-suitable input for parameters throws error", {
  expect_error(my_knn_cv(my_gapminder[4],"a",3,3))
  expect_error(my_knn_cv(my_gapminder[4],"a","t",3))
  expect_error(my_knn_cv(my_gapminder[4],"a","t",'yu'))
  expect_error(my_knn_cv(777,"a","t",'yu'))
  expect_error(my_knn_cv(my_gapminder[4],my_gapminder[4],"t",'yu'))
  expect_error(my_knn_cv(NA,my_gapminder[4],"t",'yu'))
})


