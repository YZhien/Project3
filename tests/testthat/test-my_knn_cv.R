test_that("my_knn_cv works", {
  expect_type(my_knn_cv(my_gapminder[4],unlist(my_gapminder[1]), 5, 5), 'list')
})

test_that("non-suitable input for parameters throws error", {
  expect_error(my_knn_cv(my_gapminder[4],"a",3,3))
  expect_error(my_knn_cv(my_gapminder[4],"a","t",3))
})


