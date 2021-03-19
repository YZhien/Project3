test_that("my_knn_cv works", {
  expect_type(my_lm(lifeExp ~ gdpPercap, my_gapminder), 'double')
})

test_that("non-suitable input for parameters throws error", {
  expect_error(my_lm_r(1, my_gapminder))
  expect_error(my_lm_r("y", my_gapminder))
  expect_error(my_lm_r("y", 123))
  expect_error(my_lm_r(99, TRUE))

})
