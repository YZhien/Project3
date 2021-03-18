test_that("my_t.test successfully", {
  expect_type(my_t.test(my_gapminder$lifeExp, alternative = "two.sided", mu = 60), 'list')
  expect_type(my_t.test(my_gapminder$lifeExp, alternative = "two.sided", mu = 40), 'list')
  expect_type(my_t.test(my_gapminder$lifeExp, alternative = "less", mu = 67), 'list')

})

test_that("non-sutable input throws error", {
  expect_error(my_t.test(my_gapminder$lifeExp, alternative = "two.sided", mu = "ii"))
  expect_error(my_t.test(my_gapminder$lifeExp, alternative = "twd", mu = "ii"))
  expect_error(my_t.test("yyy", alternative = "two.sided", mu = "ii"))
  expect_error(my_t.test("my", alternative = "twded", mu = "ii"))



})
