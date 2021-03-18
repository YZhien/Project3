#' a one sample t-test in R
#'
#' This function cauculated for a one sample t-test.
#'
#' @param x a numeric vector of data.
#' @param alternative a character string specifying the alternative hypothesis.
#'    This should only accept \code{two.sided}, \code{less}, or \code{greater}.
#'    Otherwise, your function should throw an informative error.
#' @param mu An integer representing the number of neighbors.
#'
#'
#' @return A \code{list} with elements :
#'        \code{test_stat}:the numeric test statistic
#'        \code{df}:the degrees of freedom
#'        \code{alternative}:the value of the parameter \code{alternative}
#'        \code{p_val}:the numeric p-value
#'
#'
#' @importFrom stats pt
#' @importFrom stats sd
#'
#'
#' @examples
#'
#'
#' my_t.test(my_gapminder$lifeExp, alternative = "two.sided", mu = 60)
#'
#' @export



# Function: a one sample t-test in R
# Input   : a a numeric vector of data
#           a character string specifying the alternative hypothesis
#           a number indicating the null hypothesis value of the mean
# Output  : the numeric test statistic
#           the degrees of freedom
#           the value of the parameter alternative
#           the numeric p-value
my_t.test <- function(x, alternative, mu) {
  # calculate size of vector
  n <- length(x)
  # calculate sample mean
  x_bar <- mean(x)
  # calculate sample standard deviation
  sampelSD <- sd(x)
  # calculate sample standard error
  sampelSE <- sampelSD/sqrt(n)
  # calculate t test statistic
  statisticTTest <- (x_bar - mu)/sampelSE
  # calculate degrees of freedom
  df <- n - 1

  # calculate p value
  p_val <- if(alternative == "two.sided") {
    # two sided
    pt(-abs(statisticTTest), df) +
      pt(abs(statisticTTest), df, lower.tail = FALSE)
  } else if(alternative == "less") {
    # one sided
    pt(statisticTTest, df)
  } else if(alternative == "greater"){
    pt(statisticTTest, df, lower.tail = FALSE)
  }
  # alternative hypothesis
  h_a <- if(alternative == "two.sided") {
    c("The true mean is not equals to", mu)
  } else if(alternative == "less") {
    c("The true mean is less than", mu)
  } else {
    c("The true mean is greater than", mu)
  }
  # summary the output
  result <- list("test_stat"   = statisticTTest,
                 "p_val"       = p_val,
                 "df"          = df,
                 "alternative" = h_a
  )
  # output
  return(result)
}
