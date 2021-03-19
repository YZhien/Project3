#' generate a function that fits a linear model in R
#'
#' To create a functiona and this function fits a linear model in R.
#'
#' @param f a \code{formula} class object, similar to lm().
#' @param data input data frame.
#'
#'
#' @return A \code{table} similar to summary() :
#'   with rows for each coefficient and columns for the
#'   \code{Estimate}, \code{Std. Error}, \code{t value},
#'   and \code{Pr(>|t|)}.
#'
#' @keywords inference
#' @keywords prediction
#'
#' @importFrom stats model.matrix
#' @importFrom stats model.response
#' @importFrom stats model.frame
#'
#' @examples
#'
#'my_lm(lifeExp ~ gdpPercap, my_gapminder)
#'
#'
#' @export


my_lm <- function(f, data) {

  # model matrix X
  model_matrix_x <- model.matrix(f, data)
  # model frame
  model_frame_x_y <- model.frame(f, data)
  # model response Y
  model_matrix_y <- model.response(model_frame_x_y)
  # model frame object
  model_frame    <- model.frame(f, data)

  beta_bar       <-  solve(t(model_matrix_x) %*% model_matrix_x) %*%
    t(model_matrix_x) %*% model_matrix_y
  # calculate degree of freedom
  df <- length(model_matrix_y) - ncol(model_matrix_x)
  # calculate square of sigma
  sigma_square <- sum((model_matrix_y -
                         model_matrix_x %*% beta_bar)^2/df)
  # calculate standard error
  se_matrix <- sqrt(sigma_square *
                      solve(t(model_matrix_x) %*% model_matrix_x))
  se <- diag(se_matrix)
  # calculate t value
  t_val <- beta_bar / se
  # calculate p value
  p_values <- c(2 * pt(abs(t_val), df, lower.tail = FALSE))
  # summary the output
  # bind them together
  beta_bar <- t(beta_bar)
  t_val <- t(t_val)
  result <- t(rbind(beta_bar, se, t_val, p_values))
  # change type to matrix
  result <- matrix(result, ncol = 4)
  colnames(result) <- c("Estimate", "Std. Error", "t value", "Pr(>|t|)")
  rownames(result) <- colnames(beta_bar)
  # convert matrix to table
  result <- as.table(result)
  # output
  return(result)
}
