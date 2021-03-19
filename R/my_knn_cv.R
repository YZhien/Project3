#' k-Nearest Neighbors Cross-Validation
#'
#' This function cauculated k-Nearest Neighbors Cross-Validation
#'
#' @param train A data frame to be calculated for knn Cross-Validation.
#' @param cl A true class value of your data.
#' @param k_nn An integer representing the number of neighbors.
#' @param k_cv A integer representing the number of folds.
#'
#' @return A list with objects with vector of predicted class for observations
#'         and numeric with the cross-validation misclassification error.
#' @keywords prediction
#'
#' @importFrom class knn
#' @importFrom dplyr select
#' @importFrom dplyr filter
#'
#' @examples
#'
#' train <- my_gapminder[4]
#' cl <- unlist(my_gapminder[1])
#' my_knn_cv(train, cl, 5, 5)
#'
#' @export


my_knn_cv <- function(train, cl, k_nn, k_cv) {
  if (class(k_nn)!="numeric") {
    stop()
  }
  if (class(k_cv)!="numeric") {
    stop()
  }
  # split data into k_nn folds
  n <- nrow(train)
  inds <- sample(rep(1:k_cv, length = n))
  fold_df <- data.frame("x" = train, "y" = cl, "fold" = inds)
  cvv_err_sum <- 0


  # iterate through data and record the sum of error
  for(i in 1:k_cv) {
    # select train data
    fold_train <- fold_df %>% filter(fold_df$fold != i)
    # get class of train data
    fold_cl  <- fold_train %>% select("y", "fold")
    # get pure train data
    fold_train <- fold_train[, -which(names(fold_train) == "y")]
    # select test data
    fold_test <- fold_df %>% filter(fold_df$fold == i)
    # get class of test data
    fold_test_cl  <- fold_test %>% select("y", "fold")
    # get pure test data
    fold_test <- fold_test[, -which(names(fold_test) == "y")]


    # predict data
    pred <- knn(fold_train, fold_test, fold_cl$y, k_nn)
    # calculate difference by making data frame
    error <- data.frame("true_s" = fold_test_cl$y, "pred" = pred)
    error <- transform(error, "err" = ifelse(error$true_s==pred, 0, 1))
    cv_error <- sum(error$err) / length(error$true_s)
    # sum the accumulated error
    cvv_err_sum <- cv_error + cvv_err_sum

  }
  # predict data using whole set
  class <- knn(train, train, cl, k_nn)
  cv_error <- cvv_err_sum/k_cv
  # record out put as list
  output <- list("class" = class, "cv_err" <- cv_error)
  return(output)
}
