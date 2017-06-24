#' Predict LDL levels
#'
#' This function uses LASSO to select predictors in order to run linear regression to predict LDL for individuals. Outputs a vector of predicted values of LDL based on input
#' @param data Data.frame or matrix where the first column is what you want to predict, and remaining columns are what you want to consider as possibly covariates
#' @keywords preds
#' @export
#' @examples
#' response <- rnorm(1000)
#' predictors <- matrix(rnorm(10000),ncol=10)
#' estimate.ldl(cbind(response,predictors))


estimate.ldl <- function(data){
  nc <- ncol(data)
  x <- as.matrix(data[,2:nc])
  m1 <- glmnet::cv.glmnet(x,data[,1])
  keep <- ((as.numeric((coef(m1, s = "lambda.1se"))))!=0)[-1]
  #xreduced <- x[,which(coef(m1, s = "lambda.1se") !=0)[-1]-1]
  xreduced <- x[,keep]
  m2 <- lm(data[,1]~xreduced)
  preds <- predict(m2)
  return(preds)
}