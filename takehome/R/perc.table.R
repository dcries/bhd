#' Create percentile table function
#'
#' This function calculates percentiles by column for a given data set 
#' @param data Data.frame or matrix in which you want to know the percentiles for each column
#' @keywords perc
#' @export
#' @examples
#' x = matrix(rnorm(1000),ncol=4)
#' create.perc.table(x)


create.perc.table <- function(data){
  return(apply(data,2,quantile,probs=c(0.05,0.1,0.25,0.5,0.75,0.9,0.95)))
}