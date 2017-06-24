#' Create percentile table function
#'
#' This function calculates percentiles by column for a given data set. There are no checks for NAs or anything 
#' else and thus this isa development level function.
#' @param data Data.frame or matrix in which you want to know the percentiles for each column
#' @keywords perc
#' @return A data.frame with percentiles in the columns corresponding to the columns input 
#' @export
#' @examples
#' x = matrix(rnorm(1000),ncol=4)
#' create.perc.table(x)


create.perc.table <- function(data){
  return(apply(data,2,quantile,probs=c(0.05,0.1,0.25,0.5,0.75,0.9,0.95)))
}