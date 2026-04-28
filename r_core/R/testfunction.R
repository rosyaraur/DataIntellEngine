# r_core/R/testfunction.R

#' Test function x y 
#' 
#' @param x value of x 
#' @param y value of y
#' @return sqredProd x^2 = y^2 
#' 
testfunction <- function(x, y){
  sqredProd = x*2 + y*2
  return(sqredProd)
}