#' Cost function for using \code{cv.glm} fitting procedure
#' 
#' This function just provides a way to get the Brier Score for a model used
#' in the \code{cv.glm} function. Of course, this is just the \code{MSE} but
#' I didn't realize that until later. So here it is. 
#' 
#' @param y the dependent binary variable
#' @param phat the predicted probabilities of a model

#' @return the mean square error of the predicted probabilities 
#' @examples
#' dependent <- sample(c(0, 1), 1000, replace = T)
#' p <- runif(1000)
#' mse <- brierB(dependent, p)

brierB <- function(y, pHat) {
  mean((y-pHat)^2)
}