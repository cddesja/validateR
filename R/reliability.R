#' Coefficient Alpha
#'
#' This calculates coefficient alpha for a given matrix or data.frame. Note that the matrix or data.frame should be at the item level and that it needs to be numeric not characters or factors.
#' 
#'@param data A matrix or data.frame containing the item level responses
#'@export
coef_alpha <- function(data){
  k <- ncol(data)
  item <- sum(apply(data, 2, var))
  tot <- var(rowSums(data))
  rel <- k / (k - 1) * (1 - item/tot)
  return(rel)
}

#'
#' Kuder-Richardson 20
#'
#' This calculates Kuder-Richarson 20 for a given matrix or data.frame. Note that the matrix or data.frame should be at the item level and that it needs to be numeric not characters or factors. KR-20 is appropriate for dichotomously scored items, \code{coef_alpha} is appropriate for non-dichotomous items.
#'  
#' @param data A matrix or data.frame containing the item level responses
#' @export
kr20 <- function(data){
  k <- ncol(data)
  item <- sum(apply(data, 2, mean)*(1-apply(data, 2, mean)))
  tot <- var(rowSums(data))
  rel <- k / (k - 1) * (1 - item/tot)
  return(rel)
}

#'
#' Spearman Brown Correction: Split - Half
#'
#' This calculates the Spearman Brown correction for a split half only. 
#' 
#' @param x The correlation of the two split halves
#' @export
spear_half <- function(x){
  half <- 2 * x / (1 + x)
  return(half)
}

#'
#' New test length given a desired reliability
#'
#' The calculates what the length of a test should be given your desired reliability, your present reliability, and the current length of the test.
#' @param desired Desired reliability
#' @param present Current reliability
#' @param test_length Current test length
#' @export
new_length <- function(desired, present, test_length){
  new <- desired * (1 - present) / (present * (1 - desired)) * test_length
  return(new)
}

#'
#' Standard error of measurement
#'
#' This calculates the standard error of measurement given the standard deviation of your total scores and your current reliabilty
#' @param sigma Standard deviation of total test scores
#' @param reliablity Current reliability
#' @export
sem <- function(sigma, reliability){
  sem <- sigma * sqrt(1 - reliability)
  return(sem)
}

#'
#' Standard error of difference
#'
#' Calculate the standard error of difference given the SEM of two tests or the reliablity of two tests and the standard deviation of the total test scores
#' @param sem1 Either the SEM or reliablity of the 1st test
#' @param sem2 Either the SEM or reliability of the 2nd test
#' @param type Either sem, if specifying SEMs or r, if specifying reliability estimates
#' @param sigma The standard deviation of the test scores if reliablity estimates were provided
sed <- function(sem1, sem2, type = "sem", sigma = NULL){
  if(type == "sem")
    sed <- sqrt(sem1 + sem2)
  if(type == "r")
    sed <- sigma * sqrt(2 - sem1 - sem2)
  return(sed)
}