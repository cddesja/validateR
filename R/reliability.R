#
# Coefficient Alpha
#
coef_alpha <- function(x){
  k <- ncol(x)
  item <- sum(apply(x, 2, var))
  tot <- var(rowSums(x))
  alpha <- k / (k - 1) * (1 - item/tot)
  return(alpha)
}

#
# Kuder-Richardson 20
#
kr20 <- function(x){
  k <- ncol(x)
  item <- sum(apply(x, 2, mean)*(1-apply(x, 2, mean)))
  tot <- var(rowSums(x))
  alpha <- k / (k - 1) * (1 - item/tot)
  return(alpha)
}

#
# Spearman Brown Correction: Split - Half
#
spear_half <- function(x){
  half <- 2 * x / (1 + x)
  return(half)
}

#
# New length given desired reliability 
#
new_length <- function(desired, present, test_length){
  new <- desired * (1 - present) / (present * (1 - desired)) * test_length
  return(new)
}

#
# Standard error of measurement
#
sem <- function(sigma, reliability){
  sem <- sigma * sqrt(1 - reliability)
  return(sem)
}

#
# Standard error of difference
#
sed <- function(sem1, sem2, type = "sem"){
  if(type == "sem")
    sed <- sqrt(sem1 + sem2)
  if(type == "r")
    sed <- sigma * sqrt(2 - sem1 - sem2)
  return(sed)
}