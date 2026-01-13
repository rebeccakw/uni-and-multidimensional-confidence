calculate_x <- function(d, s, cat_one_sigma, cat_two_sigma) {
  a <- (0.5*log(( (s*s) + (cat_two_sigma*cat_two_sigma) )/( (s*s) + (cat_one_sigma*cat_one_sigma) )))
  b <- ((( (cat_two_sigma*cat_two_sigma) - (cat_one_sigma*cat_one_sigma) ) /
           ( 2*( (s*s) + (cat_one_sigma*cat_one_sigma) ) *( (s*s) + (cat_two_sigma*cat_two_sigma) ) )))
  prod <- (a - d)/b
  
  #find where values are less than 0 
  index <- (prod < 0)
  
  #take the absolute value of negative values, so we can find the square root
  prod[index] <- abs(prod[index])
  prod <- sqrt(prod)
  
  #returns the original negative values as negative values 
  prod[index] <- - (prod[index])
  return(prod)   
}

calculate_d <- function(x, s, cat_one_sigma, cat_two_sigma) {
  a <- 0.5 * log( ((s*s) + (cat_two_sigma*cat_two_sigma)) / ((s*s) + (cat_one_sigma*cat_one_sigma)) )
  b <- (cat_two_sigma*cat_two_sigma) - (cat_one_sigma*cat_one_sigma)
  c <- 2 * ((s*s) + (cat_one_sigma*cat_one_sigma)) * ((s*s) + (cat_two_sigma*cat_two_sigma))
  prod <- a - ((b/c)*(x*x))
  return(prod)
}