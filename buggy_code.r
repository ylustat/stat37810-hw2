investment_value<-function(x,y,z=0.95, different_years=c(3,5)){
  # Computes the value of an investment x after 10 years,
  # if there is an annual growth rate of y. Except that in 
  # years 3 and 5, there is a different growth rate of z.
  multiplier = 1 + y
  different_multiplier = 1 + z # some comments
  beginning_x=x
  for(i in 1:10){
    i=i+1
    if (!(i %in% different_years)){      # should be true unless i is in the different_years vector
      if (i == 2) {
        x <- beginning_x*multiplier
      } else {
        x <- x*multiplier
      }
      
    } else {
      # In the different years we had a recession
      x <- x * different_multiplier
    }
  }
  return(x)
}
