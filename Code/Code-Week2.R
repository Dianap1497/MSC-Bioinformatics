#Homework Week 1


#Function logarithm (base 10) of a vector 
Lfunction <- function (my_vec){
  
  new_vector <-  log10(my_vec) - my_vec  
  return(new_vector)
  
}
new_vector

#mean, std ,se and saved into a new vector where each of the objects are named
mean(new_vector)
sd(new_vector)
std_er <- sd(new_vector) / sqrt(length(new_vector))
std_er

final_Vector <- c("Mean"= -27.34706, "Standard Deviation" = 14.30271, "Standard Error" = 1.430271 )
final_Vector

#sequence of numbers from 15 to 100, find the mean of the numbers in this vector which are >20 and <60, and the sum of the numbers in this vector which are>48
seq_numbers <- runif(n=100, min=15, max=100)
seq_numbers

new_sq <- seq_numbers>20 & seq_numbers<60 & sum(seq_numbers > 48)
new_sq

mean(new_sq)

#Function that returns the max and min value of a vector

vec1 <- c(1,2,3,4,5,6,7,12,8)

Min_function <- function(vec1)
{
  min <- Inf
  
  for(n in 1:length(vec1))
  {
    value <- vec1[n]
    if(value < min )
    {
      min <- value
    }
  }
  return(min)
}

Min_function(vec1)

#Max

Max_function <- function(vec1)
{
  max <- -Inf
  
  for(n in 1:length(vec1))
  {
    value <- vec1[n]
    if(value > max )
    {
      max <- value
    }
  }
  return(max)
}

Max_function(vec1)

##different way to revert
a <- 10
b <- 20
c <- a+b
d <- b-a

