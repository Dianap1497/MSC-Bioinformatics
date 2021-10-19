##Combine vectors

v1 <- c(1,4,2,1,7)
v2 <- c(3,2)
v3 <- c(v1,v2)
v3

c(c(1, 4, 2, 1, 7), c(3, 2))
##Creating a matrix that starts from 1 and ends in 6 and goes 1 by 1 with 
# 3 columns and 2 rows

matrix(seq(from=1, to=6, by=1), ncol=3, nrow=2)

##Blank matrix with NAs
matrix(NA, ncol = 3, nrow=4)

#shape a vector into a matriz dim() function
b <- seq(from=1, to=6, by=1)
dim(b) <- c(2,3)
b

#dim() can also be use to look at the dimensions of the matrix
dim(matrix(1:6, ncol=3, nrow=2))

#Matrixwith rnorm(), n=80 
d <- rnorm(80, 130, 1.5)
matrix(d, ncol=5, nrow=4)

#Creating and visualizing a matrix
our_matrix <- matrix(6:1, ncol=3, nrow=2)
our_matrix

#Return value in the first row and second colum
our_matrix[1,2]

#Return the value in the first row from the first and second column:
our_matrix[1,c(1,2)]

#First row values
our_matrix[1,]

#Second column values
our_matrix[,2]

##matrix of values
init_mat <- matrix(1:6, ncol=3, nrow=2)
init_mat+1

#Make a matrix to replicate the numbers 1,2,3 five times
mat_rep <- matrix(rep(1:3, each=5), nrow=3, ncol=5, byrow=TRUE)
mat_rep

## a vector of the numbers 1 to 5 and multiple the matrices with element-wise multiplication
vec_seq <- 1:5
mat_rep * vec_seq

#Matrix multiplication
##where the first argument is the data to be included in the matrix, and the second and third arguments specify the shape of the matrix.
mat_seq <- matrix(seq(from = 1, to = 20, length.out = 6), ncol=3, nrow=2)

#vector to perform the multiplication
vec_seq <- seq(from = 10, to = 4, length.out = 3)

#multiply the matrices element wise multiplication
mat_seq %*% vec_seq

#matrix: Displaying the logical operator of this matrix for values greater than 10
mat_seq <- matrix(seq(1,20, length.out=6), ncol=3, nrow=2)
mat_seq > 10

#values that are greater than 10
mat_seq[mat_seq > 10]

#Arrays
this_is_an_array <- array(1:24, dim=c(3,4,2))
this_is_an_array

array(1:24, dim=c(3,2,4))

still_an_array <- array(1:24, dim=c(3,2,2,4))
still_an_array

##simple data
simple_data <- data.frame("a" = runif(10, 0, 1),
                          "b" = rnorm(10, 3, 5))

##example calculations
simple_data$calc <- (simple_data$a * simple_data$b) - simple_data$b
simple_data

dataframe_student <- data.frame("name"=c("Anastasia", "Dima", "Katherine", "James",
                                         "Emily", "Michael", "Matthew", "Laura","Kevin", "Jonas"),
                                "score"=c(12.5, 9, 16.5, 12, 9, 20, 14.5, 13.5, 8, 19),
                                "questions"=c(1,3,2,3,2,3,1,1,2,1),
                               "qualify" = c("yes", "no", "yes", "no", "no","yes","yes","no","no","yes"))
dataframe_student
str(dataframe_student)

##Lists
#numeric matrix
num_mat <- matrix(rep(1:3,each=5),
                  nrow = 3,
                  ncol = 5,
                  byrow=TRUE
                  )
#vector of letter
let_vec <- LETTERS[4:16]

#data frame of species information
species_dat <- data.frame("species"=c("a","b"),
                          "observed" = c(TRUE, FALSE))
#list 
our_list <- list(num_mat, let_vec, species_dat,5)
our_list

#first object
our_list[[1]]

#first row of the 3rd object in the list
our_list[[3]][1,]

#list function with names
our_list <- list("numbers_vec" = num_mat,
                 "letters" = let_vec,
                 "spp_pres" = species_dat,
                 "number" = 5)
#display names
names(our_list)

#view the list
our_list$spp_pres

#new list with "data" split into two sites
our_second_list <- list("site_1" = our_list,
                        "site_2" = our_list)
our_second_list

#letters in site 1
our_second_list$site_1$letters


# install.packages("devtools")

install.packages("vroom")
library(vroom)


                          