# Assignment 3: Working with Matrices in R
# Description: This produces a matrix of computer science modules, median
# grade, and the student obervations for those modules they attended. This
# was part of initial experimentation with R matrices.
# Name: Niall Guerin
# Student ID: 18235079
# Date: 07.10.2018
#######################################
set.seed(10)
N=10
cs1 <- rnorm(N,72,10)
cs2 <- rnorm(N,65,7)
cs3 <- rnorm(N,80,9)
cs4 <- rnorm(N,55,7)
cs5 <- rnorm(N,61,5)

m1 <- matrix(c(cs1,cs2,cs3,cs4,cs5), nrow = 10, ncol = 5)
h <- c("cs1", "cs2", "cs3", "cs4", "cs5")
r <- c(1:10)
col_median <- "median"

# Set initial matrix row and column names
rownames(m1) <- r
colnames(m1) <- h[1:5]

# https://www.rdocumentation.org/packages/base/versions/3.5.1/topics/paste
# See paste() and paste0() to automate below setup
# Dropped use of below after seeing discussion forum advise on using paste and paste0 functions in R
# students <- c("Student#1", "Student#2", "Student#3", "Student#4", "Student#5", "Student#6", "Student#7", "Student#8", "Student#9", "Student#10")
rowtext <- "Student"

# library of functions for core tests

# Clone of matrix 1 for testing purposes
m2 <- m1

#
# helper functions for setting the rank and updating the main matrix resultset
#

set_rank <- function(rlist){
  # store the labels for the sorted list vector values
  label1 <- as.numeric(names(rlist[[1]]))
  label2 <- as.numeric(names(rlist[[2]]))
  label3 <- as.numeric(names(rlist[[3]]))
  label4 <- as.numeric(names(rlist[[4]]))
  label5 <- as.numeric(names(rlist[[5]]))
  
  # update the sequences now that they are sorted as sequence will always start at 1->10.
  # NB: this should only be done IF the input is a sorted list vector
  rlist[[1]] <- r
  rlist[[2]] <- r
  rlist[[3]] <- r
  rlist[[4]] <- r
  rlist[[5]] <- r
  
  # update the names with labels. I lost the names when I did the sort, so reapply stored names on
  # on sorted values
  names(rlist[[1]]) <- label1
  names(rlist[[2]]) <- label2
  names(rlist[[3]]) <- label3
  names(rlist[[4]]) <- label4
  names(rlist[[5]]) <- label5
  
  # return the ranked list with corresponding name labels
  x <- rlist
  x
}

# build ranked results list from original matrix values. I am sure there are more efficient ways to
# do this at matrix level. The only way I got this working for myself is using lists and list access
# after manually constructing lists in another R script and setting labels on vector contents of
# those lists. My approach here could probably be shortened to a few lines if I was smarter about how
# I used apply based on lecture samples. I feel this is clunky even if it gives me the required results
my_list <- list(m2[,1], m2[,2], m2[,3], m2[,4], m2[,5])
my_ranked_list <- lapply(my_list, sort, decreasing = T)
my_result_list <- set_rank(my_ranked_list)

# update matrix column specified in the function input parameter. a sorted list with names MUST be
# provided to this function or the matrix result will NOT render as required by the assignment
# brain stopped working (not sure it ever started) so pretty sure this function 
# can be reduced to a few lines by matching my earlier "r" variable vector (1:10) for students instead
# of this clunky method. This is basically doing a match against my earlier ranked and sorted list using
# the names value to map against the matrix row values from original matrix. If it gets a match, store 
# the value. We subsequently reuse the x variable as a column index for the matrix in conjunction with the
# row number to set that value. I used a dynamica variable x to avoid re-writing the below block 5 times.
update_matrix_columns <- function(the_list, x, y){
  a <- which( ( names(my_ranked_list[[x]]) ) == 1)
  b <- which( ( names(my_ranked_list[[x]]) ) == 2)
  c <- which( ( names(my_ranked_list[[x]]) ) == 3)
  d <- which( ( names(my_ranked_list[[x]]) ) == 4)
  e <- which( ( names(my_ranked_list[[x]]) ) == 5)
  f <- which( ( names(my_ranked_list[[x]]) ) == 6)
  g <- which( ( names(my_ranked_list[[x]]) ) == 7)
  h <- which( ( names(my_ranked_list[[x]]) ) == 8)
  i <- which( ( names(my_ranked_list[[x]]) ) == 9)
  j <- which( ( names(my_ranked_list[[x]]) ) == 10)
  
  # update the matrix parameter - y - by index with the corresponding values from the ranked list
  y[1, x] = a
  y[2, x] = b
  y[3, x] = c
  y[4, x] = d
  y[5, x] = e
  y[6, x] = f
  y[7, x] = g
  y[8, x] = h
  y[9, x] = i
  y[10, x] = j
  
  # return the parameter
  y
}

# utility function to let me specify a matrix, a string, and a column index so I can update columns
update_column_name <- function(x, y, z){
  column_names <- colnames(x)
  new_column <- y
  column_postion <- z
  column_names[column_postion] <- new_column
  column_names
}

# perform direct column updates on the matrix (m3). On each subsequent call, use result variable as
# matrix input parameter to same function so it can reuse prior dataset. Order should be m2, t1, t2,
# t3, t4 to update all 5 columns of our matrix
t1 <- update_matrix_columns(my_ranked_list, 1, m2)
t2 <- update_matrix_columns(my_ranked_list, 2, t1)
t3 <- update_matrix_columns(my_ranked_list, 3, t2)
t4 <- update_matrix_columns(my_ranked_list, 4, t3)
t5 <- update_matrix_columns(my_ranked_list, 5, t4)

# I tend to make copies of variables so I can roll back. Probably overkill but it lets me quickly
# roll back for testing in console which is harder if I overwrite the main matrix variable.
# make a copy of matrix and then set rownames. use paste function to update our matrix rownames
# rowtext is defined as a variable - 'Student', separator is #, and values are 1:10 (vector variable r)
m3 <- t5
rownames(m3) = paste0( rowtext,"#", r)
print(m3)
cat("\n")

# calculate the median. see week 3 slide 27 for examples that do similar operations
# col_median <- "Median"
# the_median <- apply(m3, 1, median)
# # unname the value so we just have a numeric vector and can add this then as a new column to matrix
# the_median <- unname(the_median)

# seemed like I was doing a lot of unnecessary rejigging of my median result here. Search on Google for same
# cbind requirement using apply does it in a single line so have opted for that here. Apply
# across columns 1-5 for rows and get the median of each row.
# Web Source: SO - https://stackoverflow.com/questions/14972926/r-programming-adding-extra-column-to-existing-matrix
m3 <- cbind(m3, apply(m3[,1:5],1,median))

# update the column to include median as "" is output by default from previous R statement
t6 <- update_column_name(m3, col_median, 6)
colnames(m3) <- t6
print(m3)