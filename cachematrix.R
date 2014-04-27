# R PROGRAMMING
# PEER ASSESSMENT

## Assignment: Caching the Inverse of a Matrix
## Matrix inversion is usually a costly computation
## and their may be some benefit to caching the inverse of a matrix
## rather than compute it repeatedly (there are also alternatives to matrix inversion
## that we will not discuss here). (Coursera Page)

## This assignment is to write a pair of functions that cache the inverse of a matrix.

## My function follows the rules described in the instruction for this assignment. I have two
## matrix function on called makeCachematrix and the other called cacheSolve

##############################
###   Make Cache Fuction   ###
##############################

# This function uses the "<<-" operator which is used to assign a value to an object in an environment that is different from the current environment

makeCacheMatrix <- function(x = matrix()) { # I take an argument x of type matrix
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  inverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set=set, get = get,
       inverse = inverse,
       getinverse = getinverse) # I get a list with four fuctions
}
#################################################################################

##############################
### Cache Solve  Fuction   ###
##############################

# The input for this function is the "special matrix" made from makeCacheMatrix
# The result is the inverse of the matrix coming whether from the special matrix's  cache or computation.
# https://class.coursera.org/rprog-002/forum/thread?thread_id=696


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()   #takes the funtion getinverse and send it to m
  if(!is.null(m)) {     #check if there is the inverse of the matrix
    message("getting cached data")
    return(m)
  }
  data <- x$get()   # get the matrix and send it to data
  m <- solve(data, ...) #finds the inverse of the matrix
  x$inverse(m)   # shows the inverse of the matrix
  m
}
#################################################################################


######################################
# EXAMPLE TO USE WITH  cachematrix
# (from: https://class.coursera.org/rprog-002/forum/thread?thread_id=696)

mymatrix <- makeCacheMatrix(matrix(1:4,2))

# this is a "special matrix" using makeCacheMatrix function, I get:

mymatrix$get()
mymatrix$getinverse()

mymatrix$set(matrix(5:8,2))

cacheSolve(mymatrix)   # I get the inverse of the matrix
cacheSolve(mymatrix)

mymatrix$getinverse()   #show the inverse of the matrix

# test inverse correctness
matrix2 = mymatrix$getinverse()
mymatrix$get() %*% matrix2           #i get the identity matrix
