## Put comments here that give an overall description of what your
## functions do

#The first function creates a special "matrix" object that can cache its inverse.  
#It contains actually four functions that:
#1. Set the value of the matrix
#2. Get the value of the matrix
#3. Set the value of the inverse
#4. Get the value of the inverse

#Here is a sample matrix.
a<-matrix(c(2,3,4,5), nrow = 2, ncol= 2, byrow = TRUE)

makeCacheMatrix <- function(x = matrix()) {
m<-NULL

set <- function(y) {
  x <<- y
  m <<- NULL
}
get <- function() x
setinverse <- function(inverse) m <<- inverse
getinverse <- function() m
list(set = set, get = get,
     setinverse = setinverse,
     getinverse = getinverse)
}

#Here I am using the function to cache an inverse
j<-makeCacheMatrix(a)



## This function refers to the previously defined function.  First it checks to see if the inverse has already been calculated.
#If so it gets the inverse from the cache and skips the computation.  Othersie it calculates the invere of the matrix and
#sets the mean of the data and sets the value of the inverse in the cache via the setinverse function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}

#Here I am calling the function to show the inverse.   
cacheSolve(j)
