## Put comments here that give an overall description of what your functions do

# Matrix inversion is usually a costly computation and there may be some benefit 
# to caching the inverse of a matrix rather than compute it repeatedly (there are also 
# alternatives to matrix inversion that we will not discuss here). Your assignment is
# to write a pair of functions that cache the inverse of a matrix.
#
# 1) makeCacheMatrix: This function creates a special "matrix" object that can 
# cache its inverse.
# 2) cacheSolve: This function computes the inverse of the special "matrix" 
# returned by makeCacheMatrix above. If the inverse has already been calculated 
# (and the matrix has not changed), then the cachesolve should retrieve the inverse
# from the cache.

## ASSIGNMENT # 2 in R 

# 1) Creating a makeCacheMatrix 
# Purpose of the matrix is to cache a special matrix object and cache its inversion
# for this purpose first function is the get function. 
### get function like the example given gets the matrix 'x'from the main function
#
### set function is used to change the matrix... when matrix is different, the (inv)inverse
# is changed to NULL, for recalculation usung the solve function.
### setinverse and getinverse are used to store the inv into MakeCacheMatrix(setinverse)
# all four functions are stored in makecachematrix using the list() fucntion.


makeCacheMatrix <- function(x = matrix()) {
  inv<- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) inv <<- solve
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

### second function is cacheSolve, gets the inverse if the getinverse is not null and
#prints the getting cached data. if the it is null then gets the matrix and calculates 
# the inverse and prints the inverse. x$setinverse(inv) stores the inverse in the function makeCacheMatrix.  
cacheSolve <- function(x=matrix, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  matrix <- x$get()
  inv <- solve(matrix, ...)
  x$setinverse(inv)
  inv
}

