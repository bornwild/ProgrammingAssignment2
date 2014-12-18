## This provides wrapper functions to create a custom matrix .Additionally it has 
## routines which can compute  and  cache  its inverse lazily  , thus avoiding recomputation 

## Creates a custom inverse cacheable  matrix .Input is a matrix 
## Inverse calcualtion assumes that the matrix which is invertible 

## Sample 
# > k <- matrix(c(4,7,2,6),2,2)
# > x <- makeCacheMatrix(k)
# > cacheSolve(x)
#     [,1] [,2]
# [1,]  0.6 -0.2
# [2,] -0.7  0.4
# > cacheSolve(x)
# getting cached inverse
#     [,1] [,2]
# [1,]  0.6 -0.2
# [2,] -0.7  0.4

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInv <- function(argInv) inv <<- argInv
  getInv <- function() inv
  list(set = set, get = get,
       setInv = setInv,
       getInv = getInv)
}


## Computes the inverse of the matrix lazily 
## Input to function is a matrix created by the makeCacheMatrix function

cacheSolve <- function(x) {
  inv <- x$getInv()
  if(!is.null(inv)) {
    message("getting cached inverse")
    return(inv)
  }
  mx <- x$get()
  inv <- solve(mx) ## Built in R function solve
  x$setInv(inv)
  inv
}
