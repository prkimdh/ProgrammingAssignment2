## Programming Assignment #2

## The first function makeCacheMatrix includes a list of 4 functions:
## 1) set the matrix
## 2) get the matrix
## 3) set the inverse of a matrix
## 4) get the inverse of a matrix
## The second function cacheSolve uses the output from makeCacheMatrix
## to compute the matrix inverse.  If the inverse has already been calculated,
## it will retrieve the inverse from the cache without recalculation.

makeCacheMatrix <- function(x = matrix()) {
  inv.mat <- NULL                              ## An empty matrix to save the inverse of a matrix 'x'
  set <- function(y) {                         ## set the matrix 'x'
    x <<- y
    inv.mat <<- NULL
  }
  get <- function() x                          ## get the matrix 'x'
  setinv <- function(inv) inv.mat <<- inv      ## set the inverse of a matrix to inv.mat
  getinv <- function() inv.mat                 ## get the inverse of a mtarix from inv.mat
  list(set = set, 
       get = get, 
       setinv = setinv, 
       getinv = getinv)
}

## Write a short comment describing this function

cacheSolve <- function(x, ...) {               ## x is the output of makeCacheMatrix function
  inv.mat <- x$getinv()                        
  if(!is.null(inv.mat)) {                      ## if the inverse has been calculated 
    message("getting cached data")
    return(inv.mat)                            ## return the saved inverse of the matrix
  }
  data <- x$get()                              ## if the inverse has not been calculated
  inv.mat <- solve(data, ...)                  ## compute the inverse of the matrix
  x$setinv(inv.mat)                            ## setting the inverse in the cache
  inv.mat
}