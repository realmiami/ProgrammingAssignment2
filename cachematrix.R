## Week 3: Assignment
## Coursera: R Programming

## The specific purpose for this function is for 
## its inverse to be cached in a so-called "matrix". 
## Calculations to its inverse of square are done
## through a solve function within the R program which can 
## be shown below.

makeCacheMatrix <- function(x = matrix()) {
invmat <- NULL

set <- function(y) {
  x <<- y
  invmat <<- NULL
}
get <- function()x
setinvmatsoln <- function(invsoln) invmat <<- invsoln
getinvmatsoln <- function() invmat
 
list(set = set, get = get, 
     setinvmatsoln = setinvmatsoln, 
     getinvmatsoln = getinvmatsoln)
}

## Here, the computation for the inverse of indicated matrix 
## takes place through the initial program made above.
## The only time it will store the inverse from cache is if 
## the given on the matrix did not differ from its 
## initial values.

cacheSolve <- function(x, ...) {
     invmat <- x$getinvmatsoln()
     if(!is.null(invmat)){
       message("retrieving cached data")
       return(invmat)
     }
     data <- x$get()
     invmat <- solve(data,...)
     x$setinvmatsoln(invmat)
     invmat
}
  
## program finish.
