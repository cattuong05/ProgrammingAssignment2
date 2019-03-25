## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix creates a special matrix that is a list which includes
## functions to 
##   set (save) the matrix
##   get (retrieve) the matrix
##   set (save) the inverse of the matrix
##   get (retrieve) the inverse of the matrix
## the function returns a list which includes above functions
## 
## Note: due to lexical scoping, 
## when makeCacheMatrix returns an object that contains functions to its
## parent environment, the object in its parent environment have access to the
## functions and its environment--including data objects referenced by its 
## function: x, inv_matrix. 
## ** [See explanation by lgreski "Demystifying makeVector()"]

makeCacheMatrix <- function(x = matrix()) {
     # Initialize/reset inv_matrix for later use
     inv_matrix <- NULL

     # set matrix for parent x and reset parent inv_matrix
     set <- function (y) {
          # Assign input arg to x object in parent environment
          x <<- y
          # Reset/clear any previously used inv_matrix in parent environment
          inv_matrix <<- NULL
     }
     
     # returns matrix
     get <- function() x
     
     # set inverse matrix to parent level inv_matrix
     set_inverse <- function (invM) {
          inv_matrix <<- invM
     }
     
     # get inverse matrix
     get_inverse <- function() inv_matrix
     
     # put functions above into a list to return
     list(set = set, get = get, 
          set_inverse = set_inverse, 
          get_inverse = get_inverse)
}


## cacheSolve calculates the inverse of the matrix created with makeCacheMatrix above.
## It improves efficiency by first checking to see whether the inverse was previously
## calculated. If so, it returned the stored inverse matrix instead of re-calculating it.

cacheSolve <- function(x, ...) {
     ## x is a special matrix created by makeCacheMatrix above   
     ## cacheSolve returns a matrix that is the inverse of 'x'
     
     # get inv_matrix
     inv_matrix <-x$get_inverse()
     
     # if inv_matrix is not NULL, then return the cached inverse matrix
     if (!is.null(inv_matrix)) {
          message("getting cached inverse matrix")
          return (inv_matrix)
     }
     
     # inv_matrix is NULL here so
     # get matrix x to calculate its inverse by R's solve()
     mx <- x$get()
     inv_matrix <- solve(mx, ...)
     
     # save inverse matrix 
     x$set_inverse(inv_matrix)
     
     # return inverse matrix
     inv_matrix
}
