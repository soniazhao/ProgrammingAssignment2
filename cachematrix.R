## The combination of functions "makeCacheMatrix" and "cacheSolve" 
## allows user to cache the inverse of a matrix and retrieve the 
## inverse from the cache. If no value is cached, "cacheSolve" will 
## calculate the inverse matrix and store the value as cached matrix 
## for further retrieve in order to avoid repeated calculation. 

## "makeCacheMatrix" generates a list of four elements:
## "get" returns the input value 
## "set" allows to input new matrix
## "get_inv_matrix" returns the cached value
## "set_inv_matrix" assigns new cached value

makeCacheMatrix <- function(x = matrix()) {
        inv_matrix<-NULL
        set <- function (y) {
            x <<- y
            inv_matrix <<- NULL
        }
        get <- function () x
        set_inv_matrix <- function (solve) inv_matrix <<- solve
        get_inv_matrix <- function () inv_matrix
        list (set = set, get = get,
              set_inv_matrix = set_inv_matrix,
              get_inv_matrix = get_inv_matrix)
}

## "cacheSolve" calculates the inverse matrix of a new input matrix or 
## retrieves the cached inverse matrix stored in "makeCacheMatrix". 

cacheSolve <- function(x, ...) {
    inv_matrix <- x$get_inv_matrix ()
    if(!is.null(inv_matrix)) {
        message("getting cached data")
        return(inv_matrix)
    }
    value <- x$get()
    inv_matrix <- solve(value, ...)
    x$set_inv_matrix(inv_matrix)
    inv_matrix
}

