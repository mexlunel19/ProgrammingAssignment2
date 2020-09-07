## Functions obtain the inverse of a matrix while searching the cache before
## computing the operation

## makeCacheMatrix creates the "special" matrix object, both the matrix and its
## inverse cache can be set by the user. It returns a list containing functions 
## to:
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse of the matrix
## 4. get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    inv_m <- NULL
    set <- function(y) {
      x <<- y
      inv_m <<- NULL
    }
    get <- function() {x}
    setinverse <- function(inverse) {inv_m <<- inverse}
    getinverse <- function() {inv_m}
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## cacheSolve returns the inverse of the "special" matrix from the above 
## function. It checks first, if the solution is in the cache. If so, then it 
## retrieves said solution instead of computing the value again. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv_m <- x$getinverse()
    if(!is.null(inv_m)) {
      message("getting cached data")
      return(inv_m)
    }
    data <- x$get()
    inv_m <- solve(data)
    x$setinverse(inv_m)
    inv_m
}
