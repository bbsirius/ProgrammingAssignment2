## Overall, the below R script cache the matrix
## and its ivnerse matrix


## makeCacheMatrix creates a special vector
## containing a function to
## 1. get value of the matrix
## 2. set value of the matrix
## 3. get the value of the inverse matrix
## 4. solve the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y){
        x <<- y
        inv <<- NULL
    }
    get <- function()x
    setInverse <- function(inverse) inv <<- inverse
    getInverse <- function() inv
    list(set = set,
         get= get,
         getInverse = getInverse,
         setInverse = setInverse)
}


## the following function calculates the inverse
## of the special vector created above.

## if the inverse of the matrix already exists
## it is read from the cache directly.
## otherwise it calls sovle() to inverse the matrix
## and set it to the vector

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv <- x$getInverse()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setInverse(inv)
    inv
}
