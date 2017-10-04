## Function to define a matrix capable of caching its own inverse,
## and function to calculate the inverse--retrieving from cached 
## data if possible.

## Creates a special "matrix" object which is really a list
## of four functions which:
## (1) Set the value of the matrix
## (2) Get the value of the matrix
## (3) Set the value of the inverse
## (4) Get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y){
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## Calculates the inverse of the matrix object defined above,
## retrieving from cached data if possible.

cacheSolve <- function(x, ...) {
        inv <- x$getinverse()
        if(!is.null(inv)){
            message("getting cached data")
            return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv
}
