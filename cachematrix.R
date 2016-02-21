## These functions return the inverse of an invertible matrix
## They check if the inverse was already calculated, if so 
## they get it from cashe, otherwise calculate the inverse and store it in cache

## Function makeCacheMatrix(x), x is a matrix creates a special object containing
## a list of functions to 
## 1 set the value of the matrix
## 2 get the value of the matrix
## 3 set the value of the inverse
## 4 get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
        inverse_matrix <- NULL
        set <- function(y) {
                x <<- y
                inverse_matrix <<- NULL
        }
        get <- function() x
        setinverse <- function(inv) inverse_matrix <<- inv
        getinverse <- function() inverse_matrix
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## Function cacheSolve(x,...) x is a matrix, ... are other parameters of 
## the function solve 
## This function checks if the inverse was already calculated, if so 
## it gets it from cashe, otherwise calculates the inverse and stores it in cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverse_matrix <- x$getinverse()
        if(!is.null(inverse_matrix)) {
                message("getting cached data")
                return(inverse_matrix)
        }
        data <- x$get()
        inverse_matrix <- solve(data, ...)
        x$setinverse(inverse_matrix)
        inverse_matrix
}        
        



