## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

m## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) inv <<- inverse
        getInverse <- function() inv
        list(set = set,
             get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## This function computes the inverse of the special "matrix" created by 
## makeCacheMatrix above. If the inverse has already been calculated (and the 
## matrix has not changed), then it should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getInverse()
        if (!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        mat <- x$get()
        inv <- solve(mat, ...)
        x$setInverse(inv)
        inv
}


TRYING TO TEST MY FUNCTIONS

> source("ProgrammingAssignment2/cachematrix.R")
> my_matrix <- makeCacheMatrix(matrix(1:6, 3, 3))

> my_matrix$get()
> my_matrix$getInverse()
NULL
> cacheSolve(my_matrix)

> cacheSolve(my_matrix)
getting cached data

> my_matrix$getInverse()

> my_matrix$set(matrix(c(4, 1, 3, 6), 2, 2))
> my_matrix$get()

> my_matrix$getInverse()
NULL
> cacheSolve(my_matrix)

> cacheSolve(my_matrix)
getting cached data

> my_matrix$getInverse()
