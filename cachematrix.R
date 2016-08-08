## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

##
## This function takes a matrix as argument and create an 'object'
## with some predefind functions. Those functions are the interface 
## to handle the assignment, calculation and to serve the inverse
## of the original matrix. Thos object is used by the cacheSolve function.
##
makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list (set = set, 
                get = get, 
                setinverse = setinverse, 
                getinverse = getinverse)
}


## This function store the inverse of the matrix. If the matrix has not changed
## the function returns the previously calculated inverse of the matrix.
## this functionality saves time and process

## basically the cahceSolve take an argument of the type makeCacheMatrix to
## solve the inverse of the matrix every time that the user needs the inverse
## of the matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("inverting matrix")
                return()
        }
        data <- x$get()
        inv <- solve(data)
        x$setinverse(inv)
        inv
}
