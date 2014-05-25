## Put comments here that give an overall description of what your
## functions do

## This function creates a list containing 4 different functions, and receives as argument a matrix. First, if you run
## this function without giving a matrix, it creates an empty matrix. So, you can after set the value of the matrix with the function
# set, making x@set(matrix). The get function returns the matrix that has been stored by "set" function or that was given at the begining to the global
# function. "setinverse" is a function that sets a value to the inverse of the matrix storing this value in "m", and finally getinverse returns this value.
#The function makeCacheMatrix returns finally a list that contains this 4 functions, asigns to x a matrix (making a globalvariable) (empty or not) and
# assigns NULL to m (as global variable).

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(inv) m <<- inv
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## This function gets a matrix "or list really) that is created by the function above. It gets its inverse by running the function
# getinverse, and if this value isnt null, its because the value has already been set, so the function does nothing. If not,
# the function calculates the inverse and stores its value in "m" by running the function "solve" for calculating this inverse and then "setinverse"
#for storing the value.

cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}
