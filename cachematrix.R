## Put comments here that give an overall description of what your
## functions do

## Creates a matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        
        #Resets values without having to create a new makeCacheMatrix object
        set <- function(y) {
                x <<- y
                i <<- NULL 
        }
        
        #Obtains matrix value: X is not defined within get(), therefore obtains it from the parent environment, makeCache
        get <- function() x
        
        #Sets the value of inverse that will cache: from the inverse value in the global environment
        setInverse <- function(inverse) i <<- inverse
        
        #Returns the value of cached inverse:
        getInverse <- function() i

        #Naming the list elements is what allows us to use the $ form of the extract operator to access the functions
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}

#Required to populate and/or retrieve the Inverse from an object of makeCacheMatrix
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getInverse()
        #Checks to see if there is a value, if there is, we can use the cached value
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data)
        x$setInverse(i)
        i
}

