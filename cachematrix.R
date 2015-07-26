## As matrix inversion is usually a costly operation, the two functions below can together be used to cache the inverse of a matrix instead of calculating it repeatedly.
## Here, we assume that the input matrix is actually invertible.


## The first function is the makeCacheMatrix function. The makeCacheMatrix function takes as input a matrix and returns a list of four elements, each element being a different function.
## The elements of this list can subsequently be used in the second function (the cacheSolve function) in order to cache the inverse of the input matrix.

makeCacheMatrix <- function(x = matrix()) {             
        s <- NULL                                       ## makeCacheMatrix first sets the solve value s to NULL.
        set <- function(y) {                            ## makeCacheMatrix then creates the first element of the list: a function that can simply set the value of the matrix y.
                x <<- y
                s <<- NULL                      
        }
        get <- function() x                             ## makeCacheMatrix then creates the second element of the list: a function that can get the value of the matrix (i.e. x).
        setsolve <- function(solve) s <<- solve         ## makeCacheMatrix then creates the third element of the list:  a function that can set the inverse of the matrix (i.e. s) to value ''solve''.
        getsolve <- function() s                        ## makeCacheMatrix then creates the fourth element of the list: a function that can get the inverse of the matrix (i.e. s).
        list(set = set, get = get,                      ## The makeCacheMatrix function finally returns a list containing the above four functions as its elements.
             setsolve = setsolve,
             getsolve = getsolve)
}


## The cacheSolve function takes as input a list as created with the makeCacheMatrix function, and returns the inverse of the matrix that was used as input for the makeCacheMatrix function.

cacheSolve <- function(x, ...) {                        ## If you give this function as input a list which was created with the makeCacheMatrix function, 
        s <- x$getsolve()                        
        if(!is.null(s)) {                               ## it returns the inverse if it has already been cached (i.e. if s is not equal to NULL),
                message("getting cached data")          ## where it displays a message to show that the cached data is being collected.
                return(s)
        }
        data <- x$get()                                 ## If the inverse has not yet been calculated before (or if it has not yet been cached), 
        s <- solve(data, ...)                           ## cacheSolve calculates the inverse of the matrix x (that was used as input for makeCacheMatrix) 
        x$setsolve(s)                                   ## caches it (i.e. sets s to solve(x)),
        s                                               ## and returns the inverse matrix of x.
}