## The assignment is to Write the following functions:
##      1. makeCacheMatrix: This function creates a special "matrix" object that 
##              can cache its inverse.
##      2. cacheSolve: This function computes the inverse of the special 
##              "matrix" returned by makeCacheMatrix above. If the inverse has 
##              already been calculated (and the matrix has not changed), then 
##              the cachesolve should retrieve the inverse from the cache.

## The function makeCacheMatrix creates a list containing functions to:
##      1. Set the value of the matrix
##      2. Get the value of the matrix
##      3. Set the value of the inverse of the matrix
##      4. Get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        s <- NULL
        set <- function(y) {                            ## Function to set a new matrix and reset cache
                x <<- y
                s <<- NULL
        }
        get <- function() x                             ## Function to get the value of a stored matrix
        setsolve <- function(solve) s <<- solve         ## Function to save a solution
        getsolve <- function() s                        ## Function to retrieve a previously saved solution
        list(set = set,
             get = get, 
             setsolve = setsolve, 
             getsolve = getsolve)                       ## Creates list with functions. 

}

## The function cacheSolve first, attempted to retrieve the inverse of the 
## matrix from $getsolve. If it exists, the function returns the inverse. If
## it does not exist, then it calculates the inverse, and stores it with 
## $setsolve and returns the inverse matrix.

cacheSolve <- function(x, ...) {
        s <- x$getsolve()       ## Checks if pre-existing solution exists
        if(!is.null(s)) {       ## If yes, returns the solution
                message("getting cached data")
                return(s)
        }
        data <- x$get()         
        s <- solve(data, ...)   ## Else, calculate the solution
        x$setsolve(s)           ## And saves it in object s 
        s                       ## Return a matrix that is the inverse of 'x'
}
