## This function caches the matrix passed as an input parameter and exposes functions to get and set the input matrix.

makeCacheMatrix <- function(x = matrix()) {
        #matrix m is set to null
        m <- NULL
        
        #setter 
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        
        #getter
        get <- function() x
        
        #set a matrix
        setmatrix <- function(matrix) m <<- matrix
        
        #get a matrix
        getmatrix <- function() m
        
        #makeCachematrix function is a list of the following four functions 
        list(set = set, get = get,setmatrix = setmatrix,getmatrix = getmatrix)
}


## This function Computes the Inverse of the matrix if the inverse is not cached
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        m <- x$getmatrix()
        #Checks if the matrix is cached and if the cache is not null it returns the cached inverse
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        
        #matrix inverse function
        m <- solve(data, ...)
        x$setmatrix(m)
        m
}
