##This function creates a special “matrix” object that can cache its inverse

makeCacheMatrix <- function( m = matrix() ) {
    i <- NULL
    ## Set the matrix
    set <- function(matrix) {
        m <<- matrix
        i <<- NULL
    }
    ## Get the matrix
    get <- function() {
        m
    }
    setInverse <- function(inverse) {
        i <<- inverse
    }
    getInverse <- function() {
        i
    }
    ## List of methods to be used with matrix
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}

##This function computes the inverse of the special “matrix” 
##returned by makeCacheMatrix abov

cacheSolve <- function(x, ...) {
    m <- x$getInverse()
    
    if( !is.null(m) ) {
        message("Getting Cached Data...")
        return(m)
    }
    
    data <- x$get()
    m <- solve(data) %*% data
    x$setInverse(m)
    m
}
