
## MakeCacheMatrix and cachesolve together create a special object that caches a matrix and its inverse. 
## MakeCacheMatrix takes an invertible matrix as an input and generates an output that is a special "vector", which is a list containing four functions to perform the following tasks - 
##(a) set the value of the input matrix and cache it.
##(b) get the value of the cached matrix.
##(c) set the value of the inverse matrix, i.e., cache it.
##(d) get the cached value of the inverse matrix.
##Note that the input matrix can be specified in two ways (a)as a parameter of 'makeCacheMatrix', or (b) using the 'set' function. 

makeCacheMatrix <- function(Mat = matrix()) {
        inv <- NULL
        set <- function(y) {
                Mat <<- y
                inv <<- NULL
        }
        ## initialize cached values of the matrix and the inverse
        get <- function() Mat
        setinverse <- function(solve) inv <<- solve
        ## cache the inverse matrix 
        getinverse <- function() inv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## cacheSolve takes as input a special vector, V, created using makeCacheMatrix. The output is the inverse of the last cached input matrix associated with V. When the function is called the first time the inverse is computed. Thereafter, if the inverse of the same matrix is requested again the cached value is provided as the output.     


cacheSolve <- function(x, ...) {
        inv <- x$getinverse()
        if(!is.null(inv)) {   
                ## inverse is in the cache
                message("getting cached data")
                return(inv)
        } else {           
                ## inverse is not in the cache 
                data <- x$get()
                inv <- solve(data, ...)    
                ## Compute the inverse
                x$setinverse(inv) 
                inv
                ## Return a matrix that is the inverse of 'x'
        }
}