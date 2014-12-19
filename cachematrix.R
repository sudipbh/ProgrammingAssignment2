###################################################################
# makeCacheMatrix: This function creates a special "matrix" object 
#                  that can cache its inverse.
###################################################################

makeCacheMatrix <- function(x = matrix()) {

    invM <- NULL
    
    get <- function() { x }
    
    set <- function(newMatrix) {
        x <<- newMatrix
        invM <<- NULL
    }
    
    getInverse <- function() { invM }
    
    setInverse <- function(invMatrix) {
        invM <<- invMatrix
    }
    
    list(set = set,
         get = get,
         getInverse = getInverse,
         setInverse = setInverse
    )

}


###################################################################
# cacheSolve: This function computes the inverse of the special "matrix"
#             returned by makeCacheMatrix above. If the inverse has 
#             already been calculated (and the matrix has not changed), 
#             then cachesolve retrieves the inverse from the cache.
###################################################################

cacheSolve <- function(x, ...) {
    rv <- x$getInverse()
    if (!is.null(rv)) {
        print("returning cached inverse")
        return(rv)
    }
    m <- x$get()
    rv <- solve(m)
    x$setInverse(rv)
    rv
}
