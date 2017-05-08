## the following functions "makeCacheMatrix" and "cacheSolve" can compute and
## cache the inverse of a matrix.

## makeCacheMatrix is a function which creates a special "matrix" object that
## can cache its inverse.

        makeCacheMatrix <- function(x = matrix()) {
        
                inv = NULL
                set = function(y) {
                        x <<- y
                        inv <<- NULL
                }
                get = function() x
                setinv = function(inverse) inv <<- inverse 
                getinv = function() inv
                list(set = set, get = get, setinv = setinv, getinv = getinv)
                }

## cacheSolve is a function which computes the inverse of the special "matrix"
## returned by makeCacheMatrix above. If the inverse has already been calculated
## (and the matrix has not changed), then the cachesolve should retrieve the
## inverse from the cache.

        cacheSolve <- function(x, ...) {
                
                inv = x$getinv()
                
               
                if (!is.null(inv)){
                        
                        message("cached data results")
                        return(inv)
                }
                
                
                data = x$get()
                inv = solve(data, ...)
                
                
                x$setinv(inv)
                
                return(inv)
                }
        
## checking
        m <- matrix(c(1, 2, 3, 4),2,2)
        m1 <- makeCacheMatrix(m)
                cacheSolve(m1)
                cacheSolve(m1)