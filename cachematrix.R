## These two functions compute the inverse of a matrix and cache it

## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
	set <- function (y) {
		x<<- y
		inv <<- NULL
	}
	get  <- function () x
	setinvrs<- function (inverse) inv <<- inverse
	getinvrs<- function () inv
	list(set = set, get = get , setinvrs = setinvrs, getinvrs =getinvrs)
	

}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated 
and the matrix has not changed, then the cachesolve will retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        inv <- x$getinvrs()
	if (!is.null(inv)) {
		message("retrieving cached result")
		return(inv)
		}
		data <- x$get()	
		inv <- solve(data, ...)
		x$setinvrs(inv)
		inv	
}

