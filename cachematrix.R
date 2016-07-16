## Matrix inversion is usually a costly computation and there 
##may be some benefit to caching the inverse of a matrix rather 
##than compute it repeatedly

## creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
	##Set The m to NULL, which will cache the inverse matrix
	m <- NULL
	
	##To init the matrix to be inversed and variable m
	set <- function(y) {
		x <<- y
		m <<- NULL
	}
	get <- function() x
	setInverse <- function(inverse) m <<- inverse
	getInverse <- function() m
	
	##generate a list contains four functions
	list(set = set, get = get,
		setInverse = setInverse,
		getInverse = getInverse)
}


## computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
##If the inverse has already been calculated (and the matrix has not changed), then
##the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
        	m <- x$getInverse()
	if (!is.null(m)) {
		message("gettting cached data")
		return (m)
	}
	data <- x$get()
	m <- solve(data)
	x$setInverse(m)
	m
}
