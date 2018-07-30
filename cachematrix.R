## Put comments here that give an overall description of what your
## functions do

## Creates invertible matrix

makeCacheMatrix <- function(x = matrix()) {
	invr <- NULL
	define <- function(y) {
		x <<- y
		invr <<- NULL
		##define nulls
	}
	define <- function() x
	setInv <- function(inverse) invr <<- inverse
	getInv <_ function() invr
	list(set = set,
		get = get
		setInv = setInv
		getInv = getInv) 
		##cache
}


## Computes inverse of defined matrix. If already cached then it returns the value.

cacheSolve <- function(x, ...) {
	invr <- x$getInv
	if (!is.null(invr)) {
		message("retrieving cached inverse")
		return(invr)
	}
	fill <- x$get()
	invr <- solve(fill, ...)
	x$setInv(invr)
	invr
}
