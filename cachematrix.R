## The functions in this script are used to create special obect that can store and cache its inverse.

## The function creates a special "matrix" object which can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
	set <- function(y) {
		x <<- y
		inv <<- NULL
	}
	get <- function() x
	setInverse <- function(inverse) inv <<- inverse
	getInverse <- function() inv
	list(set = set, get= get, 
		setInverse = setInverse,
		getInverse = getInverse)
}


## The following calculates the inverse of the special "matrix" created with the above function.

cacheSolve <- function(x, ...) {
	inv <- x$getInverse()
	if(!is.null(inv)) {
		message("getting cached data")
		return(inv)
	}
	mat <- x$get()
	inv <- solve(mat, ...)
	x$setInverse(inv)
	return(inv)
}

## Example
## x <- matrix(rnorm(9),3,3)
## m <- makeCacheMatrix(x)
## cacheSolve(m)
