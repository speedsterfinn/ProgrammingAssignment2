## These two functions allow the user to cache the
## inverse of an invertible matrix.

## Function makeCacheMatrix returns an object
## that will hold an invertible matrix, mat, and
## its inverse, inv.

makeCacheMatrix <- function(M = matrix()) {
	inv <- NULL
	set <- function(m) {
		M <<- m
		inv <<- NULL
	}
	get <- function() M
	setinverse <- function(inverse) inv <- inverse
	getinverse <- function() inv
	list(set = set, get = get,
	     setinverse = setinverse,
	     getinverse = getinverse)
}


## Function cacheSolve accepts a makeCacheMatrix
## object as input and returns the inverse of
## the matrix stored in makeCacheMatrix

cacheSolve <- function(M, ...) {
## Return a matrix that is the inverse of 'M'
	inv <- M$getinverse()
	if (!is.null(inv)) {
		return(inv)
	}
	mat <- M$get()
	inv <- solve(mat)
	M$setinverse(inv)
	inv
}

