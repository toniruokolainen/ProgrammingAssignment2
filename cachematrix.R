####################################################################
## Defines two functions for cached inverse matrix calculation.
##
## 	makeCacheMatrix: creates a special "matrix" object that can
## 		cache its invers.
## 	cacheSolve: computes the inverse of the special matrix.
##
####################################################################

## Creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL

	# Setter for the matrix.
	setmatrix <- function(y) {
		x <<- y
		inv <<- NULL
	}
	
	# Getter for the matrix.
	getmatrix <- function() x
	
	# Setter for the inverse matrix.
	setinverse <- function(im) inv <<- im
	
	# Getter for the inverse matrix.
	getinverse <- function() inv
	list(set = setmatrix, 
		get = getmatrix, 
		setinverse = setinverse,
		getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has already 
## been calculated (and the matrix has not changed), then the cachesolve
## should retrieve the inverse from the cache.
cacheSolve <- function(x = matrix(), ...) {

        # Return a matrix that is the inverse of 'x'
		m <- x$getinverse()
		
		# If inverse has been calculated then return the cached value.
		if(!is.null(m)) {
			message("getting cached data")
			return(m)
		}
		
		# We are here iff the inverse matrix was NULL.
		# Get the matrix, calculate the inverse and save the 
		# inverse value to the cache.
		data <- x$get()
		m <- solve(data, ...)
		x$setinverse(m)
		
		# Return the inverse matrix.
		m
}
