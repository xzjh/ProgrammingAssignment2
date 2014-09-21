## Put comments here that give an overall description of what your
## functions do

## The first functionm makeCacheMatrix creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {

    # set the stored inverse value to NULL
	s <- NULL
   
	  # to set the value of the matrix
	set <- function(y) {
		x <<- y
		s <<- NULL
	}
   
	# to get the value of the matrix
	get <- function() x
	
	# to set the value of the inverse
	setinverse <- function(solve) s <<- solve

	# to get the value of the inverse	 
	getinverse <- function() s

	# return a list with the functions
	list(set = set, get = get,
		setinverse = setinverse,
		getinverse = getinverse)
}


## The following function calculates the inverse of the special "matrix"
## created with the above function. However, it first checks to see if the
## inverse has already been calculated. If so, it `get`s the inverse from the
## cache and skips the computation. Otherwise, it calculates the inverse of
## the data and sets the value of the inverse in the cache via the `setinverse`
## function.

cacheSolve <- function(x, ...) {
	## Return a matrix that is the inverse of 'x'

	# check the inverse is already cached
	s <- x$getinverse()

	if(!is.null(s)) {
		message("getting cached data")
		return(s)
	}
	#not cached and hence get the matrix into the variable
	data <- x$get()

	# calcuate the inverse
	s <- solve(data, ...)

	# set the cache
	x$setinverse(s)

	# return the inverse
	s
}
