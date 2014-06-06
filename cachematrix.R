## These functions are used to cache the inverse of a matrix

## This function caches a matrix inverse

makeCacheMatrix <- function(x = matrix()) {
	myInverse <- NULL
	
	set <- function(y) {
		x <<- y
		myInverse <<- NULL
	}
	
	get <- function() x
	
	setInverse <- function(inverse) myInverse <<- inverse
	
	getInverse <- function() myInverse
	
	list(
		set = set,
		get = get,
		setInverse = setInverse,
		getInverse = getInverse
	)
}


## This function returns the cached version of a matrix if one exists or it calculates the inverse of a matrix and returns it

cacheSolve <- function(x, ...) {
	myInverse <- x$getInverse()
	
	if(!is.null(myInverse)) {
		message("getting cached data")
	} else {
		data <- x$get()
		
		myInverse <- solve(data, ...)
		
		x$setInverse(myInverse)
	}
	
	myInverse
}
