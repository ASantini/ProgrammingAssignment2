## The cachematric.R file provides the functionality to store the inverse of a 
## matrix in memory for quick and easy retrieval rather than calculating the 
## inverse on the fly when needed


## makeCacheMatrix: This function takes a matrix as an arguement and stores 
## the matrix in memory. In addtion it provides getters and setters for the
## matrix and it's inverse.

makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
	set <- function(y) {
		x <<- y
		inv <<- NULL
	}
	get <- function () x
	setInverse <- function(inverse) inv <<- inverse 
	getInverse <- function() inv
	list(set = set, get = get, 
			setInverse = setInverse,
			getInverse = getInverse) 
}


## cacheSolve: This function takes a matrix and returns it's inverse. If the 
## inverse of the matrix already exists in memory, the inverse is returned. 
## If the inverse has not been previously cached, then the inverse is
## calculated and saved prior to returning the inverse.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getInverse()
    if(!is.null(m)) {
    	message("getting cached data")
    	return(m)
    }
    data <- x$get()
    m <- solve(data)
    x$setInverse(m)
    m
}
