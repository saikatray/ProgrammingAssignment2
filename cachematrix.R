## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
	matxinv <- NULL
    
	set <- function(y) {
        x <<- y
        matxinv <<- NULL
    }
    get <- function() x
    
	setinverse <- function(solve) matxinv <<- solve
    getinverse <- function() matxinv
    
	list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
	matxinv <- x$getinverse()
		
	if(!is.null(matxinv)) {
		message("getting cached data")
		return(matxinv)
	}
		
	data <- x$get()
	matxinv <- solve(data, ...)
	x$setinverse(matxinv)
	
	matxinv
}
