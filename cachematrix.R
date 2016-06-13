## cachematrix.R
## The functions below aid in computing and caching the inverse of a matrix
## This assumes the input matrix is always invertible


## Function makeCacheMatrix returns a special "matrix" or list of functions to:
## set/get the values of a matrix and set/get the inverse of the matrix x

makeCacheMatrix <- function(x = matrix()) {
	i <- NULL
	set <- function(y) {
		x <<- y
		i <<- NULL
	}
	get <- function() x
	setinv <- function(inv) i <<- inv
	getinv <- function() i
	list(set = set,
		get = get, 
		setinv = setinv, 
		getinv = getinv)
}



## Function cacheSolve computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## The inverse is retrieved from cache if it exists, otherwise it is cached for later use

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
		i <- x$getinv()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data)
        x$setinv(i)
        i
}
