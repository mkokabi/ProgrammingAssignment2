## The makeCacheMatrix function creates a list from a matrix
## Sample: AA <- makeCacheMatrix(A)
## 
## This list will be used by the next function cacheSolve

# the input is a matrix
makeCacheMatrix <- function(x = matrix()) {
	# inverse as an internal field contains the inversed matrix
	inverse <- NULL
	
	# the setter will set the matrix and reset the inverse field back to null
	set <- function(newMatrix) {
		x <<- newMatrix
		inverse <<- NULL
	}
	
	# the getter return the matrix
	get <- function() x
	
	# the setter of inversed matrix
	setInverse <- function(solve) inverse <<- solve
	
	# the getter of inversed matrix
	getInverse <- function() inverse
	
	# the list of the above functions
	list (set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## This function solve the matrix only if it's not in the cache. Otherwise, it will 
## return the cached values. It only works with the result of above function.
##
## Sample: cacheSolve(x = AA)

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x' 
	# it uses the getInverse element of the passed list to get the inversed matrix.
	# this element itself is pointing to a function with the same name.
	inverse <- x$getInverse()
	
	# if the inverse is not null then inform of using cache
	if (!is.null(inverse)) {
		message("getting cached data")
		# just return the inverse. No need to continue the solving
		return(inverse)
	}
	
	# if we are here it means there was nothing in the cache. Start with getting the data using 
	# the get element in the passed list which is pointing to the get function.
	data <- x$get()
	# then use R solve function using the data (matrix) and other passed arguments in ... if any 
	inverse <- solve(data, ...)
	# now set the result back to the cache. Again use the setInverse element in the x list
	# which is pointing to setInverse function.
	x$setInverse(inverse)
	# and finally return the result.
	inverse
}
