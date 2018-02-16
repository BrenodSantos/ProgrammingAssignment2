## The gist of this code is to optimize computing by accessing pre-calculated 
## values for the inverse of a matrix (which is a high consuming operation)
## It receives an invertible matrix and calculate its inverse whether it's not
## on cache yet. Otherwise, access it from the cache.

## This function creates a list with the functions to set the matrix, get the 
## matrix, set its inverse and get its inverse

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setInverseMatrix <- function(inverse) i <<- inverse
        getInverseMatrix <- function() i
        list(set = set,
             get = get,
             setInverseMatrix = setInverseMatrix,
             getInverseMatrix = getInverseMatrix)
}


## This function will check whether there are any inverse matrix cached yet.
## If so, it will check whether it is the inverse of the current matrix.
## If so, it will access cache and return the inversed matrix cached, otherwise
## it will recalculate and return the inverse of the new matrix

cacheSolve <- function(x, ...) {
	## Return a matrix that is the inverse of 'x'
	
        i <- x$getInverseMatrix()
        if (!is.null(i)) {
                message("accessing cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setInverseMatrix(i)
        i
}
