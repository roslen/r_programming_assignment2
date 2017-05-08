# Programming Assignment #2 for R programming
# Roslen Anacleto
# 9 May 2017
#
# The following solution was patterned after the sample code given in the
# programming assignment.

# Create a cached matrix containing four set functions.
makeCacheMatrix <- function(x = matrix()) {
   matrix_inverse <- NULL

   # Function to set the value of x
   set <- function(y) {
      x <<- y
      matrix_inverse <<- NULL
   }

   # Function to retrieve the value of x
   get <- function() x

   # Function to set the inverse of the matrix
   setInverse <- function(inverse) matrix_inverse <<- inverse

   # Function to get the inverse of the matrix
   getInverse <- function() matrix_inverse

   # Organize into a list; Return value
   list(set = set,
        get = get,
        setInverse = setInverse,
        getInverse = getInverse)
}


## This function computes the inverse of the special "matrix" created by
## makeCacheMatrix above. If the inverse has already been calculated (and the
## matrix has not changed), then it should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
   # Get the inverse of the parameter x
   matrix_inverse <- x$getInverse()

   # Check if matrix_inverse has previously been assigned a value.
   if (!is.null(matrix_inverse)) {
      message("Retrieve cached data...")
      return(matrix_inverse)
   }

   # Retrieve the value of matrix x and then solve for its inverse.
   mat <- x$get()

   # Solve for its inverse
   matrix_inverse <- solve(mat, ...)

   # Cache this value to the variable specified in the parameter.
   x$setInverse(matrix_inverse)

   # Return the inverse
   matrix_inverse
}

