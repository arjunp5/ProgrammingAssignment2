## Writing two functions to cache  the inverse of the matrix


## This function is used to create the matrix object 

makeCacheMatrix <- function(x = matrix()) {
  mat_inverse <- NULL
  set <- function(y) {
    x <<- y
    mat_inverse <<- NULL
  }
  get <- function() x
  
  set_matrix_inverse <- function (inverse) mat_inverse <<- inverse
  get_matrix_inverse <- function() mat_inverse
  list(set = set, get = get, set_matrix_inverse = set_matrix_inverse,
       get_matrix_inverse = get_matrix_inverse)

}


## This function computes the inverse of the above matrix and caches it

cacheSolve <- function(x, ...) {
  mat_inverse <- x$get_matrix_inverse()
  if(!is.null(mat_inverse)) {
    message("Getting cached matrix data")
    return(mat_inverse)
  }
  data <- x$get()
  mat_inverse <- solve(data, ...)
  x$set_matrix_inverse(mat_inverse)
  mat_inverse
  
        ## Return a matrix that is the inverse of 'x'
}
