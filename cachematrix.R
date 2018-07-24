## This function creates a special matrix that can cache its inverse
## If a matrix is not provided, then the speciamatrix created will be null
## The object created here is a list of functionalities; It can:
## 1- set the value of the matrix to be inverted with it's set function
## 2- retrive the value of the matrix to be inverted ith it's get function
## 3- It can set the inverse of the given matrix if there is one, using the setinvrs function
## 4- It catch  the inverse of the given matrix if there is one, using the getinvrs function 

makeCacheMatrix <- function(x = matrix()) {
  x_invrs <- NULL
  set <- function(y) {
    x <<- y
    x_invrs <<- NULL
  }
  get <- function() x
  setinvrs <- function(inv) x_invrs <<- inv
  getinvrs <- function() x_invrs
  list(set = set, get = get,
       setinvrs = setinvrs,
       getinvrs = getinvrs)
}


## The following function is given a special matrix created by the makeCacheMatrix fucntion as argument
## It then computes the Inverse of the matrix contained in the special matrix if it had not yet been computed
## If the inverse had already been computed (buy calling) the cacheSolve on the special matricx object,
## Then the value of this inverse is just returned

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  x_invrs <- x$getinvrs()
  if(!is.null(x_invrs)) {
    message("getting cached Inverse of the matrix")
    return(x_invrs)
  }
  data <- x$get()
  x_invrs <- solve(data, ...)
  x$setinvrs(x_invrs)
  x_invrs
  
}
