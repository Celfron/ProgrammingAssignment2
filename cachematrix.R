## Program is set up to take an input square matrix, set and get values of the input matrix,
## and finally set and get the values of the inverse of the input matrix

## The makeCacheMatrix function sets up the set and 
## get commands for both the input matrix and it's inverse
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL #create null matrix
  
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinvs <- function(solve) m <<- solve
  getinvs <- function() m
  
  ## returns list of created functions.
  list(set = set, get = get,
       setinvs = setinvs,
       getinvs = getinvs)
}


## cacheSolve determines if the matrix has already been inverted,
## if so cacheSolve returns the already inverted matrix
## if not cacheSolve will calculate and return the inverse of the input matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinvs()
  
  #determines if inverse has already been cached
  if(!is.null(m)) {
    message("getting cached matrix")
    return(m)
  }
  
  #if inverse has not already been cached
  data <- x$get()
  m <- solve(data, ...)
  x$setinvs(m)
  m
}
