## coursera R_programming project 2
## There's an example of caching the mean of a vector,
## which is confusing because there's no working example to see how it works
## thumbs down to that
## in this assignment, you do the same thing, but make the pair of functions
## cache the inverse of a matrix (whatever that is)
## instead of computing the mean of a vector

# function that can cache the inverse
# This function makes a vector, which is really a list of 4 functions
makeCacheMatrix <- function( m = matrix() ) {
  # init
  toReturn = list()
  i <- NULL # inverse
  
  # FUN1:  set the matrix
  set <- function( matrix ) { 
    m <<- matrix
    i <<- NULL
  }
  # FUN2:  get the value of a vector
  get <- function(){ m } 
  # FUN3: set the value of the inverse of the matrix
  set.inverse <- function( inverse ){ i <<- inverse } 
  # FUN4: get value of the mean
  get.inverse <- function(){ i } 
  
  # finalize
  toReturn[[ "set" ]] = set
  toReturn[[ "get" ]] = get
  toReturn[[ "set.inverse" ]] = set.inverse
  toReturn[[ "get.inverse" ]] = get.inverse
  
  return(toReturn) }


## Return a matrix that is the inverse of 'x'
cacheSolve <- function(x = makeCacheMatrix , ...) {
  
  m <- x$get.inverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  # calculate the inverse using matrix multiplication
  
  m <- solve(data)  %*% data
  x$set.inverse(m)
  
  return(m)
}