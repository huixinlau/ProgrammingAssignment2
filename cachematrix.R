## The makeCacheMatrix function takes a matrix 'x' and returns a list of functions 
## (List: 1. set the matrix, 2. get the matrix, 3. set the inverse and 
## 4. get the inverse)

## The cacheSolve function uses the functions in said list above to 1. retrieve 
## the inverse stored in cache or 2. calculate the inverse and then store it as 
## the new value in cache


## The makeCacheMatrix function creates an object 'inv' that lets us cache the
## inverse of 'x'

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL ## create cache object
  setmatrix <- function(y) {
    x <<- y             ## set the value of x
    inv <<- NULL        ## set the value of inv to NULL
  }
  getmatrix <- function() {x}                        ## returns x 
  setinverse <- function(inverse) {inv <<- inverse}  ## assigns the inverse matrix to inv 
  getinverse <- function() {inv}                     ## returns inv
  list(setmatrix = setmatrix, getmatrix = getmatrix, ## functions are named and contained in a list
       setinverse = setinverse,
       getinverse = getinverse)
}


## The cacheSolve function returns the inverse of 'x'

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()           ## Checks if there is an inverse matrix stored in inv 
  if(!is.null(inv)) {
    message("getting cached data")## If there is, the message is printed and
    return(inv)                   ## value of inv is returned
  }
  data <- x$getmatrix()   ## If inv is NULL, the matrix is retrieved from x
  inv <- solve(data, ...) ## and the inverse is calculated
  x$setinverse(inv)       ## Set the value of inv in the cache
  inv
}
