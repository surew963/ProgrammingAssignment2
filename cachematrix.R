## a pair of functions that cache the inverse of a matrix

## makeCacheMatrix: This function creates a special "matrix" 
##object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(solve) m <<- solve
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## cacheSolve: This function computes the inverse of the 
##special "matrix" returned by makeCacheMatrix above. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  m
}

## test run

x <- matrix(c(1,1,1,3,4,3,3,3,4), nrow =3, ncol = 3)
print(x)
print('inverse:')
v1<-makeCacheMatrix(x)
v2<-cacheSolve(v1)
print(v2)
v3<-cacheSolve(v1)
print(v3)


