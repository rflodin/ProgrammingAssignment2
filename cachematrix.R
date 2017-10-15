## The makeCacheMatrix and cacheSolve functions allow a user to both create 
## a matrix, and then calculate the inverse of the matrix.  However, they save 
## on computation, by checking whether the inverse of the matrix has already
## been computed, and if so, then the cached value of the inverse is returned. 


## makeCacheMatrix based on the function makeVector provided in 
## "Example: Caching the Mean of a Vector" in the Coursera course
##  "R Programming" taught by Dr. Roger D. Peng.  
## (online link https://www.coursera.org/learn/r-programming/peer/tNy8H/programming-assignment-2-lexical-scoping)
## This function allows the creation of a matrix, but it also allows for the inverse of the matrix 
## to be stored in the cache once it has been computed.  It also allows for the inverse of the matrix to 
## be simply returned from the cache, instead of re-calculating, each time it is needed after the initial 
## calculation.  

makeCacheMatrix <- function(x = matrix()) {
    if(!is.matrix(x)){stop("x is not a matrix")}
    m <- NULL
    set <- function(y) {
      x <<- y
      print(x)
      m <<- NULL
      print(m)
    }
    get <- function() x
    setinv <- function(inv) m <<- inv
    getinv <- function() m
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## cacheSolve is based on the function cachemean provided in 
## "Example: Caching the Mean of a Vector" in the Coursera course
##  "R Programming" taught by Dr. Roger D. Peng.  
## (online link https://www.coursera.org/learn/r-programming/peer/tNy8H/programming-assignment-2-lexical-scoping)
## This function will either calculate and return the inverse of a matrix or it will  
## retrieve the inverse of the matrix from the cache if it has been previously computed.  

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  if(!any(names(x)=="getinv")){stop("x is not a matrix created with makeCacheMatrix")}  
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