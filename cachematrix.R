## The following functions are used to cache the inverse of a matrix
## so its solve calculations are done only on the first call
## on second or more calls the results are returned from cache
## and not calculated again.
## 
## sample usage: 
## x1<-matrix(c(1,-1,1,-1,2,1,-1,3,4),nrow = 3,ncol = 3)
## y<-makeCacheMatrix(x1)
## cacheSolve(y)
## cacheSolve(y)
##


## The first function, makeCacheMatrix creates a special "vector", which is really a list containing a function to
##1. set the value of the matrix
##2. get the value of the matrix
##3. set the value of the inverse matrix
##4. get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {

    m<-NULL
    set <- function(y){
        x<<-y
        m<<- NULL
    }
    get <- function() x
    setinv <- function(solve) m <<- solve
    getinv <- function() m
    list(set = set, get=get,
         setinv = setinv,
         getinv = getinv)
}


## The following function calculates the inverse matrix of the
## special "vector" created with the above function.
## However, it first checks to see if the inverse matrix has
## already been calculated. If so, it gets the inverse matrix
## from the cache and skips the computation. Otherwise, it
## calculates the inverse matrix of the data and sets the value
## of the inverted matrix in the cache via the setmean function.
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
