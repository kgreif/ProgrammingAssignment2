## makeCacheMatrix will accept a matrix as input and return a list of functions  that will
## 1) set the values of the matrix
## 2) get the values of the matrix
## 3) get the cached value of the input matrix
## 4) cache the values of the input matrix
## 5) cache the values of the inverse matrix
## 6) get the values of the inverse matrix
##
## CacheSolve will obtain the matrix from makeCacheMatrix and determine if
## it has changed.  If it is the initial matrix or if the matrix has changed,
## it will calculate the inverse of the matrix and call functions to cache
## the current value of the matrix and the inverse of the matrix.  If it has 
## not changed, it will return the value of the cache matrix and set new values
## for the input matrix.


## makeCacheMatrix will provide the initial matrix value to cacheSolve and cache
## the input matrix values and inverse matrix values for use by cacheSolve

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL 
        cachedinput <- x
        setinput <-function(y) {x <<-y}
        get <-function() x
        getcachedinput <- function() cachedinput
        cacheinput <- function(input) cachedinput <<- input
        cacheinvmat <- function(invmat) m <<- invmat
        getinvmat <- function() m
                list(get = get, cacheinvmat = cacheinvmat,
                     getinvmat = getinvmat, cacheinput = cacheinput,
                     getcachedinput = getcachedinput, setinput = setinput)

}

## cacheSolve will calculate the inverse of the input matrix if not already cached
## and if the input matrix has not chanced since the cache was created

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinvmat()
        data <- x$get()
        prevdata <- x$getcachedinput()
                if (!is.null(m) & identical(data,prevdata)== T) {
                message ("getting cached data")
                x$setinput(matrix(sample(1:100,4),2,2))
                return(m)
        }
        data <- x$get()
        m <- solve(data)
        x$cacheinvmat(m)
        x$cacheinput(data)
        m
        

}
