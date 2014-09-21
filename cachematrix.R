## This set of functions allows user to create a matrix object that can cache its inverse
## and then retrieve the inverse of the same object directly from the cache instead of direct computations
## The algorithm is based on R lexical scoping rules, and hierarchical environments

## create a special Matrix object that can set the value of its inverse in its parent environment
makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) i <<- inverse
    getinverse <- function() i
    
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)

}


## calculate the inverse of a Matrix object by first looking for a cached value, and only if not found computing it
cacheSolve <- function(x, ...) {
       i <- x$getinverse()
       if(!is.null(i)) {
           message("getting cached data")
           return(i)
       }
       data <- x$get()
       i <- solve(data, ...)
       x$setinverse(i)
       i
}
