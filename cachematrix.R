## The below code creates a function that creates the inverse for a matrix and stores it in a cache. 
## If the same matrix is used to find the inverse of, it will call a previously stored answer

## Creates a list to obtain whether the matrix has been cached or not

makeCacheMatrix <- function(x = matrix()) {
        invm <- NULL
        set <- function(y) {
                x<<- y
                invm <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) invm <<- solve
        getinverse <- function() invm
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Gets the cached inverse solution or solves it if not cached

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        invm <- x$getinverse()
        if(!is.null(invm)){
                message ("getting cached data")
                return(invm)
        }
        data <- x$get()
        invm <- solve(data,...)
        x$setinverse(invm)
        invm
}