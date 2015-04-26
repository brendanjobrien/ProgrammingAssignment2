## Create a square matrix during the call of makeCacheMatrix()
## Ex/ a <- makeCacheMatrix(1:4,2,2)
## 	 cacheSolve(a)
##	 Output:
##     		[,1] [,2]
##		[1,]   -2  1.5
##		[2,]    1 -0.5

## Function
##	Sets matrix
##	Gets matrix
##	Sets inverse matrix
##	Gets inverse matrix

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinverse <- function(inv) i <<- inv
    getinverse <- function() i
    list(
        set = set,
        get = get,
        setinverse = setinverse,
        getinverse = getinverse
    )
}


## Calculate the inverse of the matrix created with the above
## function, reusing cached result if it is available

cacheSolve <- function(x, ...) {
    i <- x$getinverse()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    m <- x$get()
    i <- solve(m, ...)
    x$setinverse(i)
    i
}