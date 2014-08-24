## 
## Matrix inversion is usually a costly computation and there is benefit to caching the
## inverse of a matrix rather than computing it repeatedly. If the contents of the matrix
## are not changing, it may makes sense to cache the inverse of the matrix so that when we need
## it again, it can be looked up in the cache rather than recomputed.
## The following pair of functions build a special matrix that can cache its inverse.
##
## Assumption: Functions assume the matrix supplied is always invertible.
## Usage:
##      myMatrixCache <- makeCacheMatrix(myMatrix)
##      myMatrixInv <- cacheSolve(myMatrixCache)
##      Note:   myMatrixInv will be the calculated the 1st time and cached,
##              subsequently the cached value of the inverse will be returned
           

## `makeCacheMatrix`: This function creates a special "matrix" object that can cache its inverse.
## `makeCacheMatrix` creates a special "matrix", which is really a list containing a function to
##
## 1.  set the value of the matrix
## 2.  get the value of the matrix
## 3.  set the value of the matrix inverse
## 4.  get the value of the matrix inverse

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## `cacheSolve`: This function computes the inverse of the special"matrix" returned by
## `makeCacheMatrix` above. If the inverse has already been calculated (and the matrix has not changed),
## then `cacheSolve` should retrieve the inverse from the cache.

cacheSolve <- function(x = matrix(), ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...) ## solve inverts the matrix
        x$setinverse(inv)
        inv
}

