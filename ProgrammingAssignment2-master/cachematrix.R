## 1st function creates an empty matrix to set/get value of vector, and set/get value of mean. 

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
        x <<- y
        m <<- NULL
        }
        get <- function() x
        setInv <- function(val) m <<- val
        getInv <- function() m
        list(set = set, get = get,
             setInv = setInv,
             getInv = getInv)
}


## 2nd function 1st check if mean has been calculated.  If so, gets mean from cache and skip computation. Otherwise,calculate mean of data and set the mean in cache in setmean function. 

cacheSolve <- function(x, ...) {
        m <- x$getInv()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setInv(m)
        m
}
