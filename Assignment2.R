makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinv <- function(inv) i <<- inv
    getinv <- function() {
        if(is.null(i)) {
            i <<- solve(x)
        }
        i
    }
    list(set = set, get = get, setinv = setinv, getinv = getinv)
}

cacheSolve <- function(x, ...) {
    i <- x$getinv()
    if(is.null(i)) {
        data <- x$get()
        i <- solve(data, ...)  # Use 'solve' instead of 'inv'
        x$setinv(i)
    } else {
        message("Getting cached data.")
    }
    i
}

# Create a matrix
A <- matrix(c(1, 2, 3, 4), nrow = 2, byrow = TRUE)

# Make a CacheMatrix object
cached_A <- makeCacheMatrix(A)

# Compute the inverse once and cache it
inverse_A <- cacheSolve(cached_A)

# Retrieve the cached inverse multiple times
# This will be fast because the inverse is cached
cached_inverse_A <- cacheSolve(cached_A)