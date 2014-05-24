## CacheMatrix - functions fror creating and querying 
##               matix objects that are able to cache
##               its inverse. The matrix must be invertable.
##
## This implementation is slightly different from lesson, in that 
## the matrix object itself caches the data. This makes the
## cacheSolve function very simple


## makeCacheMatrix - create a matrix that will cache it's inverse 
## Params: x - The invertable matrix
## Returns: Object with list of functions -
##          set (x) - Replace the current data and clears the cache
##          get () - Return the matrix
##          getinv () - Return the inverse of x 
##                      and caches value
##          setinv ([y]) - Set the inverse matrix of x, 
##                         using y if given, otherwise
##                         compute inverse from x

makeCacheMatrix <- function(x = matrix()) {
    mCache <- NULL
    set <- function (data) {
        x <<- data
        mCache <<- NULL
    }
    get <- function () {
        x
    }
    getinv <- function () {
        setinv ()
        mCache
    }
    setinv <- function (data = F) {
        if (is.null (mCache)) {
            # Debug print ("Setting cached data")
            if (is.matrix (data)) {
                mCache <<- data
            } else {
                mCache <<- solve (x)
            }
        }
    }
    list (set = set, get = get, setinv = setinv, getinv = getinv)
}



## cacheSolve (x) - Return inverse of matrixObject
## Params: x - MAtrix object cerated with makeCacheMatrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        x$getinv ()
}
