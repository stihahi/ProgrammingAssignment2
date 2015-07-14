##makeCacheMatrix : makes special matrix that can cache inverse matrix
##To be easily understood, "im" is used for an attribute.

## make special matrix.
makeCacheMatrix <- function(x = matrix()){
    im <- NULL
    set <- function(y){
        x<<-y
        im<<-NULL
    }
    get <- function() x
    setIMatrix <- function(mat) im<<-mat
    getIMatrix <- function() im
    list(set = set,get = get,
         setIMatrix = setIMatrix,
         getIMatrix = getIMatrix)
}

##caculate inverse matrix as necessary, and caches it.
##when there is already cache, print message that the program is using the cache.
cacheSolve <- function(x, ...){
    im<-x$getIMatrix()
    if(!is.null(im)){
        message("getting cache data")
        return(im)
    }
    data <- x$get()
    im <- solve(data, ...)
    x$setIMatrix(im)
    im
}