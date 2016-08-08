################################################################################
# makeCacheMatrix: a function generates a special "matrix" object
# that can cache its inverse
################################################################################

makeCacheMatrix <- function(x = matrix()) {

        i <- NULL
        
        # set the value of the matrix
        setmatrix <- function(y) {
                x <<- matrix(y)
                i <<- NULL
        }
        
        # get the value of the matrix
        getmatrix <- function() x
        
        # set the value of the inverse matrix
        setinverse <- function(inverse) i <<- inverse
        
        # get the value of the inverse matrix
        getinverse <- function() i
        
        # Return Value of makeCacheMatrix function:
        matrix(data=c(setmatrix = setmatrix, getmatrix = getmatrix,
               setinverse = setinverse,
               getinverse = getinverse),2,2)  

}

###############################################################################
# cacheSolve: a function which will compute the inverse of the special
# "matrix" returned by `makeCacheMatrix`. 
#
# The idea is that if the inverse has already been calculated, then
# `cacheSolve` should retrieve the inverse from the cache.
###############################################################################


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        # First check if the inverse has already been stored in the cache
        i <- x$getinverse()
        
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        # if found in the cache, be lazy and return this value
        
        # if not found in the cache, perform computation
        data <- x$getmatrix()
        i <- solve(data, ...) # compute the inverse matrix
        x$setinverse(i) # store the inverse matrix in the cache
        i # return the inverse matrix
        
}
