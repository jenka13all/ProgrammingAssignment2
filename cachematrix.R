## this set of functions allows us to create a cacheable inverse matrix, 
## and retrieve that matrix from the cache instead of repeatedly creating it anew
## this saves computational time, as inverting matrices is a very expensive operation


## create a list of callable functions to:
## return an already existing matrix
## set a matrix to passed-in parameter if not yet existing
## return an already existing inversion of matrix
## set an inversion of matrix if not yet existing
makeCacheMatrix <- function(x = matrix()) {

    inverse <- NULL
    
    #assign local matrix to passed-in parameter
    set <- function(paramMatrix) {
        x <<- paramMatrix
        inverse <<- NULL
    }
    
    #return set matrix 
    get <- function() x
    
    #get / set inverse
    setinverse<- function(paramInverse) inverse <<- paramInverse
    getinverse <- function() inverse
    
    #assign our functions to a list so they can be called outside this function
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## if an inverse matrix for passed-in parameter already exists, return it
## else, create one and return it
cacheSolve <- function(x, ...) {
    
    ## Return a matrix that is the inverse of 'x'
    inverse <- x$getinverse()
    
    if(!is.null(inverse)) {
        message("getting cached data")
        return(inverse)
        #exit function
    }
    
    #else...
    matrix <- x$get()
    
    matrixInverse <- solve(matrix)
    x$setinverse(matrixInverse)
    
    #return 
    matrixInverse
}
