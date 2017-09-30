## The below two functions, calculates and caches the inverse of Matrix.
## makeCacheMatrix() will creates a special "matrix" object that can cache its inverse.
## cacheSolve() computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve 
## should retrieve the inverse from the cache.


## The below function takes the a square invertible matrix as an argument and creates a special Matrix 
## object that cache's it's inverse.
## The Function returns the list of functions i.e. get, set, getInverse and setInverse.
makeCacheMatrix <- function(x = matrix()) {
                
            m <- NULL
            
            # To set the Matrix.
            set <- function (y){
                  x <<- y
                  m <<- NULL
            }
            
            # To retrive the Matrix
            get <- function(){
                  x
            }
            
            # To set the Inverse Matrix in cache.
            setInverse <- function(inverse){
                  m <<- inverse
            }
            
            # To retrive the cached Inverse of Matrix
            getInverse <- function(){
                  m
            }
            
            ## return: a list containing functions to
            ##            1. set the matrix
            ##            2. get the matrix
            ##            3. set the inverse
            ##            4. get the inverse
            ## this list is used as the input to cacheSolve()
            list(set=set,get=get,setInverse=setInverse,getInverse=getInverse)
}


## The Below function calculate and returns the Inverse of Matrix. 
## If the Inverse was already calculated, the function will return the inverse from cache.
## Otherwise Inverse will be calculated and returned.
cacheSolve <- function(x, ...) {
            
            m <- x$getInverse()
            
            # if the inverse has already been calculated 
            if(!is.null(m)){
                    # get it from the cache and skips the computation.
                    message ("Getting Cached Matrix")
                    return(m)
            }
            
            # otherwise, calculates the inverse.
            data <- x$get()
            m <- solve(data,...)
            # sets the value of the inverse in the cache via the setInverse function.      
            x$setInverse(m)
            m
}
