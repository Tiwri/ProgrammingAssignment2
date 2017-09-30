## The below two functions, calculates and caches the inverse of Matrix.
## makeCacheMatrix() will creates a special "matrix" object that can cache its inverse.
## cacheSolve() computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.


## The below function takes the matrix as an argument and creates a special Matrix object that cache's it's inverse.
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
            
            # Returning the list of function's as accessor/mutators of Matrix.
            list(set=set,get=get,setInverse=setInverse,getInverse=getInverse)
}


## The Below function calculate and returns the Inverse of Matrix. 
## If the Inverse was already calculated, the function will return the inverse from cache.
## Otherwise Inverse will be calculated and returned.
cacheSolve <- function(x, ...) {
            
            m <- x$getInverse()
            
            # If the matrix is cached, then return from cache. 
            if(!is.null(m)){
                    message ("Getting Cached Matrix")
                    return(m)
            }
            
            # Otherwise retrive the Matrix, Calculate the Inverse, set the result in cache and return the Inverse.
            data <- x$get()
            m <- solve(data,...)
            x$setInverse(m)
            m
}
