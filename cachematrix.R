## The below two functions, calculates and caches the inverse of Matrix.

## The below function creates a special Matrix object that cache's it's inverse.
makeCacheMatrix <- function(x = matrix()) {
                
            m <- NULL
            set <- function (y){
                  x <<- y
                  m <<- NULL
            }
            
            get <- function(){
                  x
            }
            
            setInverse <- function(inverse){
                  m <<- inverse
            }
            
            getInverse <- function(){
                  m
            }
            
            list(set=set,get=get,setInverse=setInverse,getInverse=getInverse)
}


## The Below function calculate and returns the Inverse of Matrix. 
## If the Inverse was already calculated, the function will return the inverse from cache.
cacheSolve <- function(x, ...) {
        
            m <- x$getInverse()
            if(!is.null(m)){
                    message ("Getting Cached Matrix")
                    return(m)
            }
            
            data <- x$get()
            m <- solve(data,...)
            x$setInverse(m)
            m
}
