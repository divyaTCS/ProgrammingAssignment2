## Our Assignment is to create a pair of R functions that can cache the inverse of a matrix since calculating
## a matrix inverse can prove to be a costly affair and may degarde oerformance 
## so it is preferred to have it cached for future use. We create a pair of functions - CacheMatrix and Cache Solve
## The same are as given below

## makeCacheMatrix: This function creates a special "matrix" object that can 
##cache its inverse. It costitutes of 4 functions doing the following - 
## 1.set the value of the matrix 2.get the value of the matrix 3.set the value of the inverse 4.get the value of theinverse

makeCacheMatrix <- function(x = matrix())   
        {      i <- NULL      
         set <- function(y)         
                {          
                        x <<- y          
                        i <<- NULL        
                }  
         get <- function() x      
         setinverse <- function(inverse)  i <<- inverse      
         getinverse <- function() i      
         list(set = set,       
              get = get,       
              setinverse = setinverse,       
              getinverse = getinverse)  
         }
                 
## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve shall retrieve the inverse from the cache. 
                 
 cacheSolve <- function(x, ...)  
        {      
                i <- x$getinverse()      
                if (!is.null(i))         
                {            
                        message("getting cached data")       
                        return(i)        
                }
                data <- x$get()           
                i <- solve(data, ...)            
                x$setinverse(i)
                i
        }
