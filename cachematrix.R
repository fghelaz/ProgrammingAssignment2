

makeCacheMatrix <- function(x = matrix()) {
        
        ## Initialize variable inv to NULL
        inv <- NULL
        
        ## Set matrix
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        
        ## Get matrix
        get <- function() x
        
        ## Set cached matrix
        setinverse <- function(matinv) {inv <<- matinv}
        
        ## Get cached inverse matrix
        getinverse <- function() {inv}
        
        ## Return cachedmatrix class type with a list of methods
        list(set = set, get = get, setinverse = setinverse, getinverse = 
                     getinverse)
}
