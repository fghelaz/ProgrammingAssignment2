

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


## Return inverse of cachedmatrix x and check cache if available. 

cacheSolve <- function(x, ...) {
        
        ## Get inv value from makeCacheMatrix function
        inv <- x$getinverse()
        
        ## Check if matrix has already been cached
        if(!is.null(inv)) {
                message("You`re using astr cached matrix.")
                return(inv)
        }
        
        ## Get matrix from makeCacheMatrix
        data <- x$get()
        
        ## Solve matrix inverse
        inv <- solve(data, ...)
        
        ## Set cache matrix inverse
        x$setinverse(inv)
        
        ## Return inverse matrix
        inv
}