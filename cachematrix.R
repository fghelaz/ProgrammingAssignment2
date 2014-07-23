
## Create a cacheble matrix type. Has the ability to cache the
## result and look it up in the future to improve performance.
##---------------------------------------------------------------------------
## For testing (dicussion forum example):
## We create a "amatrix" object from class "makeCacheMatrix"
## amatrix = makeCacheMatrix(matrix(c(1,2,3,4), nrow=2, ncol=2))
## amatrix$get()         # Method that returns original matrix
## cacheSolve(amatrix)   # Computes, caches, and returns    matrix inverse
## amatrix$getinverse()  # Returns matrix inverse
## cacheSolve(amatrix)   # Returns cached matrix inverse using previously 
# computed matrix inverse
## amatrix$set(matrix(c(0,5,99,66), nrow=2, ncol=2)) # Modify existing matrix
## cacheSolve(amatrix)   # Computes, caches, and returns new matrix inverse
## amatrix$get()         # Returns matrix
## amatrix$getinverse()  # Returns matrix inverse
##---------------------------------------------------------------------------
## Create matrix type.


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
        cachematrix<-list(set = set, get = get, setinverse = setinverse, getinverse = 
                                                            getinverse)
        class(cachematrix) <- "makeCacheMatrix"
        
        cachematrix
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