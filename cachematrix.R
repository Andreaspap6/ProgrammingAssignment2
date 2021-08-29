# makeCacheMatrix - function that returns a list of functions
makeCacheMatrix <- function(x = matrix()) {
        
        # holds the cached value or assigns NULL if nothing is cached
        i <- NULL
        
        # stores the matrix
        set <- function(y){
                x <<- y
                
                # reassigns i to NULL now that a new value is assigned to x
                i <<- NULL
        }
        
        # returns the stored matrix
        get <- function()x
        
        # caches the given argument
        setInverse <- function(inverse)i <<- inverse
        
         # gets the cached value
        getInverse <- function()i
        
        # returns a list
        list(set = set, get = get, 
             setInverse = setInverse, 
             getInverse = getInverse)
}

# Calculates the inverse of the matrix created with 
# makeCacheMatrix

cacheSolve <- function(x, ...) {
        
        ## Returns a matrix that is the inverse of 'x'
        i <- x$getInverse()
        
        # if a cached value exists then return it
        if(!is.null(i)){
                message("getting cached data")
                return(i)
        }
        
        # otherwise get the matrix, caclulate the inverse and store it in
        # the cache
        matrix <- x$get()
        i <- solve(matrix,...)
        x$setInverse(i)
        
        # returns the inverse
        i
}
