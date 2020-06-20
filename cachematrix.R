## In this case, the makeCacheMatrix function is the parent function, assuming the matrix is invertible 

makeCacheMatrix <- function(x = matrix()) { 
        inv <- NULL 
        set <- function(y){ ## setting the value of the matrix
                x <<- y ## managing the environement levels, with << we are able to modify variables in the parent levels 
                inv <<- NULL
        } ## set function is the child function 
        get <- function() {x}
        setInverse <- function(inverse) {inv <<- inverse}
        getInverse <- function() {inv} 
        list(set = set, get = get, 
             setInverse = setInverse, 
             getInverse = getInverse)
} 

## cacheSolve computes the inverse of the created function from above

cacheSolve <- function(x, ...) {
        inv <- x$getInverse() ## first need to check if the inverse has already been calculated
        if(!is.null(inv)) {
                message("pulling cache data")
                return(inv)
        }
        mat <- x$get()
        inv <- solve(mat, ...)
        x$setInverse(inv)
        inv
}
