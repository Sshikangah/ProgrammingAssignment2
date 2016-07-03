## The following function will initialize matrix inverse 
##  set the matrix
##  get the matrix
##  set inverse
##  get inverse

makeCacheMatrix <- function(x = matrix()) {
                inver <- NULL
                set <- function(matrix) {
                        ## using <<- to assign a value to an object
                        ## that is different from current environment.
                        x <<- matrix
                        inver <<- NULL
                }
                get <- function() x
                setInver <- function(inverse) inver <<- inverse
                getInver <- function() inver
                list(set = set, get = get,
                     setInver = setInver,
                     getInver = getInver)

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        x <- x$getinver()
        ## check if the inverse is already been calculated
        if(!is.null(x)) {
                message("getting cached data")
                return(x)
        }
        ## else calculate the inverse
        matrix.data <- x$get()
        x <- solve(matrix.data, ...)
        ## set the value of the inverse in the cache through setinver
        x$setInver(x)
        return(x)
        ## Return a matrix that is the inverse of 'x'
}
