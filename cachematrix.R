## created 4 functions: set to assign value of matrix to "mymatrix", get to get 
## the value stored in mymatrix", setinv to calculate and store the inverse val-
## ue of the matrix using solve function and getinv to retrive the inverse value
##, if available

## makeCaheMatrix creates and stores the 4 funtions mentioned above and calcu-
## lates the inverse of the matrix "mymatrix"

makeCacheMatrix <- function(a = matrix()) {
## This function creates a special "matrix" object that can cache its inverse.
        mymatrix <- NULL
        set <- function(y) {
                a <<- y
                mymatrix <<- NULL
        }
        get <- function() a
        setinv <- function(solve) mymatrix <<- solve
        getinv <- function() mymatrix
        list(set = set, get = get,
             setinv = setinv,
             getinv =  getinv)
}


## cachesolve retrieves the cached value of the inverse matrix if available, 
## otherwise it calculates it by calling on one of the functions stored in the 
## vector created by MakeCacheMatrix

cacheSolve <- function(a, ...) {
        ## Return a matrix that is its inverse
        mymatrix <- a$getinv()
        if(!is.null(mymatrix)) {
                message("getting cached data")
                return(mymatrix)
        }
        input <- a$get()
        mymatrix <- solve (input, ...)
        a$setinv(mymatrix)
        mymatrix 
        
}
