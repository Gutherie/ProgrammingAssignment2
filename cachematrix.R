## Functions for Programming assingment 2
## Create a cached matrix object and provide a solver function

## Create a matrix with caching feature

makeCacheMatrix <- function(x = matrix()) {
    invM <- NULL
    set <- function(y) {
        x <<- y
        invM <<- NULL
    }
    
    get <- function(){
        x
    } 
    
    setInverse <- function(inverse){
        invM <<- inverse
    } 
    
    getInverse <- function(){
        invM
    }
    
    list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)
    
}


## Generate the inverse of a matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        invM <- x$getInverse()

        if (!is.null(invM) && (!is.na(invM) && !is.nan(invM))){
            ## Matrix looks good, returning cached inverse.
            return(invM)
        }
        else {
            ## calculate inverse again, no cached version available
            invM <- solve(x$get())
            return (invM)
        }

}
