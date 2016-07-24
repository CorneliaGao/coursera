##this function have two factors: x,inv
##x is the matrix that we need to deal with
##inv is used to save the result 

makeCacheMatrix <- function(x = matrix()) {
inv <- NULL 
# sets the value of inv to NULL 
set <- function(y) { #set the value of the matrix
    x <<- y 
    ## caches the inputted matrix so that cacheSolve can check whether it has changed 
    inv <<- NULL 
    # # sets the value of inv (the matrix inverse if used cacheSolve) to NULL
}
get <-function()x
setinverse <- function(inverse)inv<<-inverse
getinverse <- function()inv
# Parts removed,save the result into inv
list(set = set, get = get, 
   setinverse = setinverse,
   getinverse = getinverse)
}
# creates a list to house the four functions, which we defined before



cacheSolve <- function(x, ...) {
        inv<-x$getinverse()
	if(!is.null(inv)){
		message("getting cached data")
		return(inv)
	}
	mat.data<- x$get()
	inv <- solve(mat.data)
	x$setinverse(inv)
	return(inv)
}


##Sample test:
##mat <- matrix(data = c(5:8), nrow = 2, ncol = 2)
##cachedMat <- makeCacheMatrix(mat)
##cachedMat$get()
     [,1] [,2]
[1,]    5    7
[2,]    6    8

##CacheSolve(cachedMat)
##     [,1] [,2]
##[1,]   -4  3.5
##[2,]    3 -2.5
##> 

##> CacheSolve(cachedMat)
##getting cached data
##     [,1] [,2]
##[1,]   -4  3.5
##[2,]    3 -2.5
