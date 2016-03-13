
## This function:
##set the value of the matrix 
##get the value of the matrix
##set the value of the inverse matrix
##get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
        m<-NULL
        set<-function(y){
                x<<-y
                m<<-NULL
        }
        get<-function() x
        setmatrix<-function(solve) m<<- solve
        getmatrix<-function() m
        list(set=set, get=get,
             setmatrix=setmatrix,
             getmatrix=getmatrix)
}



## The following function calculates the mean of the special "matrix"
##created with the above function. However, it first checks to see if the
## inverse of the matrix has already been calculated. If so, it `get`s the inverse
##from the cache and skips the computation. Otherwise, it calculates the inverse of
##the matrix and sets the value of the inverse in the cache via the `setmatrix(m)`
##function.

cacheSolve <- function(x=matrix(), ...) {
        m<-x$getmatrix()
        if(!is.null(m)){
                message("getting cached data")
                return(m)
        }
        matrix<-x$get()
        m<-solve(matrix, ...)
        x$setmatrix(m)
        m
}
