
## This function creates a special "matrix" object that can cache its inverse.
## this function take takes one parameter x which is a matrix 
## this function return a list containing 4 functions 

makeCacheMatrix <- function(x = matrix()) {
        m<-NULL
        set<-function(y){
               x<<-y
               m<<-NULL
        }
        
        get<-function() x
        setsolve<-function(solve) m<<-solve
        getsolve<-function() m
        list(set = set,get = get,
             setsolve = setsolve,
             getsolve = getsolve
             )
}


## This function computes the inverse of the special "matrix" 
##returned by makeCacheMatrix above.

##this function takes the special matrix created with makeCacheMatrix

##this function return The inverse of the matrix x

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m<-x$getsolve()
        if(!is.null(m)){
                message("getting cached data")
                return(m)
        }
        matrix<-x$get()
        m<-solve(matrix,...)
        x$setsolve(m)
        m
}
