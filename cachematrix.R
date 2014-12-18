## Put comments here that give an overall description of what your
## functions do

## Makes a special matrix object

makeCacheMatrix <- function(x = matrix()) {
    m<- NULL
    set<- function(y) {
        x<<-y
        m<<-NULL
    }
    get <- function() x
    setsolve<- function(solve) m<<-solve
    getsolve<- function() m
    list(set =set, get = get,
         setsolve = setsolve,
         getsolve = getsolve)
}

#Must make an object using makeCacheMatrix to then use cacheSolve
## Returns (or 'solves') a matrix that's the inverse of matrix 'x'

cacheSolve <- function(x, ...) {
    m<- x$getsolve()
    if(!is.null(m)){
        message("getting cached data")
        return(m)
    }
    data<- x$get()
    m<- solve(data, ...)
    x$setsolve(m)
    m
}