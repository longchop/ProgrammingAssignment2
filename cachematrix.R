## The role of these 2 functions is to store the matrix x
## Then return the inverse of the matrix by either 2 ways:
## 1. If the matrix inverse has already been calculated it then returns the cache (m is stored into memory)
## 2. If the matrix inverse has not been calculated then it calculates the inverse of the matrix and then stores it
## This will speed up the function because it doesn't need to recalculate the inverse if it has already been calculated.

## The makeCacheMatrix stores a list of 4 functions set, get, setInverse and getInverse and are used as an input to cacheSolve
## This function can be assigned to a name
## set will change the matrix stored in the main function
## get will return the matrix x stored in the main function
## setInverse will store the inverse in m into main function
## get Inverse will returns the inverse

makeCacheMatrix <- function(x = matrix()) {   #x can only be a matrix
    m <- NULL 				      #clears an already stored inverse (if there is one)
    set <- function(y) {
        x <<- y           # substitute the matrix x with y, '<<-' assign value different from current environment
        m <<- NULL        # restores the null value of the inverse m because the old inverse of the old matrix is redundant
    }
    get <- function() x
    setInverse <- function(inverse) m <<- inverse
    getInverse <- function() m 
    list(set = set, get = get,            # List all functions in makeCacheMatrix
         setInverse = setInverse,               # Assigned object has all 4 functions
         getInverse = getInverse)
}

## cacheSolve computes the inverse of the matrix returned by makeCacheMatrix. If inverse has already been calculated then it finds the inverse
## First verifies value m stored previously with getInverse, exists and not NULL
## If it exists returns message and value m and ends function
## If does not exist calculate the inverse from the value x set by makeCacheMatrix

cacheSolve <- function(x, ...) {
    m <- x$getInverse()
    if(!is.null(m)) {                    
        message("getting cached data") # if m exists then return m with message
        return(m)
    }
    vMatrix <- x$get() #if m does not existes use solve() to find the inverse
    m <- solve(vMatrix, ...)
    x$setInverse(m)
    m
}