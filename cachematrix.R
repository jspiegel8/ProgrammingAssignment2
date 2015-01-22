## Put comments here that give an overall description of what your
## functions do
##The makeCacheMatrix and cacheSolve functions work to take the
## the inverse of a matrix, however the the function
## will work to get the the cached data
## instead of computing the matrix if the inverse for a
## certain matrix has already been calculated and cached
## 
## Write a short comment describing this function
## makeCacheMatrix function works to 
## 1. set the values of a matrix
## 2. get the values of a matrix
## 3. set the values of the inverse of a matrix
## 4. get the values of the inverse of a matrix
makeCacheMatrix <- function(x = matrix()) {
 ## sets up the paramaters of the makeCacheMatrix 
        ## assigns a function which takes a paramter
        ## x which is a matrix
        m <- NULL
       ## sets m to a value of null
        set <- function(y) {
                x <<- y
                m <<- NULL} 
       ## set is a function which takes the paramater y
       ## y is equal to x but is assigned in difft env.
        
        get <- function() x
       ## get, gets the value of matrix x
        setinverse <-function(inverse) m <<- inverse
       ## sets the inverse
        getinverse <- function()m
       ## gets the inverse
        list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
        
}


## Write a short comment describing this function
## This function calculates the inverse of the matrix
## but first looks to see if the invese has already
## been calculated. If it has been calculated it
## gets the cahced value. If it hasnt been calculated,
## it calculates the inverse and sets the inverse value
## in the cache via the setmeanfunction
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        ## m is set to the get inverse value of x
        m <- x$getinverse()
        ## if the inverse value is not null, then 
        ## we will get the value from cached data
        if(!is.null(m)){
                message ("getting cached data.")
                return(m)
        }
        ## if the inverse value is null, then 
        ## get the values of the matrix and solve
        ## for the inverse of the matrix and then
        ## set the value of the inverse in the cache
        ## finally, return the inverse values
        data <- x$get()
        m <- solve(data)
        x$setinverse(m)
        m
        
}
