#################################################################
## Programming assignment 2
## Coursera course programming in R
#################################################################

#The function 'makeVector' takes the argument x of type matrix. 
#It creates a special matrix object and a list of 4 functions. 
#No actual computation takes place within the function, it s only a store for two matrices, the original 
#Matrix (input) and the inverted one. The functions are used to display the contents of the store. 

returns the content of a matrix, 

makeCacheMatrix <- function(x = matrix()) {          #the input of the function has to be a matrix
    m <- NULL
    set <- function(y) {                             #first function: sets the content of a matrix
    x <<- y                                          #uses "<<-" operator because m is not in the environment of the function
    m <<- NULL                                       #makeCacheMatrix (same is true for setInverse)
  }
    get <- function() x                              #second function: returns the content of the matrix, 
    setInverse <- function(inverse) m <<- inverse    #third function: sets the content of the inverted matrix
    getInverse <- function() m                       #fourth function: returns the stored inversion of the matrix
  
    list(set = set, get = get,                       #return the functions wrapped in a list
       setInverse = setInverse,
       getInverse = getInverse)
}


#The function 'cacheSolve'calculates the inverse of the special matrix created with the function 'makeCacheMatrix'
#It first checks to see if the inverse of the matrix has already been calculated before. If so, it gets the value of the
#inversed matrix from the cache and skips the computation. Otherwise, it calculates the involved matrix and sets the value of 
#the involved matrix in the cache via the setInverse function which was defined in the function makeCacheMatrix
#The input is expecteing a special matrix made from the function "makeCacheMatrix"

cacheSolve <- function(x, ...) {
    ptm <- proc.time()                             #calculate processing time
    m <- x$getInverse()                            #query the x vector's cache     
        if(!is.null(m)) {                          #if there is a cache
            message("getting cached data")         #print the message that the cached data is being retrieved
            cat("Process Time", proc.time() - ptm) #print processing time
            return(m)                              #just return the value stored in cache (no computation needed)
        }
    data <- x$get()                                #if the cache is not empty, 
    m <- solve(data, ...)                          #the inverse of the matrix is computed
    x$setInverse(m)                                #save the result back to the cache of x
    cat("Process Time", proc.time() - ptm)         #print processing time
    m                                              #return the result
}


##################################################################
## Use of the functions
##################################################################

##To use the function: 
## 1) Create a special matrix using the makCacheMatrix function
## 2) Use the special matrix as input for the function cacheSolve

## Example:
#1. We create a special matrix using the makeCacheMatrix function:
set.seed(5)
a <- makeCacheMatrix(matrix(rnorm(16,0,1), 4, 4))

#2. We use the special matrix as input for the function cacheSolve: 
cacheSolve(a)

#When we use the function for the first time, the function cannot find the inverse of the matrix
#in the cache. Therefore, it calculates the inverse and then returns the inversed matrix. 

#If we use the function a second time with the same input: 

cacheSolve(a)

#then the function can find the inversed matrix in the cache and does not have to calculate it another time. 
#it prints 'getting cached data' and returns the cached inversed matrix. 