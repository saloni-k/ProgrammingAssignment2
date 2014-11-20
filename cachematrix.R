#The first function, makeCacheMatrix
#creates a list containing a function to:-
#1. set the value of the matrix
#2. get the value of the matrix
#3. set the value of the inverse
#4. get the value of the inverse

makeCacheMatrix<- function(x = matrix()) {
  inv <- NULL #inv will be the inverse
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x # get returns value of original matrix
  setinv <- function(inverse) inv <<- inverse #this will be called by cacheSolve()
                        # during its first execution and will store the inverse
  getinv <- function() inv # this will return the cached inverse to cacheSolve()
                        # on next access
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)     # this is a list of functions within  
                            # makeCacheMatrix function
}

#The second function, cacheSolve
#takes the output of the first function
#as its argument
#and returns the value of its inverse either by computing
#if it has not been computed before
#or by retrieving the cached inverse
#which was previously computed


cacheSolve <- function(x, ...) {  #the input to this function is the 
                                # output of the makeCacheMatrix function
  inv <- x$getinv()           # accesses x and gets the value of inverse
  if(!is.null(inv)) {         # if inverse was not NULL i.e. already cached
    message("getting cached data") # prints a message
    return(inv)                   # returns the cached (not recomputed) inverse
  }
  data <- x$get()           # if inverse was NULL, i.e. not computed earlier
  inv <- solve(data, ...)   # the inverse is computed and returned
  x$setinv(inv)
  inv
}

