# Functions written in jan, 15, 2017 by Marcus Suassuna Santos
# Assingment 3; Week 3; Course: R-Programming; Data Science Specialization

# The set of functions makes it possible to cache potentially time-consuming
# computations. For this assingment, the inverse of a matrix will be cached.

# The first function creates a list of 4 functions in order to store the 
# information of the matrix.

makeCacheMatrix <- function(x = matrix()) {
      m <- NULL
      set <- function(y) {
            x <<- y
            m <<- NULL
      } # First function of the list that set value of the vector
      get <- function() x # Second function: get the value of the vector
      setinv <- function(inv) m <<- inv # Set the value of the inverse. The
      # operator "<<-" assign the value of inv to a different environment
      getinv <- function() m # get the value of the inverse
      list(set = set,
           get = get,
           setinv = setinv,
           getinv = getinv)
      # List of four functions is the output of the "makeCacheMatrix" function
}


## The function calculetes the inverse using the base function solve 

cacheSolve <- function(x, ...) {
      m <- x$getinv() # Uses the get value of the list given as argument
      if(!is.null(m)) { # Evaluate if the inverse has already been saved in cached
            message("getting cached data")
            return(m) # If yes, return the value stored in cache memory
      }
      data <- x$get() # If not, get the value of "get" in the list
      m <- solve(data, ...) # and calculates the inverse
      x$setinv(m) # store the value in cache memory
      m # returns the value of the inverse
}