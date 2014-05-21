#The first function, makeCacheMatrix creates a special "matrix", which is really a list containing a function to
#set the inverse of the matrix
#get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL #defining a empty variable
  set <- function(y) # setting the cache
  {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setInverse <- function(Inverse) i <<- Inverse #setting the inverse of the matrix
  getInverse <- function() i
  list(set = set, get = get, #listing the matrices here
       setInverse = setInverse,
       getInverse = getInverse)
}


#The following function calculates the inverse of the special "matrix" created with the above function. However, 
#it first checks to see if the inverse has already been calculated. If so, it gets the inverse from the cache and skips the computation.
#Otherwise, it calculates the inverse of the data and sets the value of the inverse in the cache via the setInverse function.

cacheSolve <- function(x, ...) {
  i <- x$getInverse() ## takes the "matrix" from the first function
  if(!is.null(i)) ## checks if there is a cache or not
  {
    message("getting cached data")
    return(i) ## if there is a cache then it just returns the cached output ie. the inverse.
  }
  data <- x$get() ## if the there is no cache
  i <- solve(data, ...) ## inverse is calculated here
  x$setInverse(i) ## again save it to the cache
  i ## and returns it
}