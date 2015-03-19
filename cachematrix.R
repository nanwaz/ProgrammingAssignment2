# Matrix inversion can take up a lot of memory, so caching using the <<- to cache helps
# speed up processing because you only have compute it once, then retrieve it from cache after that. 

#The first function, makeCacheMatrix creates a special "matrix", which is a list containing a function to:

# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix
#

makeCacheMatrix <- function(x = matrix()) { # return the input matrix
  i <- NULL # set the inversed matrix
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse #set inversed matrix
  getinverse <- function() i #get inversed matrix
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}

# cacheSolves returns the inverse of the matrix. First, it checks that 
# the inverse has already been computed. If so, it gets the cached results. 
# If not, it computes the inverse, sets the value in the cache via
# setinverse function.

# This function assumes that the matrix is swaure to it is always invertible. It does not provide error checking.
cacheSolve <- function(x, ...) {
  i <- x$getinverse() # get the inversed matrix from object x
  if(!is.null(i)) { # it will be null if uncalculated
    message("getting cached data.")
    return(i)
  }
  data <- x$get()# if not, get the matrix object
  i <- solve(data) # compute the inverse of a square matrix i
  x$setinverse(i)
  i # return the solved result
}


# Test
# generate simple test matrix
testMatrix = rbind(c(2, -1/2), c(-1/2, 2))
# generate the makeCacheMatrix object 
test<- makeCacheMatrix(testMatrix)
# Call matrix from cache by getting the calculated inversion using the cacheSolve function
# show results with test$get()
test$get()
# now show that caching works with cachesolve
cacheSolve(test) ##First Run calculate inverse 
###
###[,1]      [,2]
###[1,] 0.5333333 0.1333333
###[2,] 0.1333333 0.5333333
cacheSolve(test) #Second Run, cache results are returned
###getting cached data.
###[,1]      [,2]
###[1,] 0.5333333 0.1333333
###[2,] 0.1333333 0.5333333
cacheSolve(test) #Third Run, cache results are returned 
###getting cached data.
###[,1]      [,2]
###[1,] 0.5333333 0.1333333
###[2,] 0.1333333 0.5333333