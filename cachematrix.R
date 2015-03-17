## These 2 functions are calculating the inverse of the matrix x, by first 
## checking whether the inverse of the matrix x is cached and thus calculated before.
## This method is used to reduce calculating time and therefor costs

## The first function 'makeCacheMatrix' defines a list containing 4 functions to
## (a) set the given matrix, (b) get the matrix, (c) calculate the inverse of the matrix, (d) get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) { # set the function makeCacheMatrix
  m<-NULL                                   # set default to NULL
  set<-function(y){                         # set the value of the matrix
    x<<-y                                   # set the matrix of x to a new matrix y
    m<<-NULL                                # set the inverse to NULL
  }
  get<-function() x                         # get the matrix x and assign it to 'get'
  setsolve<-function(solve) m<<- solve      # calculate the inverse of the matrix and assign it to m
  getsolve<-function() m                    # get the inverse of the matrix from m
  list(set=set, get=get,                    # create a list containing the 4 functions: 'get' 'set' 'setsolve' 'getsolve'
       setsolve=setsolve,
       getsolve=getsolve)
}


## The second function is 'cacheSolve' and it checks whether the inverse of the matrix x is allready
## cached and therefore does not need to be calculated. Otherwise it calculates the inverse of the matrix x

cacheSolve <- function(x=matrix(), ...) {   # set the function cacheSolve
  m<-x$getsolve()                           # get the inverse from the cache and assign it to m
 if(!is.null(m)){                           # check whether the inverse is cached (if the inverse isn't cached m is set to NULL)
    message("getting cached data")          # if so, return a message about getting the data from the cache
    return(m)                               # and then return the cached inverse
  }
  matrix<-x$get()                           # if the inverse is not cached, get the matrix
  m<-solve(matrix, ...)                     # and compute the inverse of the matrix, with the solve-function
  x$setsolve(m)                             # running the setsolve function to cache the inverse 
  m                                         # return the inverse of the matrix x
}
