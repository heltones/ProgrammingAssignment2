##################################################################
# Programming Assignment 2: Lexical Scoping
# Eric Helton
# 
# R Programming, Coursera, August 2014 Session
# Instructors: Roger D. Peng, PhD, Jeff Leek, PhD, Brian Caffo, PhD
# 
# 24 August 2014
# 
##
# 
# These two functions create and modify (utilizing lexical scoping)
# an object in R that holds a matrix as well as a cached inverse
# to that matrix (after having been calculated the first time).
# 
# makeCacheMatrix creates the matrix object that can hold the matrix's
#   inverse.
# cacheSolve checks to see if the inverse of the matrix object has
#   already been calculated and returns it if so. If it finds that the
#   inverse has not yet been calculated, it calculates it via the
#   solve() tool and crams the result back up into the matrix object.
#
##################################################################

## This function creates the matrix object which can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL  #initialize the inverse as NULL
  set <- function(y) {
    x <<- y    #when using set, store the incoming matrix in x
    i <<- NULL  #initialize the inverse as NULL
  }
  get <- function() {x}    ## returns the value of the matrix
  setInv <- function(solve) i <<- solve  ## uses solve() for storing inverse
  getInv <- function() {i}   ## outputs the inverse, stored in i
  list(set = set, get = get,    ## list of named functions in the obj
       setInv = setInv,
       getInv = getInv)
}

## This function checks for a cached version of the inverse to the
##   matrix. It returns the cached value if it exists and calculates
##   and stores it in the matrix object if it hadn't yet been
##   calculated and cached.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$getInv()    ## get the cached inverse from the matrix object
  if(!is.null(i)) {    ## check if it exists, if so (bang null), then notify
    message("getting cached data")
    return(i)   ## and return inverse, ending this function
  }
  data <- x$get()    ## if not returned out above, get the matrix
  i <- solve(data, ...)    ## solve for the matrix's inverse
  x$setInv(i)    ## store the inverse into the object
  i
}
