## I was struggling with this assignment very much, but I succeeded in the end!
## I am wondering in what context cache creation in R is normally utilized. 

##This code defines two functions, makeCacheMatrix and cacheSolve, to  +
##efficiently compute and cache the inverse of a matrix. makeCacheMatrix creates +
##object capable of storing a matrix and its cached inverse, while cacheSolve + 
##retrieves the inverse from the cache if available, otherwise computes and stores it.

## The function makeCacheMatrix creates a special "matrix" object that can cache 
## its inverse.

##the variable makeCacheMatrix is defined as a function with a default matrix()
makeCacheMatrix <- function(x = matrix()) {
  
  ##The variable inverse is defined as NULL. This variable will be used to +
  ## store the matrix. 
  inverse <- NULL
  
  ##The purpose of set is to allow the user to change the underlying data that +
  ## cacheSolve operates on. 
  ##Inside the function, the value 'y' is assigned to 'x' in the parent environment
  ##The parent environment is the makeCacheMatrix environment. 
  set <- function(y) {
    x <<- y
    
    ## m is assigned to NULL to invalidate any previously cached m when x is updated. 
    inverse <<- NULL
  }
  
  ##Defines the function 'get' to retrieve the matrix from the cache. 
  get <- function() x
  
  ## 'setInverse' takes the argument 'inv' and this function is used to set a new +
  ## inverse to the cache object while assigning 'inv' to the inverse variable +
  ## in the parent scope. 
  setInverse <- function(inv) inverse <<- inv
  
  ## 'getInverse' is defined to retrieve the cached inverse from the cached object
  getInverse <- function() inverse
  
  ## A list is created to list the functions. 
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

## This function computes the inverse of the special
## "matrix" returned by `makeCacheMatrix` above. If the inverse has
## already been calculated (and the matrix has not changed), then
## `cacheSolve` should retrieve the inverse from the cache.
cacheSolve <- function(cache) {
  
  ##This verifies if the cached inverse already exists. It sees if the cached +
  ## inverse is NULL. 
  if(!is.null(cache$getInverse())) {
    cat("getting cached data")
    return(cache$getInverse())
  }
  
  ##If the cachd inverse does not exist, this retrieves the matrix from the cache. 
  mat <- cache$get()
  
  ##Computes the inverse into the matrix using solve().
  inv <- solve(mat)
  
  ##Sets the newly computed inverse back into the the cache.
  cache$setInverse(inv)
  
  ## Return a matrix that is the inverse of 'x'.
  inv
}

# Creating a cache object and setting a matrix
mat <- makeCacheMatrix(matrix(c(4, 2, 2, 3), nrow = 2))

# Getting the matrix from the cache
mat$get()

# Setting a new matrix
mat$set(matrix(c(3, 1, 1, 2), nrow = 2))

# Computing or retrieving the inverse
cacheSolve(mat)


cacheSolve <- function(x, ...) {
      
}
