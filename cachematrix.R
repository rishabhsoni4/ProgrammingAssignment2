

## makeCacheMatrix is a function that takes an invertible matrix argument, then caches this matrix so it can later be inverted using  the function cacheSolve

makeCacheMatrix <- function(x = matrix()) {#Defint the argument for the function as a matrix called "x"
  m <- NULL # variable to hold values computed using the following:
  set <- function(y) { #create a "set" variable to hold the function of "y"
    x <<- y  #cache "x" matrix in the variable "y" outside current work environnment
    m <<- NULL #cache the values in "m" outside current work environment.  
  }
  get <- function() x  #Get the previously cached "x" matrix
  setsolve <- function(solve) m <<- solve # get "x" matrix from "m" and store to a cache called "solve", store this in "m"
  getsolve <- function() m #get the matrix and invert it using "solve"
  list(set = set, get = get,# list of variables and their values
       setsolve = setsolve,
       getsolve = getsolve)
}
#Now that we have created and solved our matrix, make a function "cacheSolve" to return the inverted (solved) matrix. If the matrix has been solved before, we merely report the result. If the matrix has not been seen before (i.e. is not already cached), we solve it from scratch and retrun the newly calculated inverse of the matrix

cacheSolve <- function(x, ...) { #Define argument "x", which should be an invertible matrix
  m <- x$getsolve() #fetch the contents of getsolve stored in "m"
  if(!is.null(m)) { #if "m" is not empty (previously calculated matrix)
    message("getting cached data") # message to tell us this result has already been seen and stored in cache
    return(m)#then return the cached inverse
  }#OTHERWISE
  data <- x$get() #get the new matrix "x" and assign this to the variable "data"
  m <- solve(data, ...)#set "m" to contain the command to solve (invert) the data
  x$setsolve(m)# solve the "x" matrix held in the "m" variable
  m #return the solved matrix (inverse of x)
}
