# function that takes a matrix as input and creates a list of 4 objects 
# of named elements set, get, setsolve and getsolve

# this names the function and indicates it has a single argument of a matrix
makeCacheMatrix <- function(x = matrix()) {
  
  # initializes local variable to NULL
  m <- NULL                    
  
  # first element of the list being returned from the makeCacheMatrix
  set <- function(y) {    
    # assignment to x searching not just the local environment for y
    x <<- y
    # set local object m to NULL
    m <<- NULL
  }
  # create second element of list to a function with a value of input of 
  # makeCacheMatrix
  get <- function() x
  
  # create third element of list setting local variable m to object solve 
  # which is searched for outside of local environment if not found
  setsolve <- function(solve) m <<- solve
  
  # create forth element of list. this is where function cacheSolve stores 
  # inverse of the metric passed it 
  getsolve <- function() m
  
  # returns the list
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}

## Write a short comment describing this function

# function that uses the output of makeCacheMatrix function and creates the
# inverse of the matrix passed to it as the second object in the list

# define function cacheSolve with single argument
cacheSolve <- function(x, ...) {
  
  # set local m to NULL (if first time) or inverse of x in 4th element in list
  m <- x$getsolve()
  
  # test m set above, in not = NULL display msg and return value
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  # if m was NULL set matrix dat to 2nd element of input list (matix input values)
  dat <- x$get()
  
  # create inverse in local m
  m <- solve(dat, ...)
  
  # set parent environment list element setsolve to m 
  x$setsolve(m)
  # returns m 
  m
}