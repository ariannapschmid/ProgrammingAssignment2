makeCacheMatrix <- function(x = matrix()) {
  #Below, the object myInverse is being initialized to NULL 
  # x is already initialized as a function argument
  myInverse <- NULL
  
  #Below, we are assigning the input to x as well as 
  # assigning the object myInverse to NULL. This will reset 
  # any previous value of myInverse that had been cached
  set <- function (y){
    x <<- y
    myInverse <<- NULL
  }
  #Below, we retrieve ("get") our matrix x.
  get <- function() x
  
  #Below, we set the inverse
  setinverse <- function(inverse) myInverse <<- inverse
  
  #Below, we retrieve ("get") the inverse
  getinverse <- function() myInverse
  
  # Below, we assign names to each of the functions above, and place them in
  # a list to be returned to the parent environment
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
  # end of makeCacheMatrix
}



cacheSolve <- function(x, ...) {
  #Below, we try to retrieve the inverted matrix by calling getInverse()
  # Then, in the if statement, we check to see if the inverse had already
  # been cached previously (this follows if !is.null(myInverse) is TRUE). If
  # TRUE, we return the inverted matrix. 
  myInverse <- x$getinverse()
  if(!is.null(myInverse)) {
    message("getting cached data")
    return(myInverse)
  }
  #If FALSE, we retrieve the matrix from the input, calculate the inverse,
  # then call setInverse to set the calculated inverse to the inputed matrix object 
  # then we return the inverted matrix
  data <- x$get()
  myInverse <- solve(data, ...)
  x$setinverse(myInverse)
  myInverse
  
}

