#Gets the function

makeCacheMatrix <- function(x = matrix()) 
{
  inv <- NULL
  set <- function (y) {
    x <<- y
    inv <<- NULL
  }
  get <- function()x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function()inv
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}

#Solves and stores the inverse of the matrix
cacheSolve <- function(x)
{
  inv <- x$getinverse()
  if(!is.null(inv))
  {
    message("Inv is stored already")
  }

  data <- x$get()
  inv <- solve(data)
  x$setinverse(inv)
}