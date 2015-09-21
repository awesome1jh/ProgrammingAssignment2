## Jane Hartsfield 9/21/2015
## Store the input matrix in cache
## Will create a special vector, which is really a list containing a function to
## 1. Set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse
## 4. get the value of the inverse
## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
      m <- NULL   # initialize m to a null vector
      set <- function(y){
            x <<- y  
            m <<- NULL
      }
      get <- function() x
      setinverse <- function(solve) m <<- solve # puts function input into m
      getinverse <- function() m
      list(set = set, get = get, setinverse = setinverse, getinverse=getinverse)
}


## Check to see if the inverse already exists. 
## If so, return the inverse from cache.
#  Otherwise, compute the inverse and store it in cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
      m <- x$getinverse() # puts getinverse into m
      if(!is.null(m)){  # if there is an inverse there
            message("getting cached data")
            return(m)   # return the inverse and exit
      }   # if not, get the data
      data <- x$get()
      m <- solve(data,...)  # compute the inverse
      x$setinverse(m)      # set the inverse to the computed inverse 
      m           # return the computed inverse
}
