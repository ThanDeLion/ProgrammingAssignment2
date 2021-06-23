## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        d<-NULL ## introduce an empty matrix
        set<- function(y){ ## save function
        x <<-y
        d<<-NULL        
        }
        get<-function() x 
        setInverse <- function(inverse)d<<-inverse ## saving the value of the inverse
        getInverse <- function()d ## returns the value of the inverse
        list(set = set, get = get, setInverse = setInverse, getInverse = getInverse) 
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
          d <- x$getInverse()
  if(!is.null(d)){ ## returns the recent saved value of the inverse if not empty
  message("Opening Cache") 
  return(d)
  }
  mat <- x$get()
  d <- solve(mat,...) ##computes for the inverse of the matrix
  x$setInverse(d) ## saves the inversed matrix
  d
}
