## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {     #defining the argument of the function with matrix
       inv <- NULL                              #this initisalises inverse matrix(i) to the NULL
  set <- function(y) {                          #define the set function to assign new
    x <<- y                                     #the value of the matrix in parent environment
    inv <<- NULL                                #if there is a new matrix reset i , to NULL
  }
  get <- function() x                           #define the value of the get function and returns a matrix x
  setinv <- function(inverse) inv <<- inverse   #stinverse function defines in parent environment 
  getinv <- function() inv                      #getinverse function gets the value of i where it is called 
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {                #defining the argument of the cacheinverse function with matrix
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinv()                       #getting the inverse of x matrix
  if(!is.null(inv)) {                           #if the inverse is there ,then the inverse is not calculated
    message("getting cached result")            #then the message is printed 
    return(inv)                                 #inverse matrix is returned 
  }
  data <- x$get()                               #get the matrix
  inv <- solve(data, ...)                       #calculating the inverse 
  x$setinv(inv)                                 #set the nverse matrix
  inv                                           #returning the inverse matrix
}
