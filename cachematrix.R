# makeCacheMatrix:creates a special "matrix", which is  a list containing a function to:
# 1. Set the value of the matrix
# 2. Get the value of the matrix
# 3. Set the value of the inverse
# 4. Get the value of the inverse

# Use this to create random normal square matrix , a<- matrix(rnorm( ), nrow = ) 

# z<- makeCacheMatrix(x) / This creates a special matrix 


makeCacheMatrix <- function(x = matrix()) {
 
  # inverse im is being initialised, the inverse im will store the value of inverse of matrix
  im <- NULL
  
  # set function will help assign the the matrix created in the beginning to x
  
  set <- function(y) {
    x <<- y #assign values to x
    im <<- NULL # im has been assigned a null value i.e nothing has been calculated yet
  } 
  # This function will get the value of matrix
  get <- function() x
  
  # This function will set the value of inverse
  setim <- function(inverse) im <<- inverse 
  
  # This function will get the value of inverse
  getim <- function() im
  
  # This will return the matrix with all functions
  list(set = set, get = get, setim = setim, getim = getim)
}


# cacheSolve will check the inverse if it is empty it will compute the inverse of the matrix. else return the value of inverse 
cacheSolve <- function(x, ...) {
  im <- x$getim() # gets the value of inverse and passes it to im
  
  # checking the inverse for a value, if not null then return the value of inverse im from cache
   
  if (!is.null(im)) {
    message("getting cached data")
    return(im)
  }
  
  # calculate inverse
  message ("matrix has changed")
  data <- x$get()
  im <- solve(data, ...)
  
  # save the inverse value
  x$setim(im)
  
  # Return the value to the console
  im
}