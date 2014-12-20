## Invert Matrix functions.
## The two functions here are used to compute the inverse of a matrix.
## First use "MyMatrix <- makeCacheMatrix(your_square_matrix)" to create a function MyMatrix
## and initialize it with your square matrix.
## The call "cacheSolve(MyMatrix) to find the inverse on your_square_matrix

## makeCacheMatrix is used to initilize the variables and functions that 
## will be used by cacheSolve
makeCacheMatrix <- function(x = matrix()) {
      ## Initialize m with NULL
      m <- NULL
      
      ## Set function is Only used for testing to change the initial matrix.  
      set <- function(y) {
            ## store the new square matrix in x in the parent function
            x <<- y
            ## initialize m with NULL in the parent function
            m <<- NULL
      }
      
      ## Returns the initial Matrix to cacheSolve
      get <- function() x
      
      ## set m with the solved matrix inverse
      setsolved <- function(solved) m <<- solved
      
      ## returns the matrix inverse
      getsolve <- function() m
      
      ## needed to allow setsolve and getsolve to be called by cacheSolve
      list(set = set, get = get,
           setmean = setmean,
           getsolve = getsolve)

}

## cacheSolve is used to compute the inverse of a matrix.  If this is the first call (i.e. m = NULL)
## then compute inverse and store it in parent function environment for possible future calls.
cacheSolve <- function(x, ...) {
      ## returns either the already solved inverse matrix or NULL
      m <- x$getsolve()
      
      ## if m is NULL then the matrix inverse has already been computed so we simply return
      ## the previously cached data and exit the function
      if(!is.null(m)) {
            message("getting cached data")
            return(m)
      }
      
      ## otherwise we need to caculate the inverse and cache the results in the parent function
      ## for possible later use.
      
      ## get the initial square matrix
      data <- x$get()
      
      ## find the matrix inverse using the built-in 'solve' function 
      m <- solve(data, ...)
      
      ## save the solved matrix inverse in the parent function
      x$setsolve(m)
      
      ## return the solved inverse matrix (for printing?) and exit
      m
}
