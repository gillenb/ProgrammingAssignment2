## This following is an example of how scoping rules in R can be used 
## to preserve state inside of an R object. In this code, there are a   
## pair of functions that, together, cache the inverse of a matrix.
##_________________________________________________________________
## makeCacheMatrix() creates a special matrix object that can
## cache its inverse.  The actual calculation of the inverted matrix
## is done outside of makeCacheMatrix(), in a function called cacheSove().
## makeCacheMatrix returns a list of functions called set(),get(),setInverted(),
## and getInverted().  
makeCacheMatrix <- function(x = matrix()) { 
  invertedMatrix <- NULL  ##initialize the inverted matrix cache to NULL
  message("The inverted matrix cache has been initialized to NULL.")
  
  set <- function(y) {##set() records the matrix passed to it 
    x <<- y   ##assigns the value received by set() to the matrix 'x' in makeCacheMatrix
    message("The matrix to be inverted has been recorded.")
    invertedMatrix <<- NULL
    message("The inverted matrix cache has been set to NULL.")
  } 
  
  get <- function() {##get() returns the matrix stored in the X variable of makeCacheMatrix 
    return(x)
  }
  
  setInverted <- function(inverted) {
    ##setInverted() assigns what is passed to it to the variable invertedMatrix of makeCacheMatrix
    invertedMatrix <<- inverted
    message("setInverted() has successfully cached the inverted matrix.")  
  }
  
  getInverted <- function() {
    ##getInverted() retrieves what is stored in variable invertedMatrix of makeCacheMatrix
    message("getInverted() is retrieving what is stored in the inverted matrix cache...")
    invertedMatrix
    
  }
  message("The matrix to be inverted has been recorded.")
  message("The functions set(),get(),setInverted() and getInverted() have been defined.")
  
  list(set = set, get = get,
       setInverted = setInverted,
       getInverted = getInverted)
  
} 


## The following function does the actual calculation of the 
## inverted matrix.  makeCacheMatrix() must have first been called
## in order to create the functions needed within cacheSolve()
## This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has already 
## been calculated (and the matrix has not changed), then the 
## cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) { 
  ## Return a matrix that is the inverse of 'x' 
  ## Assumes that the matrix 'x' is a square invertable matrix
  invertedMatrix<-x$getInverted()  ## retreive what is stored in cache 
  
  if(!is.null(invertedMatrix)) {##Checking to see if the inverted matrix cache contains a non-NULL matrix
    message("It appears that the inverted matrix is, in fact, already cached.")
    message("Now returning the cached inverted matrix.")
    return(invertedMatrix)
  }
  message("Unfortunately, the inverted matrix is not cached.  Attempting inversion using solve()...")
  if(class(try(solve(x$get()),silent=TRUE))=="try-error") {##test if error occurs when trying to do inversion
    print("Sorry, the provided matrix is not invertable. The inverted matrix cache is set to NULL")
    invertedMatrix <- NULL  ##Since matrix cannot be inverted, reset inverted matrix cache to NULL
  }
  else {##No error occurs when trying inversion (using "try"), so proceeding with the inversion...
    invertedMatrix <- solve(x$get())
    message("The inverted matrix has been successfully computed using solve().")
    message("Attempting to cache the newly computed inverted matrix using setInverted()...")
    x$setInverted(invertedMatrix)  ##assign newly computed inverted matrix to invertedMatrix in makeCacheMatrix
    invertedMatrix  ##returns invertedMatrix
  }
  
} 




