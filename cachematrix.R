## This file has 2 functions makeCacheMatrix and cacheSOlve
## one creates a special matrix object and other caluclates the inverse matrix adn stores in cache

## This function creates a special matrix object
## which has 4 functions
## setMatrix to set the matrix object, preferable square matrix
## getMatrix returns the matrix that was stored previously
## setInverseMatrix sets inverse matrix for a given matrix
## getInverseMatrix gets the inverse matrix that was stored previously

makeCacheMatrix <- function(mat = matrix()) {
  
  inv_mat <- NULL
  
  setMatrix <- function(new_mat) {
    mat <<- new_mat
    inv_mat <<- NULL
  }
  
  getMatrix <- function() mat
  
  setInverseMatrix <- function(inv) inv_mat <<- inv
  
  getInverseMatrix <- function() inv_mat
  
  list(setMatrix = setMatrix, getMatrix = getMatrix,
       setInverseMatrix = setInverseMatrix,
       getInverseMatrix = getInverseMatrix)

}


## This function recieves mat as input which is a matrix
## checks for inverse matrix in the object
## if it is not found then inverse matrix is caluclated using solve function
## inverse matrix is stored in object as well as returned back

cacheSolve <- function(mat, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv_mat <- mat$getInverseMatrix()
  
  if(!is.null(inv_mat)) {
    message("getting cached matrix")
    return(inv_mat)
  }
  
  data <- mat$getMatrix()
  
  inv_mat <- solve(data)
  
  mat$setInverseMatrix(inv_mat)
   
  inv_mat
}

