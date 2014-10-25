## Function that Creates a special "matrix" object that can cache its inverse
 

 makeCacheMatrix <- function( m = matrix() ) 
 { 

 
 	## Initialize the inverse property 
     inve <- NULL 
 
 
     ## Method used to set the matrix 
     ## Nullifies the inve variable
     set <- function( matrix ) 
     { 
             m <<- matrix 
             inve <<- NULL 
     } 
 
 
     ## Method to get the matrix 
     get <- function() 
     { 
     	## Return the matrix 
     	return(m) 
     } 
 
 
     ## Method to set the inverse of the matrix 
     setInverse <- function(inverse) 
     { 
         inve <<- inverse 
     } 
 
 
     ## Method to get the inverse of the matrix 
     getInverse <- function() 
     { 
         ## Return the inverse property 
         return(inve) 
     } 
 
 
     ## Return a list of the functions
     list(set = set, get = get, 
          setInverse = setInverse, 
          getInverse = getInverse) 
 } 
 
 
 
 
 ## Compute the inverse of the special matrix returned by "makeCacheMatrix" 
 ## above. If the inverse has already been calculated (and the matrix has not 
 ## changed), then the "cachesolve" should retrieve the inverse from the cache. 
 
 cacheSolve <- function(x, ...) 
 { 
 
 
     ## Return a matrix that is the inverse of 'x' 
     m <- x$getInverse() 
 
 
     ## Just return the inverse from the cacheif its already set 
     if( !is.null(m) ) 
     { 
             message("getting cached data") 
             return(m) 
     } 
 
 
     ## Get the matrix from our object 
     matr_data <- x$get() 
 
 
     ## Calculate the inverse using solve 
     m <- solve(matr_data,...) 
 
     ## Set the inverse to the object 
     x$setInverse(m) 
 
 
     ## Return the matrix 
     return(m) 
 } 


## Usage Example:
## source("cachematrix.R")
## amatrix = makeCacheMatrix(matrix(c(1,2,3,4), nrow=2, ncol=2))
## amatrix$get()  #Returns original matrix
## cacheSolve(amatrix)   # Computes, caches, and returns matrix inverse
## amatrix$getInverse()  # Returns matrix inverse
## cacheSolve(amatrix)   # Returns cached matrix inverse using previously computed matrix inverse
## amatrix$set(matrix(c(0,5,99,66), nrow=2, ncol=2)) # Modify existing matrix
## cacheSolve(amatrix)   # Computes, caches, and returns new matrix inverse
## amatrix$get()         # Returns matrix
##  amatrix$getInverse()  # Returns matrix inverse

 