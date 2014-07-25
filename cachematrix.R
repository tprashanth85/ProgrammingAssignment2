## The two below functions are developed to simulate caching for calculating the inverse of a
## matrix. makeCacheMatrix creates a special object that can be used to set and get matrix and
## to set and get inverse of matrix. cacheSolve function computes the inverse of the 
## special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated 
## and if the matrix is unchanged, then the cachesolve will retrieve the inverse from the cache.


## This function creates a special matrix object 
## which is really a list containing a function to
## set the value of the matrix
## get the value of the matrix
## set the value of the inverse
## get the value of the inverse

## To run the functions below commands can be tested:
## a<-makeCacheMatrix()
## x<-matrix(1:4,nrow=2,ncol=2)
## y<-matrix(5:8,nrow=2,ncol=2)
## a$setMatrix(x)
## cacheSolve(a)
## cacheSolve(a,x)
## cacheSolve(a,y)

makeCacheMatrix <- function(matx = matrix())
{
  inver <- NULL
  setMatrix <- function(maty)
  {
    ##This function sets the value of the matrix
    matx <<- maty
    inver <<- NULL
  }
  getMatrix <- function()
  {
    ## This function gets the value of the matrix
    matx
  }
  setInverse <- function(inv)
  {
    ## This function sets the inverse of the matrix
    inver <<- inv
  }
  getInverse <- function()
  {
    ## This function gets the inverse of the matrix
    inver
  }
  ## list of all the functions is returned by makeCacheMatrix to an object
  list(setMatrix= setMatrix, getMatrix = getMatrix, setInverse = setInverse, getInverse = getInverse)
}

cacheSolve <- function(x,...)
{
    inv <- x$getInverse() ##Get the inverse if already calculated
    classlist <- list(...) ##Get the list of arguments passed to cacheSolve
    if(length(classlist) >=1) ##If arguments are passed, then new matrix is provided
    {
        mata <- classlist[[1]]
    }
    else
    {   ## No arguments passed means that the existing matrix needs to be used
        mata <- x$getMatrix()
    }
    matb <- x$getMatrix()
    if(is.matrix(mata) && dim(mata)==dim(matb) && all(mata==matb) && (!is.null(inv)))
    {
      ## Compare the existing matrix with the new matrix, if they are same print cached inverse
        message("No Change in Matrix")
        message("Getting cached inverse")
        return(inv)
    }
    else
    {
      ##If existing matrix and new matrix are different, calculate inverse for new matrix
        message("New Matrix")
        message("Calculating Inverse")
        inv <- solve(mata)
        x$setInverse(inv)
        inv
    }        
  
}
