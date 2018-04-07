## Put comments here that give an overall description of what your
## functions do


## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
m_inv <- NULL

setmat <- function(y) {
  x <<- y
  m_inv <<- NULL
}
getmat <- function() x
setmatinv <- function(minv) m_inv <<- minv
getmatinv <- function() m_inv

list(setmat = setmat ,getmat = getmat, setmatinv = setmatinv, getmatinv = getmatinv)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve 
## should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

m_inv <- x$getmatinv()

if(!is.null(m_inv)) {
  message("getting cached data")
  return(m_inv)
  }

data <- x$getmat()
m_inv <- solve(data, ...)
x$setmatinv(m_inv)
m_inv

}

# To test above use the matrix below (obtained from 
# https://cran.r-project.org/web/packages/matlib/vignettes/inv-ex1.html:\)

#A <- matrix( c(5, 1, 0,
#               3,-1, 2,
#               4, 0,-1), nrow=3, byrow=TRUE)


# Then run lines below:

#amatrix <- makeCacheMatrix(A)
#cacheSolve(amatrix)
#amatrix$getmatinv()
