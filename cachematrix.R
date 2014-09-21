## Function that creates a special matrix object which is in reality is a list

makeCacheMatrix <- function(x = matrix()) {
m <- NULL
	setmatrix <- function(y){
		x <<- y
		m <<- NULL
	}
	getmatrix <- function() x
	setmatrinv <- function(solve) m<<- solve
	getmatrinv <- function() m
	list(setmatrix=setmatrix, getmatrix=getmatrix, setmatrinv=setmatrinv, getmatrinv=getmatrinv)
}


## Function that returns the inverse of the matrix cretated with the makeCacheMatrix function
## If the cached inverse of the matrix has previously been computed, ie, is available, the cacheSolve function retrieves it
## If not, cathe cacheSolve function computes, caches and returns it

cacheSolve <- function(x=matrix(), ...) {
	m<-x$getmatrinv()
	if(!is.null(m)){
		message("getting cached matrix inverse data")
		return(m)
		} else {
			m <- solve(x$getmatrix())
			x$setmatrinv(m)
			return(m)
			}
	}

##I run the functions with the following inputs
#mat <- matrix(data = c(1,-.25,-.25,1), nrow = 2, ncol = 2)
#mat2 <- makeCacheMatrix(mat)
#cacheSolve(mat2)
