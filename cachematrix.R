## Function that creates a special matrix object in the form of a list

makeCacheMatrix <- function(x = matrix()) {
	# sets the value of mi the inverse of the matrix to NULL
	# NULL is the default value of im before cacheSolve is used 
	mi <- NULL
	# function that sets the value of the matrix 
	setmatrix <- function(y){
		x <<- y # caches the inputted matrix so that cacheSolve can check whether it already exits
		mi <<- NULL # sets the value of mi the inverse of the matrix to NULL
	}
	# function that gets the value of the matrix
	getmatrix <- function() x
	# function that sets the value of the matrix inverse
	setmatrinv <- function(solve) mi<<- solve # solve is the R function that computes the inverse of a matrix
	# function that gets the value of the matrix inverse
	getmatrinv <- function() mi
	# special vector in the form of a list that contains the data on the inputted matrix and its inverse 
	list(setmatrix=setmatrix, getmatrix=getmatrix, setmatrinv=setmatrinv, getmatrinv=getmatrinv)
}


## Function that returns the inverse of the matrix cretated with the makeCacheMatrix function
## If the cached inverse of the matrix has previously been computed, ie, is available, the cacheSolve function retrieves it
## If not, cathe cacheSolve function computes, caches and returns it

cacheSolve <- function(x=matrix(), ...) {
	# get the inverse of the matrix and put it in mi using the getmatrinv function
	mi <- x$getmatrinv()
	# check if a cached matrix inverse already exists
	# if a cached value of the matrix inverse already exists then return it
	if(!is.null(mi)){
		message("getting cached matrix inverse data")
		return(mi) # return the cached matrix inverse
		} else {
			# if a cached value does not exist yet then compute the inverse of the inputted matrix using the getmatrix function
			mi <- solve(x$getmatrix())
			# put the inverse of the matrix in mi using the setmatrinv function
			x$setmatrinv(mi)
			return(mi)
			}
	}

##I run the functions with the following inputs
#matrin <- matrix(data = c(1,-.25,-.25,1), nrow = 2, ncol = 2)
#matrin2 <- makeCacheMatrix(matrin)
#cacheSolve(matrin2)
