## The following pair of functions computes the inverse of an invertible matrix. Instead of computing the inverse repeatedly,
## the functions help the process by caching the inverse and first check its value in the cache on subsequent requests before
## proceeding with a computation.


## The function makeCacheMatrix() creates and returns a list of functions aimed at performing the following set and
##	retrieve tasks (note that no computation takes place within this function):
##		1. set the values of the matrix
##		2. get the value of the matrix
##		3. set the value of the inverse of input matrix
##		4. get the value of the inverse matrix
##
##	Input: a matrix (assume an invertible matrix)
##	Output: a matrix object that consists of a list of functions able to manipulate cached matrix variables

makeCacheMatrix <- function(mx = matrix()) {
	im <- NULL   ## set the inverse matrix to NULL
        set <- function(y) {    ## changes the matrix stored in the main function
                mx <<- y		   ##  substitutes the matrix mx with a new matrix y provided as the argument (in the parent environment)
                im <<- NULL        ##  restores to null the value of the inverse matrix im (in the parent environment),
                                   ##  because the old inverse matrix of the old matrix is not needed anymore 
                                   ##  The new matrix needs to be recalculated through the function cacheSolve

        }
        get <- function() mx     ## return the matrix x stored in the main function. No input necessary

        ## Functions setinverse and getinverse don't calculate the inverse, 
        ## they simply store the value of the input in a variable im into the main function makeCacheMatrix (setinverse), 
        ## and then return it (getinverse)
        setinverse <- function(inverse) im <<- inverse    ## assign inverse matrix to im (in the parent environment)
        getinverse <- function() im     ## return stored inverse matrix
        list(set = set, get = get,		## create and return a list of functions
             setinverse = setinverse,
             getinverse = getinverse)

}


## The function cacheSolve computes the inverse of the matrix object returned by makeCacheMatrix(). If the inverse already exists, do not
##  calculate it, instead return the cached inverse matrix
##
##	Input: a matrix object containing a list of functions that can manipulate cached variables
##  Output: the inverse of a matrix that can be accessed in the cache via the input object (list of functions)

cacheSolve <- function(mxlist, ...) {
        ## Return a matrix that is the inverse of the input matrix 'mx'
        im <- mxlist$getinverse()	## retrieve the cached inverse matrix

        ##	Check if the cached inverse matrix already exists and verify it is not NULL, then return its value
        if(!is.null(im)) {
                message("getting cached data")
                return(im)
        }

        ## 	Othervise, if the cached matrix is NULL, compute the inverse matrix obtained through the input matrix object mx
        data <- mxlist$get()	## retrieve the values of the matrix out of the matrix object
        im <- solve(data, ...) 	## compute the input matrix' inverse (assume it is invertible) (solve(data, ...) %*% data to verify)
        im
}
