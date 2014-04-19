## The two functions below taken together calculate the inverse of a matrix once,
## cache its inverse and then catch this inverse whenever it is needed.
# This procedure prevents repeated calculation of the inverse if the 
# contents of the matrix remain unmodified.
# cacheSolve performs the actual operations (inversing, caching and catching),
# while makeCacheMatrix creates the enclosure for the functions that will perform the operations for a given matrix.

## makeCacheMatrix takes a matrix as input argument.
# It returns a list with four functions
# set() assigns the value of the matrix
# get() gets the value of the matrix
# setinv() sets the value of the inverse
# getinv() gets the value of the inverse
# The following example code shows that the values of x and m when get()
# will be called are indeed the values of its enclosure.
#       > first_matrix <- matrix(rnorm(4),nrow= 2)
#       > first_matrix_list    <- makeCacheMatrix(first_matrix)
#       > identical(get("x", envir  = environment(first_matrix_list$get)),first_matrix)
#               [1] TRUE
#       > get("r", envir  = environment(first_matrix_list$set))
#       NULL
#The same results can be obtained for the other functions. 

makeCacheMatrix <- function(x = matrix()) {
        r <- NULL
        set <- function(y){
                x <<- y
                r <<- NULL
        }      
        get <- function() x
        setinv <- function(invmat) r <<- invmat
        getinv <- function() r
        list(set = set, get = get, setinv = setinv, getinv = getinv )
}


## cacheSolve takes the return value of makeCacheMatrix (the list of functions)
# as input argument.
# As a first step it "gets" the value of the inverse matrux
# When this is performed the first time, the inverse matrix does not exist yet,
# the functions "gets" the original matrix (i.e. gets the value as it was in the
# enclosure of get()), calculates its inverse,
# "caches" the value of the inverse (i.e. assigns the value of the inverse
# to the enclosure of setinv() ), and returns its value:
#       > solution <- cacheSolve(first_matrix_list)
#       > identical(get("r", envir  = environment(first_matrix_list$setinv)), solution)
#               [1] TRUE
# When the function is called next time, it "gets" the inverse (which has been
# cached in the first step), and returns it immediately, with a message clarifying
# that it is getting the "cached" version.
#       > solution2 <- cacheSolve(first_matrix_list)
#       getting cached data
#       > identical(get("r", envir  = environment(first_matrix_list$setinv)), solution2)
#       [1] TRUE

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        r <- x$getinv()
        if(!is.null(r)){
                message("getting cached data")
                return(r)
        }
       data <- x$get()
       r <- solve(data, ...)
       x$setinv(r) 
       r
}
