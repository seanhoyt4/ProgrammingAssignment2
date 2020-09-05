# The purpose of the following functions is to create a special matrix object
# that allows the user to calculate the inverse of the matrix and store both
# the matrix and the inverse matrix in the cache without having to recompute the 
# inverse as the scope changes

# makeCacheMatrix: creates a special matrix object that allows a user to set a
#                       matrix, get the matrix, set the inverse of the matrix, 
#                       and get the inverse matrix 
#
# Inputs:        x: a square matrix
# Outputs:       the special matrix object containing function to set and get
#                the matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
        
        # NULL the inverse
        inv <- NULL
        
        # Create function to set the matrix
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        
        # Create function to get the matrix
        get <- function() x
        
        # Create a function to set the inverse
        setInv <- function(myInverse) inv <<- myInverse
        
        # Create a function to get the inverse
        getInv <- function() inv
        
        # Create the special matrix object
        list(set = set, get = get, setInv = setInv, getInv = getInv)
        
}


# cacheSolve: returns or calculates, sets, and returns the inverse of a matrix
#               using the special matrix object
#
# Inputs:       x: the special matrix object
# Outputs:      inv: the inverse of the matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        # Get the inverse stored in special matrix object
        inv <- x$getInv()
        
        # If the inverse already exists, return the inverse
        if(!is.null(inv)) {
                message('getting cached data')
                return(inv)
        }
        
        # Get the matrix from the special matrix object
        mat <- x$get()
        
        # Calculate the inverse of the matrix
        inv <- solve(mat)
        
        # Set the inverse of to matrix to the special matrix object
        x$setInv(inv)
        
        # Return the inverse
        inv
}