## These two functions will be used to calculate and store the inverse of a
## matrix
## The makeCacheMatrix function creates a special "matrix" object that 
## will be stored, which is a list containing a function to
##       1.Set the matrix
##       2. Get the value of the matrix
##       3. Set the value of the inverse
##       4. Get the value of the Inverse


makeCacheMatrix <- function(x = matrix()) {
        # storage space for inverse Matrix
        invMat<- NULL
        
        # place matrix in the working environment
        set <-function(y){
                x <<- y
                invMat<<- NULL
        }
        get<- function() x
        setInv<- function(inverse) invMat<<- inverse
        getInv<- function() invMat
        
        # return the above functions to working environment
        list(set = set, get = get, setInv = setInv, getInv = getInv)
}



## This function computes the inverse of the special "matrix" stored in the
## makeCacheMatrix function
##        
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        invMat <- x$getInv()      
        
        # if inverse matrix has been computed, return the value from cache
        if(!is.null(invMat)){           
                message("getting cached data")
                return(invMat)
        }
        # if inverse hasn't been computed, compute it
        data<- x$get()
        invMat<- solve(data,...)
        
        # store inverse in cache
        x$setInv(invMat)
        return(invMat)
}

