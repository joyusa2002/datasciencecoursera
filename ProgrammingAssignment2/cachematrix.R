# This code was written by Joy Mehta to test the inverse caching concept 

joyMatrix <- function(x = matrix()) {
        i <- NULL ## Creating a blank matrix to store Inverse
        set <- function(y) {
                x <<- y  ## Assign new matrix
                i <<- NULL
        }
        get <- function() x
        ##Inverse calculation
        setsolve <- function(solve) i <<- solve 
        
        #Cache value
        getsolve <- function() i 
        
        ##Store the values in a list and return the list
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}

## Caching function
cacheSolve <- function(x, ...) {
        i <- x$getsolve() #Check if matrix x is cached
        if(!is.null(i)) {
                message("getting cached data") # get the cached value
                return(i)
        }
        data <- x$get() #Calculating inverse
        i <- solve(data, ...) 
        x$setsolve(i) #Caching Inverse data
        i
}

##Test Cases
x <- makeCacheMatrix(matrix(10:14,2))
z<-x$get() ##Get the matrix created
z

cacheSolve(x) ## Get the inverse
cacheSolve(x) ## Check if cached inverse is returned

#Test if inverse is correct
y<-x$getsolve()
y %*% z ##Should be identity matrix