## Put comments here that give an overall description of what your
## functions do
## the code below allows us to cache-in a matrix. Since we are inverting the
## matrix, it will have to be a square matrix (we assume it is also invertible)
## We will create a matrix and pass it as parameter to the makeCacheMatrix function
## and store the returned object in a variable, say m1.

## We will then us the object m1 to find the inverse of the matrix. our fuctions
## would compute and return the inverse if not computed previously, else, the cached
## value would be returned.

## This function would take a (square and invertibe) matrix and perform the
## housekeeping work required to cache the matrix object so that its inverse
## can be readily fetched if it has been computed previously, saving us precious 
## computing time.
makeCacheMatrix <- function(x = matrix()) 
{
    m_Inv <- NULL           ## Inv does not exist initially
    set <- function(y)      ## cache the matrix when seen the first time
    {
        x <<- y
        m_inv <<- NULL      ## Inv does not exist initially (Obviously)
    }
    
    ## Return the cached object (matrix)
    get <- function() 
    {
        return (x)
    }
    
    ## Set the Inv matrix to the cached inv matrix
    setInv <- function(p_Inv) 
    {
        m_Inv <<- p_Inv
    }
    
    ## Return the cached inverted matrix
    getInv <- function() 
    {
        return(m_Inv)
    }
    
    ## List of functions available
    list(set = set, get = get, setInv = setInv, getInv = getInv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) 
{
    ## Return a matrix that is the inverse of 'x'
    
    ## Invoking the cached inverted matrix
    m <- x$getInv()
    
    if(!is.null(m)) 
    {
        ## Yeah!!! 
        message("getting cached data")
        return(m)
    }
    else
    {
        ## Ooops!! got nothing! inv was not catched yet
        message("Not cached computing for the first time")
    }
    
    ## If the matrix was cached, we will use that
    data <- x$get()
    m <- solve(data, ...)
    x$setInv(m)
    m
}


## Test case 1
##    > m1 <- matrix(c(2, 4, 6, 100, 200, 100, -56, 48, 50), 3, 3)
##    > m1
##         [,1] [,2] [,3]
##    [1,]    2  100  -56
##    [2,]    4  200   48
##    [3,]    6  100   50
##    > solve(m1)
##              [,1]       [,2]   [,3]
##    [1,]  0.081250 -0.1656250  0.250
##    [2,]  0.001375  0.0068125 -0.005
##    [3,] -0.012500  0.0062500  0.000
##    
##    > obj1 <- makeCacheMatrix(m1)
##    > cacheSolve(obj1)
##    Not cached computing for the first time
##              [,1]       [,2]   [,3]
##    [1,]  0.081250 -0.1656250  0.250
##    [2,]  0.001375  0.0068125 -0.005
##    [3,] -0.012500  0.0062500  0.000
##    > cacheSolve(obj1)
##    getting cached data
##              [,1]       [,2]   [,3]
##    [1,]  0.081250 -0.1656250  0.250
##    [2,]  0.001375  0.0068125 -0.005
##    [3,] -0.012500  0.0062500  0.000
##    > 

## Test case 2
##    Just kidding!!!