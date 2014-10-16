# The functions defined below create, solve and cache   
# matrices. Called again the cacheSolve function will 
# supply the previous solution from cache, if and only if
# the matrix supplied to the function has not changed.

# makeCacheMatrix creates an environment for a supplied 
# matrix with 4 functions that provide caching functions.
# Input: makeCacheMatrix(matrix)
# Output: a list of 4 functions working on the provided 
# matrix. The functions are get, set, setInv and getInv,
# none of these should be called by the user. 

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    
    set <- function(mat) {
        x <<- mat
        inv <<- NULL
    }
    get <- function() x
    setInv <- function(inverse) inv <<- inverse
    getInv <- function() inv
    
    list(set = set, get = get,
         setInv = setInv, getInv = getInv)
}


# cacheSolve is the "solve" equivalent for matrices in 
# the environment setup by makeCacheMatrix, it provides 
# the additional functionality of caching the result, and
# retrieving already cached results to and from memory,
# respectively.
# Input: cacheSolve(object from makeCacheMatrix)
# Output: The inverse of the matrix

cacheSolve <- function(m, ...) {
    sol <- m$getInv()
    if(!is.null(sol)) {
        message("Solved from cache")
        return(sol)
    }
    data <- m$get()
    sol <- solve(data, ...)
    m$setInv(sol)
    sol
}

## Test cases:
# matricen <- matrix(c(1:8,15),3,3)
# matricen
# solve(matricen)
# m <- makeCacheMatrix(matricen)
# cacheSolve(m)
# cacheSolve(m)
# 
# matricen <- matrix(c(11:18,15),3,3)
# solve(matricen)
# m <- makeCacheMatrix(matricen)
# cacheSolve(m)
# cacheSolve(m)

# Technically, the "set" subfunction of the makeCacheMatrix
# function is not necessary, but i've kept it in for symmetry
# with the example.