# Step 1: Create a square matrix object to test the function by using the 
# matrix function
my2X2_matrix <- matrix(1:4, 2, 2)
my2X2_matrix # print object to confirm squared matrix
solve(my2X2_matrix) # verify that your matrix has an inverse

# Step 2: Create the makeCacheMatrix function based on the example provided for 
#caching the mean that both caches the matirx object and caches it's inverse

makeCacheMatrix <- function(x = numeric()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve  
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

# Step 3: Create a cache matrix object
cached_mat <- makeCacheMatrix(my2X2_matrix)  

# Step 4: Create the cacheSolve function using the example to solve for the 
# cached mean to either look for a cached object or solve the inverse of a matrix
cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}

# Test of the cached object
cacheSolve(cached_mat)
