## la funcion halla la matriz inversa a la dada

makeCacheMatrix <- function(x = matrix()) {
  mat <- NULL
  set <<- function(y){
    x <<- y
    mat <<- NULL
  }
  get <- function() x
  inv1 <- function(sol) mat <<- sol
  inv2 <- function() mat
  list(set=set, get=get, inv1=inv1, inv2=inv2)
}


## la funcion guarda la matriz en la cache para no calcularla cada vez que se use

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  mat <- x$getinverse()
  if(!is.null(mat)){
    return(mat)
  }
  data <- x$get()
  mat <- solve(data, ...)
  x$setinverse(mat)
  mat
}
