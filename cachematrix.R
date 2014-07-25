## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix<- function(X = matrix()) {
  inv <- NULL
  Matrix <- function(Y) {
    X <<- Y
    inv <<- NULL
  }
  Mat <- function() X
  Inversa <- function(solve) inv <<- solve
  Inver <- function() inv
  list(Mtatrix = Matrix, Mat = Mat, Inversa = Inversa, Inver = Inver)
}


## Write a short comment describing this function

cacheSolve <- function(X) {
  inv <- X$Inver()
  if(identical(inv, solve(X$Mat()))) {
    message("Obtención de inversa en caché")
    return(inv)
  }
  else{
    Matr <- X$Mat()
    inv <- solve(Matr)
    X$Inversa(inv)
    return(inv)
  }
}
