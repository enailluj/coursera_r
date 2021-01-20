makeCacheMatrix <- function(x = matrix()) {
  inverse_Matrix <- NULL
  
  set <- function(y){
    x <<-y
    inverse_Matrix <<- NULL
  }
  
  get <- function(){ x
                        
    inverse_set <- function(inverse) inverse_Matrix <<- inverse
    inverse_get <- function() inverse_Matrix
    list(set = set,get=get,inverse_set=inverse_set,inverse_get=inverse_get)
  }
   
}


cacheSolve <- function(x, ...) {
        
  inverse_Matrix <- x$inverse_get()
  if (!is.null(inverse_Matrix)){
    message("data is being cached")
    return(inverse_Matrix)
  }
  mat <- x$get()
  inverse_Matrix <- solve(mat,...)
  x$setInverse(inverse_Matrix)
  inverse_Matrix
}
