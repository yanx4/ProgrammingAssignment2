## Compute inverse of a matrix and Cache.

## This function creates a special "matrix" that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
     i<-NULL  ##provides a default NULL to matrix inverse "i".
      
     set<-function(y){ ##set the value of the matrix
          x<<-y ##caches the inputted matrix to x
          i<<-NULL ##sets the matrix inverse to NULL
     }
     get<-function() x ##get the cache matrix
     
     setinv<-function(inv)  i<<-inv ##set the matrix inverse to i
     getinv<-function() i  ##get the matrix inverse
     list(set=set,get=get,setinv=setinv,getinv=getinv) ##save the 4 functions into a list
}

##This function computes the inverse of the special "matrix" returned by makeCacheMatrix
##above. If the inverse has already been calculated(and the matrix has not changed),
##then the cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x,b,...) {
      i<-x$getinv()  ## assign the cached matrix inverse to i
      y<-b  ## assign current matrix b to y
      
##check if the inverse has been calculated and the cache matrix x$get() is identical to current matrix y
## if yes, print out the message "getting cached data" and return the calcualted cache matrix inverse i
      if(!is.null(i)&&identical(x$get(),y)){
         message("getting cached data")
         return(i)
      }
##Otherwise, reset the cache matrix x$set(y) and calculate the current matrix's inverse and cache.
      x$set(y) 
      data<-x$get()
      i<-solve(data,...)
      x$setinv(i)
      i
}
