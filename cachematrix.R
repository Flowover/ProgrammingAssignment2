## This function return the inverse matrice of an input one. To do that, it checks first
## if this matrice is already known and either calculate it or return the one recored.

## This fonction allow to recover the matrix stored in makeCacheMatrix function 
## with get() or to put another one with set()
## We can also have the inverse matrix which correspond to the x matrix stored in the main 
## function with getinv() and put another one with setinv(). The default inverse matrix is
## an empty matrix (dim=0,0).

makeCacheMatrix <- function(x = matrix()) {
        xinv<-matrix(nrow=0,ncol=0)
        
        set<- function(y){
                x<<-y
                xinv<<-matrix(nrow=0,ncol=0)
        }
        get <- function() x
        setinv<-function(y) xinv<<-y
        getinv<- function() xinv
        
        list(set=set,get=get,setinv=setinv,getinv=getinv)

}


## This function tests if the matrice input has already an inverse matrice associated thanks
## to the function getinv().
## If it has(dimensions of the inverse matrice are not 0), 
## the function cachesolve return this matrice. 
## If not, the function calculates the inverse matrice, associates it with the input 
## matrice with the function setinv() and return the inverse matrice.

cacheSolve <- function(x, ...) { ## Return a matrix that is the inverse of 'x'
   
        xinv<- x$getinv()   
   
   if(dim(xinv)[1]!=0 || dim(xinv)[2]!=0)
   {
           x$getinv()
           print("getting cached inverse matrix")
           return(xinv)
   }
        print("claculating the inverse matrix")
        matr<-x$get()
        xinv<-solve(matr)
        x$setinv(xinv)
        xinv
        
}
