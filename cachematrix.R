## Put comments here that give an overall description of what your
## functions do

## This function will be called by the second one and is the one that will give us de inverse

makeCacheMatrix <- function(x = matrix()) {
        y<-matrix((nrow=nrow(x), ncol=ncol(x))
        for(i in nrow(y)) {
                for(j in ncol(y)) {
                        if ( i!=j) {
                                y[i,j]<-0
                                j<-j+1
                        } else {
                                y[i,j]<-1
                                j<-j+1
                        }
                }
                i<-i+1
        }
        inverse<- x%*%y
return (inverse)
                
}


## this function will give us the result of the first one

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        answer<-makeCacheMatrix(x)
        return(answer)
}
