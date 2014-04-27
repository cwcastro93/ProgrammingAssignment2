##This function creates a special "matrix" object that can cache its inverse


##This part of the function set and get the value of the matrix  and
## set and get the value of the inverse of the matrix 


makeCacheMatrix<- function(x=matrix())  {
        inv<-NULL
        set<-function(y) {
                x<<-y
                inv<<-NULL
        }
        get<-function() x
        setinverse<- function(inverse) inv<<-inverse
        getinverse<- function() inv
        list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
        
}

##Computes the inverse has already been calculated (and the matrix has not changed),
##then the cachesolve should retrieve the inverse from the cache

cacheSolve<- function(x, ...)  {
        inv<- x$getinverse()
        if (!is.null(i)) {
                message("getting cached data")
                return(inv)
        }
        data<- x$get()
        inv<- solve(data, ...)
        x$setinverse(inv)
        inv
}
