#We want to make two functions
#One of them should do the followings:
#1. Set the value of matrix
#2. Get the value of matrix
#3. Set the value of inversion
#4. Get the value of inversion

#The second function should check if there is any value for that matrix or not

#First function: basically same as the example

makeCacheMatrix <- function(matrixvalue = matrix()){
    #checking to see if it is a matrix
    #we need to set the inverse of matrix equal to NULL
    matrixinversion <- NULL
    #set value
    set <- function(y){
        matrixvalue <<- y
        matrixinversion <<- NULL
    }
    
    #get value
    get <- function(){
        matrixvalue
    }
    
    #set inverse
    setinversion <- function(inversevalue){
        matrixinversion <<- inversevalue
    }
    
    #get the inversion
    getinversion <- function(){
        matrixinversion
    }
    list(set = set, get = get, setinversion = setinversion, getinversion = getinversion)
}

#Second function:
cacheSolve <- function(m,...){
    
    #using the getinversion function
    matrixinversion <- m$getinversion()
    #checking to see if it should be calculated or not
    if(!is.null(matrixinversion)){
        return(matrixinversion)
    }
    #Else:
    #use the get for getting the matrix
    specialmatrix <- m$get()
    #using the matrix to get the inverse
    matrixinversion <- solve(specialmatrix)
    #saving the value
    m$setinversion(matrixinversion)
    #returning the value
    return(matrixinversion)
    
}