## Put comments here that give an overall description of what your
## functions do

## creates a special "vector"
##takes argument matrix, x, for the function makeCacheMatrix
## inv represents null objects
makeCacheMatrix <- function(x= matrix()){
  inv <- NULL
  set <- function(y){# set value of matrix by setting another function
    x <<- y
    inv <<- NULL#double arrow assignment is a closure function that encloses environment of the parent fucntion
  } #enables two rounds of parameter, capable of modifiying variables in parent
  get <- function() {x} #set the value of the matrix
  setInverse <- function(inverse) {inv <<- inverse} #set the value of the inverse
  getInverse <- function() {inv} #need to get the value of the invsere
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
} #provide list

## computes inverse of matrix and create with aformentioned matrix
## Return a matrix that is the inverse of 'x'
cachesolve <- function(x, ...){
  inv <- x$getInverse() ##matrix that is inverse of x and assign to inv
  if(!is.null(inv)){ ## see if inverse is already calculated, if so, can get the inverse
    message("getting cashed data")
    return(inv) 
  }
  mat <- x$get()#compute the inverse of matrix and set the matrix the way we want
  inv <- solve(mat, ...)#using standard R function to get the answer
  x$setInverse(inv)#set inverse function gets the value we want
  inv#concludes the function
}



