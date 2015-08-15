# Coursera
# R programming
# Programming Assignment 2: Lexical Scoping: Instructions
# https://class.coursera.org/rprog-031/human_grading/view/courses/975105/assessments/3/submissions

# submitted by: gopalkriz
# date: Sys.Date() 15Aug2015
#-----------------------------------#

setwd("D:/VIVEK/DataAnalysis/Coursera/2 Rprog/Rprog Assignment2 Lexical Scoping");getwd()

##Assignment: Caching the Inverse of a Matrix

#Write the following functions:
#1. makeCacheMatrix: function creates a special "matrix" object that can cache its inverse.
#2. cacheSolve: computes the inverse of the special "matrix" returned by makeCacheMatrix
# If the inverse has already been calculated (and the matrix has not changed),
# then the cachesolve should retrieve the inverse from the cache.
#
#if X is a square invertible matrix, then solve(X) returns its inverse

#-----------------------------------#

## Function that cache the inverse of a matrix

#-----------------------------------#

##  Create function list and Matrix

# Create a special "matrix", which is a list containing a function to
#1. set the value of the matrix
#2. get the value of the matrix
#3. set the value of the inverse matrix
#4. get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {	#(0) Input has to be a matrix.
	i <- NULL
	set <- function(y) {
		x <<- y 							#(1) This writes x in the global.
		i <<- NULL 							#(2) This writes i in the global.
	}
	get <- function() x						#(3) Redefines get() locally.
	setinverse <- function(inv) i <<- inv	#(4) sets inv value to variable in the global.
	getinverse <- function() i				#(5) gets inv value to variable.
	
	list( 							 		#(6) create list of command calls.
		set = set,
		get = get,
		setinverse = setinverse,
		getinverse = getinverse
	)
}


#-----------------------------------#

##  Reuse cache result or Calculate Inverse Matrix
#   Calculates the inverse of the special "matrix"created by above function
#1. first checks to see if the inverse has already been calculated / available in cache
#2. if yes, gets the inverse from the cache and skips the computation
#3. Otherwise, it calculates the inverse of the matrix 
#4. sets the value of the inverse in the cache via the setinverse function

cacheSolve <- function(x, ...) { 		#(0) Input has to be a matrix.
	i <- x$getinverse() 		 		
	if(!is.null(i)) { 		 		 	#(1) look for Inverse matrix in cache.
		message("getting Inverse of Matrix from the cached data")
		return(i)
	}
	m <- x$get() 		 		 	 	#(2) gets the matrix to local.
		message("please wait for calculation, the Inverse of Matrix not available in cache") 		 		 	
	i <- solve(m, ...)	 		 	 	#(3) solve() creates the transpose matrix assigned to local.
	x$setinverse(i)	 		 	 		#(4) sets the result to the cache.
	i	 		 	 		 		 	#(5) print the transpose matrix
}

#-----------------------------------#