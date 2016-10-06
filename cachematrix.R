## Matrix inversion can be big computatuion specially with big
## matrixes or you need to compute the inverse repeatedly.

## The first funchion makeCacheMAtrix will inverse a matrix.
## First the function sets the value of the matrix, then gets the value
## Next the function sets the value of inverse of the matrix,
## and then gets the value of the matrix

makeCacheMatrix <- function(x = matrix()) {
        inver <- NULL
        set <- function(y) {
                x <<-y
                inver <<- NULL
        }
        get <- function() x
        setinver <- function(inverse) inver <<- inverse
        getinver <- function() inver
        list(set=set, get=get, setinver=setinver, getinver=getinver)
}


## This function returns the inverse of a matrix.
## It checks if the inverse has already been computed, if so it gives the inverse.
## If not, it computes the inverse, sets the value in the makeCacheMatrix function
## through the setinver function in the makeCacheMatrix function above.

cacheSolve <- function(x, ...) {
        inver <- x$getinver()
        if(!is.null(inver)) {
                message("fetching cached data.")
                return(inver)
        }
        data <- x$get()
        inver <- solve(data)
        x$setinver(inver)
        inver
}
