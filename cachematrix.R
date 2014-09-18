####### function to cache the matrix and its inverse ##########
makeCacheMatrix <- function(x = matrix()) 
	{
	###Set my_invers_matrix to NULL. This is output variable.
	my_inverse_matrix <- NULL
	###function to set input matrix
	set_matrix <- function(Input_matrix) 
		{
		x <<- Input_matrix
		my_inverse_matrix <<- NULL
		}
	### function to get matrix. 
	get_matrix <- function()
		{
		return (x)
		}
	### set inverse of matrix to cache
	setInversematrix <- function(Input_Inverse_matrix) 
		{
		my_inverse_matrix <<- Input_Inverse_matrix
		}
	### get inverse of matrix from cache
		getInversematrix<- function() 
		{
		my_inverse_matrix
		}
	list(set_matrix = set_matrix, get_matrix = get_matrix,
	setInversematrix = setInversematrix,
	getInversematrix = getInversematrix)
	}

#############function to get inverse of a matrix#############
cacheSolve <- function(x, ...) 
{
	###get inverse of function calculated earlier(if not null)
	my_inverse_matrix <- x$getInversematrix()
	### check if the inverse is already calculated
	if(!is.null(my_inverse_matrix)) 
		{
		message("Fetching result from cache : Matrix inverse")
		return(my_inverse_matrix)
		}
	### get the matrix
	my_matrix <- x$get_matrix()
	### calculate inverse using solve function
	my_inverse_matrix <- solve(my_matrix, ...)
	### set inverse matrix to cache
	x$setInversematrix(my_inverse_matrix)
	return(my_inverse_matrix)
	}
