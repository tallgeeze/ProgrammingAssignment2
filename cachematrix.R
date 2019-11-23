##Hi,thank you for auditing my assignment, wish you a nice day.
## functions discribution:
In this function 
R.version
matrix_buffer <- NULL
makeCacheMatrix <- function(x = matrix()) {
  matrix_inv <- NULL
  init_matrix <-function()
  {
    matrix_buffer <<- x
    matrix_Inv<<- NULL  
  }
  init_matrix()
  bind_inv <- function(temp_inv){matrix_inv <<- temp_inv}
  get_matrix <- function() matrix_buffer 
  get_matrix_inv <- function() matrix_inv
  list(init_matrix = init_matrix, bind_inv=bind_inv, get_matrix=get_matrix,get_matrix_inv= get_matrix_inv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  matrix_input <-makeCacheMatrix(x)  
  matrix_buffer<-matrix_input$get_matrix()
  matrix_inv <- matrix_input$get_matrix_inv()
  if(!is.null(matrix_inv)){return(matrix_inv)}
  
  matrix_inv <- inv(matrix_buffer)
  matrix_input$bind_inv(matrix_inv)
  matrix_inv
}
