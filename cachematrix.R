##Hi,thank you for auditing my assignment, wish you a nice day.
## functions discribution:
# In this function, you may give a matrix as follows:
#   normal_matrix <- matrix(c(1,2,3,4), nrow=2)
# and you can test it with calling function:
# cache_matrix <- cacheSolve(normal_matrix), 
# it will check if Inv of this matrix is calculated
# if not , it will calculate Inverse of it and return Inverse matrix
# if yes, it will directly return its Inverse matrix

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



cacheSolve <- function(x, ...) {
 ## Return a matrix that is the inverse of 'x'
  matrix_input <-x 
  matrix_buffer<<-matrix_input$get_matrix()
  matrix_inv <- matrix_input$get_matrix_inv()
  if(!is.null(matrix_inv)){
    print("aha!") 
    return(matrix_inv)
    }
  
  matrix_inv <- inv(matrix_buffer)
  matrix_input$bind_inv(matrix_inv)
  matrix_inv
}
cat("
  Hello!Welcome.
  you may test with follow commands: 
  test <- makeCacheMatrix(n) 
  cacheSolve(test) 
  you can also visit test$get_matrix_inv() to see the cached inverse matrix \n
  **if you do cacheSolve(test) again, it will print 'aha!' and the Inverse from Cache **\n 
  you can try some other matrix to test, remember to name your new matrix as below: 
    cache_newmatrix <- makeCacheMatrix(your new matrix)
    then :cacheSolve(cache_newmatrix)")

test <- makeCacheMatrix(n)
cacheSolve(test)
test$get_matrix_inv()