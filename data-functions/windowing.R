#' Windowing Function for Data
#'
#' This function partitions the input data into overlapping windows of a specified size.
#' The position of each window is determined by a moving step defined by `window_len`.
#' 
#' @param data A dataframe that you wish to partition into overlapping windows.
#' @param batch_size An integer that specifies the size of each window. Default is 100.
#' @param window_len An integer that specifies the step size for moving the window. Default is 5.
#' 
#' @return A list of dataframes, where each dataframe corresponds to a window of the original data.
#' 
#' @examples
#' data <- data.frame(x = rnorm(1000), y = rnorm(1000))
#' windowed_data <- windowing_f(data, batch_size = 100, window_len = 10)
#' @export
windowing_f <- function(data, batch_size = 100, window_len = 5){
  i <- 1
  left <- 1
  right <- batch_size
  df_list <- list()
  while(TRUE){
    df_list[[i]] <- data[left:right,]
    if(left > dim(data)[1] - batch_size){
      break
    }
    i = i + 1
    left = left + window_len
    right = right + window_len
  }
  return(df_list)
}