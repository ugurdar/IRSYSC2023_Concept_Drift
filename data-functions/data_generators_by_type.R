#' Generate Data with Sudden Drift
#'
#' This function generates binary data with a sudden drift.
#' The drift is either induced by changing the mean or the coefficient, based on the `COEF_VAR_1`.
#'
#' @param STREAM_LENGTH_TOTAL Integer. Total length of the data stream. Default is 1200.
#' @param NUMBER_OF_BATCH Integer. Number of batches the data is divided into. Default is 2.
#' @param COEF_VAR_1 Vector or NULL. Coefficients to be applied for generating binary data.
#'
#' @return A dataframe with data exhibiting a sudden drift.
#' 
#' @export
data_sudden_drift <- function(STREAM_LENGTH_TOTAL = 1200,
                              NUMBER_OF_BATCH = 2,
                              COEF_VAR_1) {
  BATCH_SIZE = STREAM_LENGTH_TOTAL / NUMBER_OF_BATCH
  data_merged <- data.frame()
  for (i in 1:NUMBER_OF_BATCH) {
    if (is.null(COEF_VAR_1)) {
      data_created <- data_gen_bin(
        n  = BATCH_SIZE,
        b1 = 1,
        x1_mean = 100,
        x1_mean_coef = ifelse(i == 1,
                              1,2)
      )
    }else{
      data_created <- data_gen_bin(n = BATCH_SIZE, b1 = COEF_VAR_1[i])
    }
    
    data_merged <- rbind(data_merged, data_created)
  }
  return(data_merged)
}

#' Generate Data with Gradual Drift
#'
#' This function generates binary data with a gradual drift.
#' Drift characteristics depend on the specified parameters.
#'
#' @param STREAM_LENGTH_TOTAL Integer. Total length of the data stream. Default is 1200.
#' @param NUMBER_OF_BATCH Integer. Number of batches the data is divided into. Default is 30.
#' @param VAR1_MEAN_COEF Numeric. Coefficient for the variable mean. Default is 2.
#' @param COEF_VAR_1 Vector or NULL. Coefficients to be applied for generating binary data.
#'
#' @return A dataframe with data exhibiting a gradual drift.
#' 
#' @export
data_gradual_drift <- function(STREAM_LENGTH_TOTAL = 1200,
                               NUMBER_OF_BATCH = 30,
                               VAR1_MEAN_COEF = 2,
                               COEF_VAR_1) {
  BATCH_SIZE = STREAM_LENGTH_TOTAL / NUMBER_OF_BATCH
  data_merged <- data.frame()
  for (i in 1:NUMBER_OF_BATCH) {
    if (i %in% c(3,5, 9, 10, 13, 14, 15, 17:NUMBER_OF_BATCH)) {
      if (is.null(COEF_VAR_1)) {
        data_created <- data_gen_bin(n = BATCH_SIZE,
                                     x1_mean = 100,
                                     x1_mean_coef = VAR1_MEAN_COEF)
      }else{
        data_created <- data_gen_bin(BATCH_SIZE,b1 = COEF_VAR_1[1])
      }

      data_merged <- rbind(data_merged, data_created)
    } else{
      if (is.null(COEF_VAR_1)) {
        data_created <- data_gen_bin(n = BATCH_SIZE,
                                      x1_mean = 100)
      }else{
        data_created <- data_gen_bin(BATCH_SIZE,b1 = COEF_VAR_1[2])
      }
      data_merged <- rbind(data_merged, data_created)
    }
  }
  return(data_merged)
}

#' Generate Data with Incremental Drift
#'
#' This function generates binary data with an incremental drift.
#' The characteristics of the drift are based on the specified parameters.
#'
#' @param STREAM_LENGTH_TOTAL Integer. Total length of the data stream. Default is 1200.
#' @param NUMBER_OF_BATCH Integer. Number of batches the data is divided into. Default is 30.
#' @param COEF_VAR_1 Vector or NULL. Coefficients to be applied for generating binary data.
#' @param VAR1_MEAN_COEF Numeric. Coefficient for the variable mean. Default is 2.
#'
#' @return A dataframe with data exhibiting an incremental drift.
#' 
#' @export
data_incremental_drift <- function(STREAM_LENGTH_TOTAL = 1200,
                                   NUMBER_OF_BATCH = 30,
                                   COEF_VAR_1,
                                   VAR1_MEAN_COEF = 2) {
  BATCH_SIZE = STREAM_LENGTH_TOTAL / NUMBER_OF_BATCH
  data_merged <- data.frame()
  for (i in 1:NUMBER_OF_BATCH) {
    if (i %in% 1:10) {
      if(is.null(COEF_VAR_1)) {
        data_created <- data_gen_bin(n = BATCH_SIZE,
                                     x1_mean = 100)
      }else{
        data_created <- data_gen_bin(n = BATCH_SIZE,
                                     b1 = COEF_VAR_1[1])
      }
      data_merged <- rbind(data_merged, data_created)
      
    }else if (i %in% 11:20) {
      if (is.null(COEF_VAR_1)) {
        data_created <- data_gen_bin(n = BATCH_SIZE,
                                     x1_mean = 100,
                                     x1_mean_coef =  (i * 0.1))
      }else{
        data_created <- data_gen_bin(n = BATCH_SIZE,
                                     b1 = (COEF_VAR_1[1] + COEF_VAR_1[2])/2
                                     )
        
      }
      data_merged <- rbind(data_merged, data_created)
      
    }else{
      if (is.null(COEF_VAR_1)) {
        data_created <- data_gen_bin(n = BATCH_SIZE,
                                       x1_mean = 200 )
        
      }else{
        data_created <- data_gen_bin(BATCH_SIZE,
                                     b1 = COEF_VAR_1[2])
        
      }


      data_merged <- rbind(data_merged, data_created)
    }
  }
  return(data_merged)
}

#' Generate Data with Recurring Drift
#'
#' This function generates binary data with recurring drifts.
#' The drift characteristics are determined based on the specified parameters.
#'
#' @param STREAM_LENGTH_TOTAL Integer. Total length of the data stream. Default is 1200.
#' @param NUMBER_OF_BATCH Integer. Number of batches the data is divided into. Default is 30.
#' @param COEF_VAR_1 Vector or NULL. Coefficients to be applied for generating binary data.
#' @param VAR1_MEAN_COEF Numeric. Coefficient for the variable mean. Default is 2.
#'
#' @return A dataframe with data exhibiting recurring drifts.
#' 
#' @export
data_recuring_drift <- function(STREAM_LENGTH_TOTAL = 1200,
                                   NUMBER_OF_BATCH = 30,
                                COEF_VAR_1,
                                VAR1_MEAN_COEF = 2) {
  BATCH_SIZE = STREAM_LENGTH_TOTAL / NUMBER_OF_BATCH
  data_merged <- data.frame()
  for (i in 1:NUMBER_OF_BATCH) {
    if (i %in% 1:10) {
      if (is.null(COEF_VAR_1)){
        data_created <- data_gen_bin(n = BATCH_SIZE,
                                       x1_mean = 100)
      }else{
        data_created <- data_gen_bin(BATCH_SIZE,
                                     b1 = COEF_VAR_1[1])
      }

      data_merged <- rbind(data_merged, data_created)
      
    } else if (i %in% 11:20) {
      if (is.null(COEF_VAR_1)) {
        data_created <- data_gen_bin(n = BATCH_SIZE,
                                       x1_mean = 100,
                                       x1_mean_coef = VAR1_MEAN_COEF)
      }else{
        data_created <- data_gen_bin(BATCH_SIZE,
                                     b1 = COEF_VAR_1[2])
      }

      data_merged <- rbind(data_merged, data_created)

    } else{
      if (is.null(COEF_VAR_1)){
        data_created <- data_gen_bin(n = BATCH_SIZE,
                                     x1_mean = 100)
      }else{
        data_created <- data_gen_bin(BATCH_SIZE,
                                     b1 = COEF_VAR_1[1])
      }
      
      data_merged <- rbind(data_merged, data_created)
    }
  }
  return(data_merged)
}
