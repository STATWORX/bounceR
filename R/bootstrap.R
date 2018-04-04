##     ____              __       __
##    / __ )____  ____  / /______/ /__________ _____
##   / __  / __ \/ __ \/ __/ ___/ __/ ___/ __ `/ __ \
##  / /_/ / /_/ / /_/ / /_(__  ) /_/ /  / /_/ / /_/ /
## /_____/\____/\____/\__/____/\__/_/   \__,_/ .___/
##                                          /_/

# Moving Block Bootstrap
#' A function which computes a moving block bootstrap of a target time series and its respective features.
#'
#' @param data a dataset containing the target time series and its features
#' @param target a character with the target variable
#' @param seed a numeric handling the random generator
#'
#' @return  BLA BLA
#' @export
#'
#' @import np
#'
#' @examples
#' # Simulate test data
#' test_data <- sim_data()
#' # Bootstrap
#' bootstrapped <- mbb(data = test_data,
#'                     target = "y",
#'                     seed = 1)
mbb <- function(data,
                target,
                seed = 1){

  # calculate optimal block length
  opt_block_length <- round(b.star(data[, c(target)])[2])

  # loop over all observations
  for(i in seq(nrow(data))){

    # set start and end for blocks
    start <- i
    end <- i + opt_block_length

 	  # build a new data set with all blocks
    if (i == 1){
 	    data_blocks <- data[start:end, ]
 	    ID <- rep(i, length(start:end))
 	  } else {
 		  data_blocks <- rbind(data_blocks, data[start:end, ])
 	    ID <- append(ID, rep(i, length(start:end)))
    }
 }

  # merge the results
  data_blocks <- cbind(data_blocks, ID)

  # random draw
  set.seed(seed)

  # random sample unique block IDs
  ID_reshuffled <- data.frame(ID=c(sample(unique(ID), size=length(unique(ID)), replace=TRUE)))

  # shuffle data_blocks according to ID_reshuffled
  data_bs <- ID_reshuffled %>%
    dplyr::left_join(., data_blocks, by="ID") %>%
    dplyr::mutate(row_num = row_number()) %>%
    dplyr::filter(row_num <= nrow(data)) %>%
    dplyr::select(., -ID)

 	# function returns a data.table object
  result <- as.data.frame(data_bs)
 	return(result)

}
