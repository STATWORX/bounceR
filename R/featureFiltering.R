#' Feature space reduction via filtering methods
#'
#' @description This function .....
#'
#' @param data a dataset containing the target, all features, and potentially a time index
#' @param target a string with the column name of the target
#' @param index a string with the column name of the index
#' @param method a string indicating the method used for filtering: "cc", "mrmr"
#' @param returning a string indicating the return: "names", "data"
#'
#' @details some details...
#'
#' @return Either a vector of names of a data.frame object depending on the returning argument
#' @import dplyr mRMRe
#' @examples
#' \dontrun{
#' # Simulate Data
#' test_df <- sim_data()
#'
#' # Correlation Collinearity Filter
#' test_cc <- featureFiltering(data = test_df,
#'                             target = "y",
#'                             method = "cc",
#'                             returning = "names")
#'
#' # Maximum Relevance Minimum Redundancy Filter
#' test_mr <- featureFiltering(data = test_df,
#'                             target = "y",
#'                             method = "mrmr",
#'                             returning = "names")
#' }
#' @export
featureFiltering <- function(data,
                             target,
                             index = NULL,
                             method = "cc",
                             returning = "names") {


# SETUP -------------------------------------------------------------------
# external libraries
#require(mRMRe)
#require(caret)
#require(dplyr)

# input checks
if(missing(data)) stop("I couldn't find your data")
if(missing(target)) stop("I couldn't the target")
if(mode(target) != "character") stop("You only need to give me the name of the column")
if(!is.null(index) & mode(index) != "character") stop("I need the name of the index column")
if(!method %in% c("cc", "mrmr", "nzv")) stop("You can only choose \"cc\" or \"mrmr\"")
if(!returning %in% c("names", "data")) stop("You can only choose names or data")

# setting input
data <- as.data.frame(data)

# get out the index
df_help <- data[, !names(data) %in% c(index)]

# CORRELATION -------------------------------------------------------------
# if method is correlation
if(method == "cc"){

  # open empty container
  correlation_target <- c()
  names_features <- c()

  # loop over data
  for(i in seq(df_help)){

    # only include numeric features
    if(!is.numeric(df_help[, i])) next

    # calculate correlations and get features names
    correlation_target[i] <- cor(data.frame(target = df_help[, target], var = df_help[, i]), use = "pairwise.complete.obs")[2,1]
    names_features[i] <- names(df_help)[i]

  }

  # get the mean correlation
  threshold_corr <- mean(abs(correlation_target))

  # build a correlation data frame
  correlation_df <- data.frame(correlation = abs(correlation_target),
                               features = names_features)

  # filter out irrelevant ones
  correlated_features <- correlation_df %>%
    dplyr::filter(correlation > threshold_corr) %>%
    dplyr::select(features) %>%
    dplyr::filter(features != target) %>%
    dplyr::pull(.) %>%
    as.character(.)

  # subset the dataset
  subset_df <- df_help %>%
    dplyr::select(., dplyr::one_of(c(target, correlated_features)))

  # set them all to numerics
  subset_df <- sapply(subset_df, as.numeric)

  # build a correlation matrix
  correlation_matrix <- as.matrix(cor(subset_df, use = "pairwise.complete.obs"))

  # get the threshold
  threshold_coll <- mean(correlation_matrix[lower.tri(correlation_matrix)])

  # collinear features
  collinear <- subset(reshape2::melt(correlation_matrix), value > threshold_coll) %>%
    dplyr::filter(Var1 != Var2)

  # run collinearity checks
  best_features <- c()

  # loop over collinear features
  for(i in seq(nrow(collinear))){

    # correlation with target
    corr_a <- cor(data.frame(row1 = subset_df[, target],
                             row2 = subset_df[, paste(collinear$Var1[i])]),
                  use = "pairwise.complete.obs")[2, 1]
    corr_b <- cor(data.frame(row1 = subset_df[, target],
                             row2 = subset_df[, paste(collinear$Var2[i])]),
                  use = "pairwise.complete.obs")[2, 1]

    # check for higher correlation with target
    if(is.na(corr_a) | is.na(corr_b)){

      next

    } else if(corr_a >= corr_b){

      best_features[i] <- paste(collinear$Var1[i])

    } else if(corr_a < corr_b){

      best_features[i] <- paste(collinear$Var2[i])

    }

  }

  # extract the relevant features
  best_features <- na.omit(unique(best_features))

  # filter out the target
  best_features <- best_features[!best_features %in% c(target)]

  # return
  if(returning == "names"){

    return(best_features)

  } else if(returning == "data"){

    return(data[, names(data) %in% c(index, target, best_features)])

  }


# MAXIMUM RELEVANCE -------------------------------------------------------
} else if(method == "mrmr"){

  # set a n_features threshold
  n_features <- floor(sqrt(nrow(df_help))) + 1

  # set an n_solutions threshold
  n_solutions <- 1000

  # convert data to mrmr_data
  mrmr_data <- as.data.frame(sapply(df_help, as.numeric))
  mrmr_data <- mRMRe::mRMR.data(data = mrmr_data)

  # run the ensemble
  mrmr_features <- mRMRe::mRMR.ensemble(mrmr_data,
                                       solution_count = n_solutions,
                                       feature_count = n_features,
                                       target_indices = which(names(df_help) == target))

  # get the solutions matrix
  sol_mat <- mRMRe::solutions(mrmr_features)[[1]]

  # check out zero variance
  sol_mat <- sol_mat[, apply(sol_mat, MARGIN = 2, FUN = function(x) {
                                                  y <- var(x, na.rm = TRUE)
                                                  if(!is(y, "numeric")) y <- 0
                                                  return(y!=0)
                                                                  })]
  
  # create empty vector
  majority_vote <- c()

  # start majority vote
  for (i in seq(n_features)) {

    # get the candidates
    candidates <- names(sort(table(sol_mat[i, ]), decreasing = TRUE))

    # build majority vote
    for (ii in seq_along(candidates)) {

      # get out target
      if(candidates[ii] == "1") next
      if(!candidates[ii] %in% majority_vote) {
        majority_vote[i] <- candidates[ii]
        break
      }
    }
  }

  # manage the return
  best_features <- names(df_help[, as.numeric(majority_vote)])

  # cut out the target
  best_features <- best_features[!best_features %in% c(target)]

  # return
  if(returning == "names"){

    return(best_features)

  } else if(returning == "data"){

    return(data[, names(data) %in% c(index, target, best_features)])

  }
 }

  # manage return
  if(returning == "names"){

    best_features <- names(df_help)[!names(df_help) %in% c(target)]
    return(best_features)

  } else if(returning == "data"){

    return(data[, names(data) %in% c(index, target, best_features)])

  }
}
