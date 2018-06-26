#' Automated Feature Selection for Machine Learning Models with Boosting Ensembles
#'
#' @param data a dataset containing the target, all features, and potentially a time index
#' @param target a string with the column name of the target
#' @param index a string with the column name of the index
#' @param max_time a string taking the maximum time the user is willing to wait in minutes
#' @param selection takes the return from \code{\link{selectionControl}}
#' @param bootstrap a string specifying the bootstrap method
#' @param boosting takes the return form \code{\link{boostingControl}}
#' @param early_stopping a string giving the method for early stopping
#' @param parallel a logical taking TRUE or FALSE
#' @param verbose a logical indicating whether logging info is displayed
#'
#' @return a list with the following entries
#' \itemize{
#' \item \code{stability} - a stability matrix with two columns: feature names and selection stability score
#' \item \code{opt_formula} - statistically optimal formula derived from breakpoints in the stability score series
#' \item \code{setup} - a list with misc. configurations
#' }
#' @import dplyr purrr future furrr mboost
#' @details This function implements a feature selection algorithm leveraging the ideas from backpropagation and randomness. Each n_rounds round a new random 
#' stability matrix with two columns is initialized. The first column has the name of the feature and the second a stability score indicating the empirical
#' relevance score of a feature. Each of these n_rounds random matrices is then updated over the course of n_mods componentwise boosting models on 
#' random subsets of the feature space. The updating process works as follows: if a feature was contained in a subset, but was not selected in the boosting,
#' it's score in the randomly intialized matrix is reduced by the amount of the penalty. If a feature was contained in a subset and was selected in a boosting, 
#' it's score in the randomly intialized matrix is increased by the amount of the reward. After n_mods models in each n_rounds rounds the n_rounds updated 
#' stability matrices are combined by simply averaging the scores for each feature across all matrices.
#' @examples
#' \dontrun{
#' # Simulate Data
#' test_df <- sim_data()
#'
#' # Genetic Boosting Ensemble
#' test_ge <- featureSelection(data = test_df,
#'                             target = "y",
#'                             max_time = "10 min"
#'                             selection = selectionControl(n_rounds = NULL,
#'                                                          n_mods = NULL,
#'                                                          p = NULL,
#'                                                          penalty = NULL,
#'                                                          reward = NULL),
#'                             bootstrap = "regular",
#'                             early_stopping = "none",
#'                             n_cores = 1)
#'                             
#' test_ge <- featureSelection(data = test_df,
#'                             target = "y",
#'                             max_time = NULL,
#'                             selection = selectionControl(n_rounds = NULL,
#'                                                          n_mods = NULL,
#'                                                          p = NULL,
#'                                                          penalty = NULL,
#'                                                          reward = NULL),
#'                             bootstrap = "regular",
#'                             early_stopping = "none",
#'                             n_cores = 1)
#' }
#' @export
featureSelection <- function(data,
                             target,
                             index = NULL,
                             max_time = "10 min",
                             selection = selectionControl(),
                             bootstrap = "none",
                             boosting = boostingControl(),
                             early_stopping = "none",
                             parallel = TRUE,
                             verbose = TRUE){

# SETUP -------------------------------------------------------------------
# input checks
if(missing(data)) stop("I cannot find your data")
if(missing(target)) stop("I cannot find your target")
if(!target %in% names(data)) stop("The data does not contain the target!")
if(mode(target) != "character") stop("Just give me the name of the target column")
if(!is.null(index) & mode(index) != "character") stop("Just give me the name of the index column")
if(!bootstrap %in% c("none", "regular", "moving")) stop("You can only choose either none, regular, or moving")
if(!early_stopping %in% c("none", "aic", "cv")) stop("You can only choose either none, aic, or cv")
if(bootstrap == "moving" & is.null(index)) warning("Moving block bootstrap only works with time series")

# setting input data to data.frame
data <- as.data.frame(data)

# check dimension
cols_before <- dim(data)[2]

# check for na cols
data <- data[, !apply(is.na(data), 2, all)]

# check dimension after
cols_after <- dim(data)[2]

# print warning if necessary
if(cols_after != cols_before) cat("All rows contain only NAs were deleted!")

# cutting out the index
if(!is.null(index)) data <- data[, !names(data) %in% c(index)]

# logging starting time
starting_time <- Sys.time()

# print warning that it might take a while
if(boosting[["base"]] == "tree") cat("Caution tree baselearner can lead to an excessive computation time")

# change default: aic with tree
if(boosting[["base"]] == "tree" & early_stopping == "aic") {

  # print warning
  warning("You cannot use AIC with trees as base learners, so I am using CV")

  # change default
  early_stopping <- "cv"

}


# TIMING ------------------------------------------------------------------
# check if necessary
if(!is.null(max_time)){
  

  # decode max_time
  max_time <- as.numeric(gsub("([0-9]+).*$", "\\1", max_time))*60

  # run genetic boost and extract time
  burn_in_time <- invisible(system.time(geneticBoost(iteration = 1,
                                                     data = data,
                                                     target = target,
                                                     selection = selection,
                                                     bootstrap = bootstrap,
                                                     early_stopping = early_stopping,
                                                     boosting = boosting))["user.self"])[["user.self"]]

}

# BACKEND -----------------------------------------------------------------
# check if parallel
if(parallel){
  
  # detect the number of cores
  cores <- parallel::detectCores()

  # set parallization plan
  future::plan(multiprocess)

}

# if not parallel
cores <- 1

# GENETICBOOST ------------------------------------------------------------
# calculate the optimal number of n_rounds
if(!is.null(max_time)) selection[["n_rounds"]]  <- (floor((max_time - burn_in_time)/burn_in_time))*(cores*4)

# message
if(verbose){
  cat("Feature Selection using a Genetic Algorithm")
  cat("\n")
}

# message
if(verbose){
  cat("Starting Genetic Algorithm...")
  cat("\n")
}

# loop over rounds
important_features <- 1:selection[["n_rounds"]] %>%
  furrr::future_map(.x = .,
                    .f = ~ geneticBoost(iteration = .x,
                                        data = data,
                                        target = target,
                                        selection = selection,
                                        bootstrap = bootstrap,
                                        early_stopping = early_stopping,
                                        boosting = boosting)) %>%
  dplyr::bind_rows(.) %>%
  dplyr::group_by(feature) %>%
  dplyr::summarise(freq = mean(freq, na.rm = TRUE)) %>%
  as.data.frame() %>%
  dplyr::arrange(dplyr::desc(freq)) %>%
  dplyr::as_tibble() %>%
  dplyr::filter(!feature %in% c("Intercept"))

  # rescale
  important_features$freq <- (important_features$freq - min(important_features$freq)) /
                              diff(range(important_features$freq))


# time diff for ensemble
time_diff <- diff(c(starting_time, Sys.time()))
units(time_diff) <- "mins"

# logging info
if(verbose){
  cat("...Finished Genetic Algorithm in", time_diff, units(time_diff))
  cat("\n")
}

# OUTPUT ------------------------------------------------------------------
# optimal formula: data.frame
break_data <- tryCatch({data.frame(freq = important_features$freq,
                         obs = 1:nrow(important_features))},
                        error = function(e){stop("The return from the ensemble was empty, you should maybe check your data for too many missings,
                                                  or other weird things")})

# optimal formula: break analysis
n_features <- tryCatch({strucchange::breakpoints(freq ~ obs,
                                    data = break_data[1:p, nrow(break_data), ],
                                    h = max(0.05*nrow(data), 5),
                                    breaks = 1)$breakpoints},
                       error = function(e){return(floor(sqrt(nrow(data))))})

# optimal_formula: failsafe NA
if(is.na(n_features)) n_features <- floor(sqrt(nrow(data)))

# optimal formula: failsafe
if(nrow(important_features) <= n_features) n_features <- nrow(important_features)

# optimal formula: formula
form <- as.formula(paste(target, "~", paste(important_features$feature[1:n_features], collapse = "+"), sep = ""))

# RETURN ------------------------------------------------------------------
# storing output
new("sel_obj",
    stability = important_features,
    opt_formula = form,
    setup = list(target = target,
                 n_features = n_features,
                 n_rounds = selection$n_rounds,
                 bootstrap = bootstrap,
                 early_stopping = early_stopping,
                 n_cores = cores,
                 time = Sys.time() - starting_time,
                 length = nrow(important_features)))

}
