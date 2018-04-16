#' Automated Feature Selection for Machine Learning Models with Boosting Ensembles
#'
#' @param data a dataset containing the target, all features, and potentially a time index
#' @param target a string with the column name of the target
#' @param index a string with the column name of the index
#' @param selection takes the return from \code{\link{selectionControl}}
#' @param bootstrap a string specifying the bootstrap method
#' @param boosting takes the return form \code{\link{boostingControl}}
#' @param early_stopping a string giving the method for early stopping
#' @param n_cores an integer giving the amount of cores
#' @param verbose a logical indicating whether logging info is displayed
#'
#' @return a list with the following entries
#' \itemize{
#' \item \code{stability} - a stability matrix with two columns: feature names and selection stability score
#' \item \code{opt_formula} - statistically optimal formula derived from breakpoints in the stability score series
#' \item \code{setup} - a list with misc. configurations
#' }
#' @import dplyr doMC doParallel foreach
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
#'                             selection = selectionControl(n_rounds = 10,
#'                                                          n_mods = 100,
#'                                                          p = 30,
#'                                                          penalty = 0.3,
#'                                                          reward = 0.2),
#'                             bootstrap = "regular",
#'                             early_stopping = "none",
#'                             n_cores = 1)
#' }
#' @export
featureSelection <- function(data,
                             target,
                             index = NULL,
                             selection = selectionControl(),
                             bootstrap = "none",
                             boosting = boostingControl(),
                             early_stopping = "none",
                             n_cores = 1,
                             verbose = TRUE){

# SETUP -------------------------------------------------------------------

# input checks
if(missing(data)) stop("I cannot find your data")
if(missing(target)) stop("I cannot find your target")
if(mode(target) != "character") stop("Just give me the name of the target column")
if(!target %in% names(data)) stop("The data does not contain the target!")
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

# BACKEND -----------------------------------------------------------------
# check if parallel
if(n_cores > 1){

  # check the system
  this_sys <- as.list(Sys.info())

  # check if windows
  if(this_sys$sysname == "Windows"){

    # open socket cluster
    cl <- parallel::makeCluster(n_cores, type = "PSOCK")

    # register parallel Backend
    doParallel::registerDoParallel(cl)

  # UNIX based OS
  } else {

    # register with doMC
    doMC::registerDoMC(cores = n_cores)
  }

}

# GENETICBOOST ------------------------------------------------------------
# message
if(verbose){
  cat("Feature Selection using a Genetic Algorithm")
  cat("\n")
}

# set default: n_rounds
if(is.null(selection[["n_rounds"]])) selection[["n_rounds"]] <- floor((1 + ncol(data)/nrow(data)) * 100)

# set default: n_mods
if(is.null(selection[["n_mods"]])) selection[["n_mods"]] <- floor((1 + ncol(data)/nrow(data)) * 1000)

# set default: p
if(is.null(selection[["p"]])) selection[["p"]] <- floor(sqrt(sqrt(ncol(data) * (1 + (nrow(data)/sqrt(ncol(data)))))))

# set default: penalty
if(is.null(selection[["penalty"]])) selection[["penalty"]] <- 0.2

# set default: reward
if(is.null(selection[["reward"]])) selection[["reward"]] <- 0.1

# save default p
original_p <- max(5, floor(sqrt(selection[["p"]])))

# message
if(verbose){
  cat("Starting Genetic Algorithm...")
  cat("\n")
}

# loop over n_rounds
important_features <- suppressWarnings(foreach::foreach(i = seq(selection[["n_rounds"]]),
                                .errorhandling = "remove") %dopar% {

# get feature names
feature_names <- names(data)[!names(data) %in% c(target)]

# error message if no features
if(length(feature_names) == 0){

  # print warning
  warning("It seems like your data does not have any features in Iteration", i)

} else if(length(feature_names) != 0){

  # create a mirror dataset
  df_mirrored <- data

  # set random seed
  set.seed(i * selection[["n_rounds"]])
  
  # freq vector
  freq_vec <- sample(seq(from = 0, to = 1, by = 0.001),
                 length(feature_names),
                 replace = T)

  # intialize random stability matrix
  stability_matrix <- data.frame(feature = feature_names,
                                 freq = freq_vec,
                                 freq_original = freq_vec)

  # iterate over models
  for(ii in seq(selection[["n_mods"]])){
    
    # set random seed
    set.seed(i + ii)

    # check if there is still something left
    if(class(df_mirrored) != "data.frame") break
    
    # check if stability matrix is getting too small
    if(nrow(stability_matrix) <= original_p) break

    # failsafe if df_mirrored gets to small
    if(selection[["p"]] >= ncol(df_mirrored)){
      p <- ncol(df_mirrored) - 1
    } else {
        p <- selection[["p"]]
    }

    # check for bootstrap mehod
    if(bootstrap == "none"){

      # no bootstrap
      df_mirrored_model <- df_mirrored[, names(df_mirrored) %in% c(target, sample(names(df_mirrored)[!names(df_mirrored) %in% c(target)], p, replace = F))]

    } else if(bootstrap == "regular"){

      # regular bootstrap
      df_mirrored_model <- df_mirrored[sample(1:nrow(df_mirrored), size = nrow(df_mirrored), replace = T),
                                       names(df_mirrored) %in% c(target, sample(names(df_mirrored)[!names(df_mirrored) %in% c(target)], p, replace = F))]

    } else if(bootstrap == "moving"){

      # moving block bootstrap
      df_mirrored_model <- mbb(data = df_mirrored[, names(df_mirrored) %in% c(target, sample(names(df_mirrored)[!names(df_mirrored) %in% c(target)], p, replace = F))],
                               target = tagret,
                               seed = (i + ii))

    }

    # input features
    input_feature_names <- names(df_mirrored_model)[!names(df_mirrored_model) %in% c(target)]

    # check for the base learner
    if(boosting[["base"]] == "lm"){

      # run the linear boosting model
      model <- tryCatch({mboost::glmboost(as.formula(paste(target, "~", ".", sep = "")),
                                          data = df_mirrored_model,
                                          center = FALSE,
                                          control = mboost::boost_control(mstop = boosting[["mstop"]], nu = boosting[["nu"]]))},
                        error = function(e){ return(NULL)})

    # non linear base learner
    } else if(boosting[["base"]] == "tree"){

      # run the non-linear boosting model
      model <- tryCatch({mboost::gamboost(as.formula(paste(target, "~", ".", sep = "")),
                                          data = df_mirrored_model,
                                          control = mboost::boost_control(mstop = boosting[["mstop"]], nu = boosting[["nu"]]),
                                          baselearner = c("btree"))},
                        error = function(e){ return(NULL)})
    }

    # warning if model is null
    if(is.null(model)){

      # print warning
      warning(paste("In iteration", i, "I could not fit a model in round", ii))

      # return
      next

    }

    # check for early stopping
    if(early_stopping == "cv"){

      # cross validation
      cv <- mboost::cvrisk(model,
                            folds = mboost::cv(model.weights(model),
                                        type = "kfold",
                                        B = 5),
                            grid = seq(from = 0, to = mboost::mstop(model), by = 10),
                            papply = lapply)

      # subset the model
      model <- model[mboost::mstop(cv)]

    } else if(early_stopping == "aic"){

      # AIC
      aic_corr <- AIC(model, method = "corrected", data = "actset")

      # subset the model
      model <- model[mboost::mstop(aic_corr)]

    }
    
    # output features
    output_feature_names <- gsub("[\\(\\)]", "", unlist(regmatches(names(summary(model)$selprob), gregexpr("\\(.*?\\)", names(summary(model)$selprob)))))

    # get the difference
    features_for_penalty <- setdiff(input_feature_names, output_feature_names)

    # parse through penalty features
    for(iii in seq(features_for_penalty)){

      # go through and get position
      row_to_penalize <- which(stability_matrix == features_for_penalty[iii])

      # penalty
      stability_matrix$freq[row_to_penalize] <- stability_matrix$freq[row_to_penalize] - selection[["penalty"]]

    }

    # parse through reward features
    for(iii in seq(output_feature_names)){

      # go through and get position
      row_to_reward <- which(stability_matrix == output_feature_names[iii])

      # reward
      stability_matrix$freq[row_to_reward] <- stability_matrix$freq[row_to_reward] + selection[["reward"]]

    }

    # check whether some features can be deleted
    features_to_delete <- which(stability_matrix$freq <= 0)

    # if some features need to be deleted
    if(length(features_to_delete) != 0) {

      # find features to delete in the martrix
      features_names_to_delete <- stability_matrix$feature[features_to_delete]

      # delete the features from he feature matrix
      stability_matrix <- stability_matrix[-features_to_delete, ]

      # delete from the data set
      df_mirrored <- df_mirrored[, !names(df_mirrored) %in% features_names_to_delete]
    }
  }
}

# scale the frequency between 0 and 1
#stability_matrix$freq <- (stability_matrix$freq - min(stability_matrix$freq)) /
#                          diff(range(stability_matrix$freq))

# return
return(stability_matrix)

}
)

# time diff for ensemble
time_diff <- diff(c(starting_time, Sys.time()))
units(time_diff) <- "mins"

# logging info
if(verbose){
  cat("...Finished Genetic Algorithm in", time_diff, units(time_diff))
  cat("\n")
}

# compute stability matrix
important_features <- important_features %>%
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
                 n_cores = n_cores,
                 time = Sys.time() - starting_time,
                 length = nrow(important_features)))

}
