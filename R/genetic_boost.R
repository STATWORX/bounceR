#' Genetic Boosting Core Algorithm for Feature Selection
#'
#' @param iteration an integer specifying the iteration
#' @param data a dataset containing the target, all features, and potentially a time index
#' @param target a string with the column name of the target
#' @param selection takes the return from \code{\link{selectionControl}}
#' @param bootstrap a string specifying the bootstrap method
#' @param boosting takes the return form \code{\link{boostingControl}}
#' @param early_stopping a string giving the method for early stopping
#'
#' @return an updated feature Matrix
#'
#' @export
geneticBoost <- function(iteration,
                         data,
                         target,
                         selection,
                         bootstrap,
                         early_stopping,
                         boosting){

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
    
    # draw seed
    set.seed(sample(iteration:(1000000 + iteration), 1))

    # extract feature names
    feature_names <- names(data)[!names(data) %in% c(target)]

    # error message if no features
    if(length(feature_names) == 0){

        # print
        warning("It seems lie your data does not not have any features in iteration:", iteration)
    
    }

    # generate frequency vector
    freq_vec <- sample(seq(from = 0, to = 1, by = 0.001),
                       length(feature_names),
                       replace = T)
    
    # intialize random stability matrix
    stability_matrix <- data.frame(feature = feature_names,
                                   freq = freq_vec,
                                   freq_original = freq_vec)
    
    # create copy of the data
    df_mirrored <- data

    # iterate over models
    for(i in seq(selection[["n_mods"]])){

        # set random seed
        set.seed(i)

        # check if df_mirrored is still a data.frame
        if(class(df_mirrored) != "data.frame") break

        # check if stability matrix is getting too small
        if(nrow(stability_matrix) <= original_p) break

        # failsafe if df_mirrored gets too small
        if(selection[["p"]] >= ncol(df_mirrored)){

            # reset p
            p <- ncol(df_mirrored) - 1

        } else{

            # reset p
            p <- selection[["p"]]

        }

        # check for bootstrap method
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
                                     seed = (i^2))

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
        for(ii in seq(features_for_penalty)){

            # go through and get position
            row_to_penalize <- which(stability_matrix == features_for_penalty[ii])

            # penalty
            stability_matrix$freq[row_to_penalize] <- stability_matrix$freq[row_to_penalize] - selection[["penalty"]]

        }

        # parse through reward features
        for(ii in seq(output_feature_names)){

            # go through and get position
            row_to_reward <- which(stability_matrix == output_feature_names[ii])

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
    
    # return stability matrix
    return(stability_matrix)

}