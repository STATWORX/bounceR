#' Build formulas out of an output returned by \code{\link{featureSelection}}
#' This function allows you to build custom estimation functions from a \linkS4class{sel_obj}.
#'
#' @param object must be an object of class \linkS4class{sel_obj}
#' @param n_features a \code{\link{numeric}} value indicating the number of features to add into the equation
#'
#' @return A model formula suggested by the feature selection algorithm.
#' @examples
#' \dontrun{
#' # simulate data
#' test_df <- sim_data()
#'
#' # feature selection
#' sel <- featureSelection(df = test_df,
#'                         target = "y",
#'                         index = NULL,
#'                         method = "randomboost",
#'                         n_cores = 1)
#' # extract one feature
#' form <- builder(object = sel, n_features = 1)
#' }
#' @export
builder <- function(object, n_features = 5){

            # check
            if(!class(object) == "sel_obj") stop(paste0("This function only works with objects of type ",
                                                        sQuote("sel_obj"), "!"))
            if(n_features > length(pull(object[["stability"]][, "feature"]))){
              n_features <- length(pull(object[["stability"]][, "feature"]))
              warning(paste0("Seems like you chose too many features. Try to reduce ",
                             sQuote("n_features"), ". Using all features: ",
                             n_features))
            }

            # pull out the features
            form <- as.formula(paste(object[["setup"]][["target"]], "~", paste(pull(object[["stability"]][, "feature"][1:n_features, ]), collapse = "+"), sep = ""))

            # return
            return(form)
          }
