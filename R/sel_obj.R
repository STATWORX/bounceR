# S4 CLASS ---------------
#' A S4 class to represent the a feature selection output obtained by the \code{\link{featureSelection}} function.
#' @slot stability a \code{\link{tibble}} with the stability matrix obtained by the feature selection algorithm
#' @slot opt_formula a \code{\link{formula}} object as proposed by a structural break test.
#' @slot setup a \code{\link{list}} containing call and runtime information.
#' @exportClass sel_obj
setClass("sel_obj",
         representation(stability = "tbl_df",
                        opt_formula = "formula",
                        setup = "list"))

# Plot method ---------------
#' @describeIn sel_obj A variable importance plot
#' @param x an object of class \linkS4class{sel_obj}
#' @param n_features a numeric value determining the number of variables in the plot
#' @exportMethod plot
setMethod("plot", signature("sel_obj"),
          function(x, n_features = NULL){
            if (is(n_features, "NULL")) n_features <- x@setup$n_features

            # plot variable importance
            x@stability[1:n_features, ] %>%
              ggplot2::ggplot(.) +
              ggplot2::geom_bar(ggplot2::aes(y = freq, x = stats::reorder(feature, freq)),
                                stat = "identity",
                                alpha = 0.6,
                                width = 0.4) +
              ggplot2::ggtitle("Feature Importance") +
              ggplot2::xlab("Feature Names") +
              ggplot2::ylab("Relative Selection Frequency") +
              ggplot2::theme_minimal() +
              ggplot2::coord_flip()
          })

# Summary method ---------------
#' @describeIn sel_obj A summary of an output obtained by \code{\link{featureSelection}}
#' @param object an object of class \linkS4class{sel_obj}
#' @exportMethod summary
setMethod("summary", signature("sel_obj"),
          function(object){

            cat("Feature Selection using Randomized Boosting")
            cat("\n")
            cat("\n")
            summary_out <- data.frame(n_features = object@setup[["n_features"]],
                                      length = object@setup[["length"]],
                                      early_stopping = object@setup[["early_stopping"]],
                                      bootstrap = object@setup[["bootstrap"]],
                                      n_cores = object@setup[["n_cores"]],
                                      time = object@setup[["time"]])

            print(summary_out)
          })

# Print method ---------------
#' @describeIn sel_obj A printed setup of a \linkS4class{sel_obj}
#' @exportMethod print
setMethod("print", signature("sel_obj"),
          function(x){
            cat("Feature Selection using Randomized Boosting")
            cat("\n")
            cat("\n")
            cat("\n")
            cat("Boosting Setup:")
            cat("\n")
            cat("Number of Iterations:", x@setup[["n_rounds"]])
            cat("\n")
            cat("Features selected in total:", x@setup[["n_features"]])
            cat("\n")
            cat("\n")
            cat("Stability Matrix:")
            cat("\n")
            print(head(x@stability, n = 10))
          })


# Show method ---------------
#' @describeIn sel_obj A printed setup of a \linkS4class{sel_obj}
#' @exportMethod show
setMethod("show", signature("sel_obj"),
          function(object){
            cat("Feature Selection using Randomized Boosting")
            cat("\n")
            cat("\n")
            cat("\n")
            cat("Boosting Setup:")
            cat("\n")
            cat("Number of Iterations:", object@setup[["n_rounds"]])
            cat("\n")
            cat("Features selected in total:", object@setup[["n_features"]])
            cat("\n")
            cat("\n")
            cat("Stability Matrix:")
            cat("\n")
            print(head(object@stability, n = 10))
          })
