#' Control Function for the Boosting Setup
#'
#' @param mstop an integer specifying the number of boosting iterations for each weak learner
#' @param nu  a numeric between 0 and 1 specifying the learning rate for each weak learner
#' @param base a character specifying the type of baslearner to use. One of "lm" or "tree", either
#'             uses linear baselearners or tree baselearner.
#'
#' @return a list of the specified input
#'
#' @examples
#' boosting <- boostingControl(mstop = 1000, nu = 0.05)
#'
#' @export
boostingControl <- function(mstop = 100,
                            nu = 0.1,
                            base = "lm"){

    # error handling ----

    # mstop
    if(!is.numeric(mstop)) stop(paste(sQuote("mstop"), "must be an integer"))
    mstop <- round(mstop)

    # nu
    if(!is.numeric(nu)) stop(paste(sQuote("nu"), "must be a numeric value"))

    # base
    if(!base %in% c("lm", "tree")) stop(paste(sQuote("base"), "must be either", sQuote("lm"), "or", sQuote("tree")))

    # return ----
    return(list(mstop = mstop, nu = nu, base = base))

}
