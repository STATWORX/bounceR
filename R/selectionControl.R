#' Control Function for the Selection Setup
#'
#' @param n_rounds an integer specifying the number of outer loop iterations
#' @param n_mods an integer specifying the number of models inside each genetic mutation state
#' @param p an integer specifying the number of features for each weak learner
#' @param penalty an integer between 0 and 1 giving the penalty in the genetic setting
#' @param reward an integer between 0 and 1 giving the reward in the genetic setting
#'
#' @return a list of the specified input
#'
#' @examples
#' selection = selectionControl(n_rounds = 1000,
#'                              n_mods = 100)
#'
#' @export
selectionControl <- function(n_rounds = NULL,
                             n_mods = NULL,
                             p = NULL,
                             penalty = NULL,
                             reward = NULL){
  

  return(list(n_rounds = n_rounds, n_mods = n_mods, p = p, penalty = penalty, reward = reward))
}
