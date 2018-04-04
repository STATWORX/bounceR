#' Simulating Classification and Regression Data
#'
#' @param n an integer giving the number of observations the simulated dataset shall have
#' @param modelvars an integer giving the number of relevant predictors, out of the entire feature space
#' @param noisevars an integer giving the number of irrelevant predictors
#' @param model_sd an integer giving the standard deviation of the model
#' @param noise_sd an integer giving the standard deviation of the noise features
#' @param epsilon_sd an integer giving the standard deviation of the error term
#' @param outcome a string indicating whether a "regression" or a "classification" dataset are supposed to be simulated
#' @param cutoff a numeric value specifying the cutoff value for classification simulation
#'
#'
#' @return a data.frame object
#'
#' @examples
#' # simulating regression data
#' sim_reg_data <- sim_data(n = 1000, outcome = "regression")
#'
#' @export
sim_data <- function(n = 1000,
                     modelvars = 10,
                     noisevars = 100,
                     model_sd = 4,
                     noise_sd = 4,
                     epsilon_sd = 4,
                     outcome = "regression",
                     cutoff = NULL){

  # cutoff not needed if regression
  if(outcome == "regression" & !is.null(cutoff)) message("You don't need a cutoff for regression outcomes!")
  # model matrix
  model_cols <- 1:modelvars %>% formatC(., width = max(nchar(.)), flag = "0") %>% paste0("var", .)
  model_mat <- matrix(rnorm(n * modelvars, mean = 0, sd = model_sd), ncol = modelvars,
                      dimnames = list(rows = 1:n, cols = model_cols))

  # noise matrix
  noise_cols <- 1:noisevars %>% formatC(., width = max(nchar(.)), flag = "0") %>% paste0("noise", .)
  noise_mat <- matrix(rnorm(n * noisevars, mean = 0, sd = noise_sd), ncol = noisevars,
                      dimnames = list(rows = 1:n, cols = noise_cols))

  # epsilon
  epsilon = rnorm(n, mean = 0, sd = epsilon_sd)

  # betas
    betas <- c(-1, 1)[1:modelvars %% 2 + 1]

  # compute target
  if(outcome == "regression"){

    y <- 1 + model_mat %*% betas + epsilon

  } else if(outcome == "classification"){

    y_star <- 1/(1 + exp(-(model_mat %*% betas + epsilon)))
    y <- ifelse(y_star > cutoff, 1, 0)

  }

  # return
  return(data.frame(y = y, model_mat, noise_mat))

}
