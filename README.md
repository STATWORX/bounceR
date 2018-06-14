# bounceR <img src="misc/figures/hex.png" width=170 align="right" />

***

The R package bounceR provides methods and tools for automated feature selection for Machine Learning models. The methods are fit for situation in which the data scientists faces an exceedingly high number of features. Even if the number of features far exceeds the number of observations, the methods are equipped to reduce dimensionality of the feature space.

The package leverages two main tools for feature selection. First, a bunch of simple filtering methods are implemented. Filtering method in general provide simple heuristics to pre-reduce the feature space, by applying mostly bivariate comparisons. Second, the package contains wrapper methods for feature selection. Wrapper methods in general are iterative search algorithms. The wrapper methods implemented here leverage componentwise boosting as a weak learners.


<img src="misc/figures/bounceR.gif" align="middle"/>


***

## Installation

You can install the development version from Github.

    # install.packages("devtools")
      devtools::install_github("STATWORX/bounceR")



## Usage

    library(bounceR)

    # DATA GENERATION ---------------------------------------------------------
    # We start by simulating a dataset where we can divide the feature space into relevant and irrelevant features
    # simulating a dataset
    train_df <- sim_data(n = 1000,
                        modelvars = 30,
                        noisevars = 2000,
                        model_sd = 4,
                        noise_sd = 4,
                        epsilon_sd = 4,
                        outcome = "regression",
                        cutoff = NULL)

    # FILTER METHODS ----------------------------------------------------------
    # To reduce the dimensionality of the feature space, we can filter out irrelevant features using simple correlation,
    # information criteria and near zero variance metrics.
    # Correlation Collinearity Filter
    test_cc <- featureFiltering(data = train_df,
                                target = "y",
                                method = "cc",
                                returning = "names")

    # Maximum Relevance Minimum Redundancy Filter
    test_mr <- featureFiltering(data = train_df,
                                target = "y",
                                method = "mrmr",
                                returning = "names")

    # WRAPPER METHODS ---------------------------------------------------------
    # For a rather rigorous and more importantly model oriented selection, we can use wrapper methods to produce optimal
    # model equations, based on stability criteria.
    
    test_ge <- featureSelection(data = train_df,
                                target = "y",
                                selection = selectionControl(n_rounds = 100,
                                                             n_mods = 1000,
                                                             p = 30,
                                                             penalty = 0.3,
                                                             reward = 0.2),
                                bootstrap = "regular",
                                early_stopping = "none",
                                parallel = TRUE)
                                
    # If you are not sure how long it 100 n_rounds might take and you'd rather set a specific time,
    # you can use the max_time argument
    
    test_ge <- featureSelection(data = train_df,
                                target = "y",
                                max_time = "10 mins",
                                bootstraps = "regular",
                                early_stopping = "none",
                                parallel = TRUE)


## For more information

- Take a look at our blog to check out the functions and a bunch of other stuff https://www.statworx.com/de/blog/

## Sources

- We use the componentwise boosting implementation from the mboost package https://github.com/boost-R/mboost
- The hex sticker is partially made from <a href="https://www.vecteezy.com">Free Vector Design by: vecteezy.com</a>

## License

This package is free and open source software, licensed under GPL-3.
