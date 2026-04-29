## Using Expectation Maximization for Right-Censored Cricket Data

This repository contains the R Shiny app and supporting code for modeling cricket statistics using an Expectation-Maximization (EM) algorithm and Maximum Likelihood Estimation for gamma models on right-censored data.

## Why
Standard Cricket batting average calculations are biased because they exclude not-out innings from the denominator. This leads to inflation for players who frequently end games as not-out, in turn, distorting true performance.

This project addresses that issue by modeling batting performance as a right-censored statistical problem, where not-out innings represent incomplete observations rather than missing data.

The goal is to produce a more statistically consistent estimation of player performance that accounts for incomplete dismissal information.

## Method
Batting scores are modeled using a Gamma distribution under censoring.
- E-step:
  - Computes conditional expectations of uncensored performance for not-out innings using the current parameter estimates.
 
- M-step:
  - Maximizes the expected complete-data log-likelihood to update the Gamma parameters (shape and scale).

Iterate until convergence

This process results in adjusted estimations of: expected runs per inning, expected balls faced per inning, and derived strike rate under the corrected performance distribution.

## Files

- `Cricket App.R` – Main R Shiny app file. Launch this file in RStudio to run the interactive app.  
- `EM_Algorithm.R` – Contains the functions implementing the EM algorithm and related calculations.  
- `Dataset.xlsx` – Example cricket dataset for testing and demonstration.  


## How to Run

1. Install R and RStudio if not already installed:  
2. Install required R packages (if not already installed):
