# Script for loading packages and functions used in the rest of the analysis
# Load packages -----------------------------------------------------------
library(tidyverse)
library(here)
library(changepoint)
library(bcp)
library(data.table)
library(nlmrt)
library(FSA)
library(gridExtra)
library(stats4)
library(DCCA)
library(MARSS)
library(sf)
# Functions ---------------------------------------------------------------

retrieve_sr_data <- function(x){
  row <- which(use_stocks$stock_name == x)
  
  # create a tibble for each stock's biomass and recruit data
  stock = tibble(
    year = pull(takers_rec[,1]),
    recruits = pull(takers_rec[,x]),
    sb = pull(takers_ssb[,x]),
    logR = pull(log(takers_rec[,x])))
  
  # remove model run in time
  min_year <- pull(use_stocks[row, "min_year"])
  max_year <- pull(use_stocks[row, "max_year"])
  
  stock <- stock %>%
    filter(year >= min_year) %>%
    filter(year <= max_year)
  
  # Change any 0 recruit values to 1
  stock <- stock %>%
    mutate(logR = replace(logR, logR == -Inf, 1)) %>%
    mutate(recruits = replace(recruits, recruits == 0, 1))
  
  return(stock)
}

bevholt <- function(stock){
  dats <- stock$sb
  datr <- stock$recruits
  
  BHminusLL <- function(loga, logb, logsigmaR){
    # extract parameters
    a <- exp(loga); b <- exp(logb); sigmaR <- exp(logsigmaR)
    
    # make predictions
    pred <- log(a*dats/(1 + b*dats))
    
    #calculate negative log like
    NegLogL <- (-1)*sum(dnorm(log(datr), pred, sigmaR, log = TRUE))
    return(NegLogL)
  }
  
  
  starts <- list(loga = log(bevholt_starts$a), logb = log(bevholt_starts$b), logsigmaR = 5)
  mle_out <- mle(BHminusLL, start = starts)
  
  # return output
  return(mle_out)
}

ricker <- function(stock){
  dats <- stock$sb
  datr <- stock$recruits
  
  RminusLL <- function(loga, logb, logsigmaR){
    # extract parameters
    a <- exp(loga); b <- exp(logb); sigmaR <- exp(logsigmaR)
    
    # make predictions
    pred <- log(a*dats*exp(-b*dats))
    
    #calculate negative log like
    NegLogL <- (-1)*sum(dnorm(log(datr), pred, sigmaR, log = TRUE))
    return(NegLogL)
  }
  
  
  starts <- list(loga = log(ricker_starts$a), logb = log(ricker_starts$b), logsigmaR = 5)
  mle_out <- mle(RminusLL, start = starts)
  
  # return output
  return(mle_out)
}

# Read in CSV files -------------------------------------------------------
# initial filter of stocks
takers_rec <- read_csv(here("results/original_analysis/csv_files", "takers_rec.csv"))
takers_rec <- takers_rec[,-1]

takers_ssb <- read_csv(here("results/original_analysis/csv_files", "takers_ssb.csv"))
takers_ssb <- takers_ssb[,-1]

# stocks to be included in analysis, along with the min/max year of the time series
use_stocks <- read_csv(here("results/original_analysis/csv_files", "use_stocks.csv"))
use_stocks <- use_stocks[,-1]

# stocks after fitting SR functions and divided by SR curve shape
dome_stocks <- read.csv(here("results/original_analysis/csv_files", "dome_stocks.csv"))
dome_stocks <- as_tibble(dome_stocks[,-1])

monotonic_stocks <- read_csv(here("results/original_analysis/csv_files", "monotonic_stocks.csv"))
monotonic_stocks <- as_tibble(monotonic_stocks[,-1])

# stocks divided by primary driver of recruitment (over observed biomass)
stock_model_fits <- read_csv(here("results/original_analysis/csv_files", "stock_model_fits.csv"))
stock_model_fits <- as_tibble(stock_model_fits[,-1])

# categorized stocks
sb_driven_stocks <- read_csv(here("results/original_analysis/csv_files", "sb_driven_stocks.csv"))
env_driven_stocks <- read_csv(here("results/original_analysis/csv_files", "env_driven_stocks.csv"))
edge_stocks <- read_csv(here("results/original_analysis/csv_files", "edge_stocks.csv"))
