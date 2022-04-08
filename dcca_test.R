library(DCCA)
library(forecast)
library(tidyverse)

# not sure this is the easiest way to determine correlation. I'm going to simulate
# an AR 1 process for spawning biomass, then calculate recruitment
SB2 <- arima.sim(list(order = c(1,0,0), ar = 0.5), n = 50, rand.gen = rnorm, sd = 100) + 1000
plot(SB2)


# Functions -----------------------------------------------------------
# going to set up a simulation similar to the one Cody used in his paper
# will choose a initial level of SB that is some % of unfished, then randomly choose 
# a value of sigmaR. From there, will sample 50 years of dynamics. At the end, will test Spearman's
# and DCCA on the stock

BevHolt_sim <- function(Nsims, Nyears, SB_init, depletion, a, b){
  # create matrix for results
  sb_sims <- r_sims <- matrix(NA, nrow = Nyears, ncol = Nsims)
  sim_info <- matrix(NA, nrow = Nsims, ncol = 1)
  
  # simulation
  for(i in 1:Nsims){
    # draw random variability for each simulation
    sigmaR <- runif(1, min = 0.05, max = 0.8)
    # record sim info
    sim_info[i,1] <- sigmaR
    # sample spawning biomass values
    sb_sims[,i] <- runif(Nyears, min = depletion*SB_init, max = SB_init)
    r_sims[,i] <- (a*sb_sims[,i])/(1 + b*sb_sims[,i])*exp(rnorm(Nyears, 0, sigmaR))
  }
  
  return(list(sb_sims, r_sims, sim_info))
}

# going to change how I simulate spawning biomass (AR(1))
BevHolt_sim2 <- function(Nsims, Nyears, SB_init, depletion, a, b){
  # create matrix for results
  sb_sims <- r_sims <- matrix(NA, nrow = Nyears, ncol = Nsims)
  sim_info <- matrix(NA, nrow = Nsims, ncol = 1)
  
  # simulation
  for(i in 1:Nsims){
    # draw random variability for each simulation
    sigmaR <- runif(1, min = 0.05, max = 0.8)
    # record sim info
    sim_info[i,1] <- sigmaR
    # sample spawning biomass values
    sb_sims[,i] <- runif(Nyears, min = depletion*SB_init, max = SB_init)
    r_sims[,i] <- (a*sb_sims[,i])/(1 + b*sb_sims[,i])*exp(rnorm(Nyears, 0, sigmaR))
  }
  
  return(list(sb_sims, r_sims, sim_info))
}

# calculate correlation coefficients for each method
get_corrs <- function(spawn, recruit, win_size){
  
  # create matrix for results
  Nsims <- dim(spawn)[2]
  corr_df <- matrix(NA, nrow = Nsims, ncol = 2)
  
  # calculate correlation coefficients
  for(i in 1:Nsims){
    # spearman's correlation
    sp_rho <- cor.test(rank(recruit[,i]), rank(spawn[,i]))
    corr_df[i,1] <- sp_rho$estimate
    # dcca 
    dc_rho <- rhodcca(recruit[,i], spawn[,i], m = win_size)
    corr_df[i, 2] <- dc_rho$rhodcca
  }
  
  return(corr_df)
}

# Simulations -------------------------------------------------------------
sims <- BevHolt_sim(Nsims = 10000, Nyears = 50, SB_init = 1000, depletion = 0.1, a = 10, b = 0.01)
sb <- sims[[1]]
r <- sims[[2]]
sigmaR <- sims[[3]]

sim_corrs <- get_corrs(spawn = sb, recruit = r, win_size = 5)

# will plot the results
sims_df <- tibble(sigmaR = sigmaR,
                  sp_rho = sim_corrs[,1],
                  dcca_rho = sim_corrs[,2])

ggplot(data = sims_df) + geom_point(aes(x = sigmaR, y = sp_rho), color = "red") +
  geom_point(aes(x = sigmaR, y = dcca_rho), color = "blue") + xlab("recruitment error") +
  ylab("correlation coef")

plot(seq(1,50,1), sb[,1], type = "l")
plot(seq(1,50,1), r[,1], type = "l")

