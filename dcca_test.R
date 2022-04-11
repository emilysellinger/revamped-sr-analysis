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

BevHolt_sim <- function(Nsims, Nyears, SB0, depletion, R0, h, SPR0){
  # create matrix for results
  sb_sims <- r_sims <- matrix(NA, nrow = Nyears, ncol = Nsims)
  sim_info <- matrix(NA, nrow = Nsims, ncol = 1)
  
  # simulation
  for(i in 1:Nsims){
    # draw random variability for each simulation
    sigmaR <- sample(c(0.1, 0.3, 0.5, 0.7, 0.9), 1, replace = TRUE)
    # record sim info
    sim_info[i,1] <- sigmaR
    # sample spawning biomass values
    sb_sims[,i] <- runif(Nyears, min = depletion*SB0, max = SB0)
    r_sims[,i] <- (0.8*R0*h*sb_sims[,i])/(0.2*SPR0*R0*(1 - h) + (h - 0.2)*sb_sims[,i])*exp(-rnorm(Nyears, 0, sigmaR))
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

# Ricker paramertization for simulations
Ricker_sim <- function(Nsims, Nyears, SB0, depletion, R0, h, S0){
  # create matrix for results
  sb_sims <- r_sims <- matrix(NA, nrow = Nyears, ncol = Nsims)
  sim_info <- matrix(NA, nrow = Nsims, ncol = 1)
  
  # simulation
  for(i in 1:Nsims){
    # draw random variability for each simulation
    sigmaR <- sample(c(0.1, 0.3, 0.5, 0.7, 0.9), 1, replace = TRUE)
    # record sim info
    sim_info[i,1] <- sigmaR
    # sample spawning biomass values
    sb_sims[,i] <- runif(Nyears, min = depletion*SB0, max = SB0)
    r_sims[,i] <- R0*(sb_sims[,i]/S0)*exp(log(5*h)*(1 - sb_sims[,i]/S0)/0.8)*exp(-rnorm(Nyears, 0, sigmaR))
  }
  
  return(list(sb_sims, r_sims, sim_info))
}
# calculate correlation coefficients for each method
get_corrs <- function(spawn, recruit, win_size, error, nu){
  
  # create matrix for results
  Nsims <- dim(spawn)[2]
  corr_df <- matrix(NA, nrow = Nsims, ncol = 2)
  
  # calculate correlation coefficients
  for(i in 1:Nsims){
    # spearman's correlation
    sp_rho <- cor.test(rank(recruit[,i]), rank(spawn[,i]))
    corr_df[i,1] <- sp_rho$estimate
    # dcca 
    dc_rho <- rhodcca(recruit[,i], spawn[,i], m = win_size, nu = nu)
    corr_df[i, 2] <- dc_rho$rhodcca
  }
  
  # create data frame
  corr_df <- tibble(sigmaR = as.vector(error),
                    sp_rho = corr_df[,1],
                    dcca_rho = corr_df[,2])
  
  return(corr_df)
}

# Simulations -------------------------------------------------------------
bh_sims1 <- BevHolt_sim(Nsims = 10000, Nyears = 50, SB0 = 150, depletion = 0.1, R0 = 20, SPR0 = 3, h = 0.3)
r_sims1 <- Ricker_sim(Nsims = 10000, Nyears = 50, SB0 = 150, depletion = 0.1, R0 = 20, h = 1, S0 = 150)

# extract data
bh_sb1 <- bh_sims1[[1]]
bh_r1 <- bh_sims1[[2]]
bh_sigmaR1 <- bh_sims1[[3]]

r_sb1 <- r_sims1[[1]]
r_r1 <- r_sims1[[2]]
r_sigmaR1 <- r_sims1[[3]]

# calculate correlations
bh_sim_corr1 <- get_corrs(spawn = bh_sb1, recruit = bh_r1, win_size = 10, nu = 1, error = bh_sigmaR1)
r_sim_corr1 <- get_corrs(spawn = r_sb1, recruit = r_r1, win_size = 10, error = r_sigmaR1, nu = 1)

# plot the results
ggplot(data = bh_sim_corr1) + geom_point(aes(x = sigmaR, y = sp_rho), color = "red") +
  geom_point(aes(x = sigmaR, y = dcca_rho), color = "blue") + xlab("recruitment error") +
  ylab("correlation coef")

ggplot(data = r_sim_corr1) + geom_point(aes(x = sigmaR, y = sp_rho), color = "red") +
  geom_point(aes(x = sigmaR, y = dcca_rho), color = "blue") + xlab("recruitment error") +
  ylab("correlation coef")


