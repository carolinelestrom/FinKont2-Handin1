setwd("/Users/carolinelestrom/Documents/KU/FinKont2/Handins/Handin1")

library(png) ### Plots
library(knitr) ### Data manipulations
library(tidyverse) ### Data manipulations
library(dtplyr) ### Data manipulations - merge datasets
library(ggplot2) ### Plots
library(gridExtra) ### Plots





# Q1 - The Bachelier Model

## Q1.b


### FUNCTIONS
### ========

### Bachelier Pricing
BachelierFormula <- function(spot, strike, timetomat, t, sigma){
  X = (spot-strike)/(sigma*sqrt(timetomat-t)) 
  delta_bach <- pnorm(X) ### Delta
  pi_bach <- (spot-strike)*pnorm(X)+ sigma*sqrt(timetomat-t) * dnorm(X) ### arbitrage-free price
  return(pi_bach) 
}

### Black-Scholes Pricing
BlackScholesFormula <- function(spot, strike, t, r, div, timetomat, sigma){
  d1 = (1/(sigma*sqrt(timetomat-t))) * (log(spot/strike) + (r+ 0.5*sigma^2)*(timetomat-t))
  d2 = d1 - sigma * sqrt(timetomat-t)
  pi_BS = spot * exp(-div*timetomat)*pnorm(d1) - exp(-r*(timetomat-t))*strike*pnorm(d2)
  return(pi_BS)
}

### Implied Volatility
IV <- function(spot, strike, timetomat, sigma){
  difference <- function(sigmaIMP){ #sigmaIMP = implied volatility
    return(BachelierFormula(spot, strike, timetomat, 0, sigma) - 
             BlackScholesFormula(spot, strike, 0, 0, 0, timetomat, sigmaIMP))
  } 
  
  return(uniroot(difference, lower = 10^-6, upper = 10)$root) 
}



### INITIALIZE
### ==========

S0 = 100
T = 1
sigma = 15


### CALCULATIONS
### ============

K <- seq(50, 150, 0.1) #strike
ImpVol = rep(NA, length(K))

for (i in 1:length(K)) {
  ImpVol[i] = IV(S0, K[i], T, sigma)
}


### PLOT
### ====

data <- as.data.frame(cbind(K, ImpVol))

#png(file="1b.png")

data %>%
  ggplot(aes(x = K, y = ImpVol)) +
  geom_line(color = "#901a1E", size = 1.7) +
  theme_bw() +
  xlab("Strike") + ylab("Implied volatilities") +
  ggtitle("Implied volatility across strikes in the Bachelier model") +
  theme(plot.title = element_text(size=27, hjust=0)) +
  theme(axis.title = element_text(size=23)) +
  theme(axis.text.x = element_text(size=17, angle = 0, vjust = 0.7),
        axis.text.y = element_text(size=17)) +
  scale_x_continuous(breaks=c(50, 75, 100, 125, 150), limits = c(50, 150), 
                     labels = c(50, 75, "100 (ATM)", 125, 150)) +
  scale_y_continuous(breaks=c(0.12, 0.14, 0.16, 0.18, 0.20), limits = c(0.117, 0.21)) +
  geom_segment(aes(x = 100, y = 0.117, xend = 100, yend = 0.15), 
               linetype = "dotdash", linewidth = 1.3, color = "steelblue")  +
  geom_segment(aes(x = 50, y = 0.15, xend = 100, yend = 0.15), 
               linetype = "dotdash", linewidth = 1.3, color = "steelblue")



#dev.off()
