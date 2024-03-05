setwd("/Users/carolinelestrom/Documents/KU/FinKont2/Handins/Handin1")

library(png) ### Plots
library(knitr) ### Data manipulations
library(tidyverse) ### Data manipulations
library(dtplyr) ### Data manipulations - merge datasets
library(ggplot2) ### Plots
library(gridExtra) ### Plots






# Q2 - Quanto Hedging and The Kingdom of Denmark Put

## Q2.b

### Suitably initiated

### INITIALIZE
### ==========

S0 <- 30000
Y0 <- 1/100
X0 <- Y0
rUS <- 0.03
rJ <- 0
sigmaX <- c(0.1, 0.02)
sigmaJ <- c(0, 0.25)
timetomat <- 2
t <- 0
strike <- S0

muJ <- 0
muX <- 0


Nhedge <- 252*timetomat
Nrep <- 10000



### FUNCTIONS
### =========

### d1 and d2 from option price formula
d1 <- function(S, t, K, rJ, sigmaX, sigmaJ, timetomat) {
  (c(log(S / K)) + (rJ - t(sigmaX) %*% sigmaJ + norm(sigmaJ, type = "2")^2 / 2) * 
     (timetomat - t)) / (sqrt(timetomat - t) * norm(sigmaJ, type = "2"))
}

d2 <- function(S, t, K, rJ, sigmaX, sigmaJ, timetomat) {
  (c(log(S / K)) + (rJ - t(sigmaX) %*% sigmaJ - norm(sigmaJ, type = "2")^2 / 2) *
     (timetomat - t)) / (sqrt(timetomat - t) * norm(sigmaJ, type = "2"))
}


### g - Price derivative w.r.t. stock
g_QP <- function(S, t, Y0, K, rUS, rJ, sigmaX, sigmaJ, timetomat) {
  d1 <- d1(S, t, K, rJ, sigmaX, sigmaJ, timetomat)
  StockDerivative_QP <- Y0 * exp((rJ - t(sigmaX) %*% sigmaJ - rUS) * (timetomat - t)) * (pnorm(d1) - 1)
  return(StockDerivative_QP)
}
### Pricing function
F_QP <- function(S, t, Y0, K, rUS, rJ, sigmaX, sigmaJ, timetomat) {
  d1 <- d1(S, t, K, rJ, sigmaX, sigmaJ, timetomat)
  d2 <- d2(S, t, K, rJ, sigmaX, sigmaJ, timetomat)
  OptionPrice_QP <- Y0 * exp(-rUS * (timetomat - t)) * (K * pnorm(-d2) - exp((rJ -t(sigmaX) %*% sigmaJ) * (timetomat - t)) * S * pnorm(-d1))
}




### 1st HEDGE
### =========

St <- rep(S0, length = Nrep)
Xt <- rep(X0, length = Nrep)
dt <- timetomat / Nhedge

initialoutlay <- c(F_QP(S0, t, Y0, strike, rUS, rJ, sigmaX, sigmaJ, timetomat))
Vpf <- rep(initialoutlay, length = Nrep)
a <- g_QP(St, t, Y0, strike, rUS, rJ, sigmaX, sigmaJ, timetomat) / Xt
b <- Vpf - a * St * Xt

for(i in 2:Nhedge) {
  ### Value of stock
  W <- cbind(rnorm(Nrep), rnorm(Nrep))
  St <- St * exp((muJ + rJ - t(sigmaX) %*% sigmaJ - 0.5 * norm(sigmaJ, type = "2")^2) * dt + c(W %*% sigmaJ) * sqrt(dt))
  ### Value of exchange rate
  Xt <- Xt * exp((muX + rUS - rJ - 0.5 * norm(sigmaX, type = "2")^2) * dt + c(W %*% sigmaX) * sqrt(dt))
  ### Value of portfolio
  Vpf <- a * St * Xt + b * exp(dt * rUS)
  ### Value of portfolio (stock)
  a <- g_QP(St, ((i - 1) * dt), Y0, strike, rUS, rJ, sigmaX, sigmaJ, timetomat) / Xt
  ### Value of portfolio (bank account)
  b <- Vpf - a * St * Xt
}

### Value of stock
W <- cbind(rnorm(Nrep), rnorm(Nrep))
ST <- St * exp((muJ + rJ - t(sigmaX) %*% sigmaJ - 0.5 * norm(sigmaJ, type = "2")^2) * dt 
               + c(W %*% sigmaJ) * sqrt(dt))
### Value of exchange rate
XT <- Xt * exp((muX + rUS - rJ - 0.5 * norm(sigmaX, type = "2")^2) * dt + c(W %*% sigmaX) 
               * sqrt(dt))
### Value of portfolio
Vpf <- a * ST * XT + b * exp(dt * rUS)


### PLOT
### ====

plot(ST, Vpf, col = "blue", xlab = "S(T)", ylab = "Value of hedge portfolio(T)", 
     main = "1st Hedge of Quanto Put Option",
     xlim = range(ST))
text(13000, 200, paste("# hegde points =", Nhedge), adj = 0)
points(sort(ST), Y0 * pmax(strike - sort(ST), 0), type = "l", lwd = 3)



hedgedata <- as.data.frame(cbind(ST, Vpf))



p1_daily <- hedgedata %>%
  ggplot(aes(x = ST, y = Vpf)) +
  geom_point(color = "#901a1E", size = 1.7, shape = 1) +
  theme_bw() +
  xlab("S(T)") + ylab("Value of hedge portfolio(T)") +
  ggtitle("1st Hedge of Quanto Put Option") +
  theme(plot.title = element_text(size=23, hjust=0)) +
  theme(axis.title = element_text(size=17)) +
  theme(axis.text.x = element_text(size=13, angle = 0, vjust = 0.7),
        axis.text.y = element_text(size=13)) +
  geom_line(aes(x = sort(ST), y = Y0 * pmax(strike - sort(ST), 0)), size = 1.3, 
            color = "steelblue") +
  annotate(geom="text", x=57000, y=200, label=paste("# hegde points =", Nhedge),
           color="#39641c", size = 7) +
  scale_x_continuous(breaks=c(25000, 50000, 75000, 100000)) +
  scale_y_continuous(breaks=c(0, 50, 100, 150, 200))




### 2nd HEDGE
### =========

St <- rep(S0, length = Nrep)
Xt <- rep(X0, length = Nrep)
dt <- timetomat / Nhedge

initialoutlay <- c(F_QP(S0, t, Y0, strike, rUS, rJ, sigmaX, sigmaJ, timetomat))
Vpf <- rep(initialoutlay, length = Nrep)
a <- g_QP(St, t, Y0, strike, rUS, rJ, sigmaX, sigmaJ, timetomat) / Xt
b <- (-a) * St
c <- Vpf - a * St * Xt - b * Xt

for(i in 2:Nhedge) {
  #Value of stock
  W <- cbind(rnorm(Nrep), rnorm(Nrep))
  St <- St * exp((muJ + rJ - t(sigmaX) %*% sigmaJ - 0.5 * norm(sigmaJ, type = "2")^2) * dt
                 + c(W %*% sigmaJ) * sqrt(dt))
  #Value of exchange rate
  Xt <- Xt * exp((muX + rUS - rJ - 0.5 * norm(sigmaX, type = "2")^2) * dt + c(W %*% sigmaX)
                 * sqrt(dt))
  #Value of portfolio
  Vpf <- a * St * Xt + b * exp(dt * rUS) * Xt + c * exp(dt * rUS)
  #Value of portfolio (stock)
  a <- g_QP(St, ((i - 1) * dt), Y0, strike, rUS, rJ, sigmaX, sigmaJ, timetomat) / Xt
  #Value of portfolio (foreign bank account)
  b <- (-a) * St
  #Value of portfolio (domestic bank account)
  c <- Vpf - a * St * Xt - b * Xt
}

#Value of stock
W <- cbind(rnorm(Nrep), rnorm(Nrep))
ST <- St * exp((muJ + rJ - t(sigmaX) %*% sigmaJ - 0.5 * norm(sigmaJ, type = "2")^2) * dt
               + c(W %*% sigmaJ) * sqrt(dt))
#Value of exchange rate
XT <- Xt * exp((muX + rUS - rJ - 0.5 * norm(sigmaX, type = "2")^2) * dt + c(W %*% sigmaX) 
               * sqrt(dt))
#Value of portfolio
Vpf <- a * ST * XT + b * exp(dt * rUS) * XT + c * exp(dt * rUS)



### PLOT
### ====


plot(ST, Vpf, col = "blue", xlab = "S(T)", ylab = "Value of hedge portfolio(T)", 
     main = "2nd Hedge of Quanto Put Option",
     xlim = range(ST))
text(13000, 200, paste("# hegde points =", Nhedge), adj = 0)
points(sort(ST), Y0 * pmax(strike - sort(ST), 0), type = "l", lwd = 3)


hedgedata <- as.data.frame(cbind(ST, Vpf))



p2_daily <- hedgedata %>%
  ggplot(aes(x = ST, y = Vpf)) +
  geom_point(color = "#901a1E", size = 1.7, shape = 1) +
  theme_bw() +
  xlab("S(T)") + ylab("Value of hedge portfolio(T)") +
  ggtitle("2nd Hedge of Quanto Put Option") +
  theme(plot.title = element_text(size=23, hjust=0)) +
  theme(axis.title = element_text(size=17)) +
  theme(axis.text.x = element_text(size=13, angle = 0, vjust = 0.7),
        axis.text.y = element_text(size=13)) +
  geom_line(aes(x = sort(ST), y = Y0 * pmax(strike - sort(ST), 0)), size = 1.3,
            color = "steelblue") +
  annotate(geom="text", x=47000, y=200, label=paste("# hegde points =", Nhedge),
           color="#39641c", size = 7) +
  scale_x_continuous(breaks=c(25000, 50000, 75000, 100000)) +
  scale_y_continuous(breaks=c(0, 50, 100, 150, 200))





### In the limit

Nhedge <- 252*timetomat*8
Nrep <- 10000


### 1st HEDGE
### =========

St <- rep(S0, length = Nrep)
Xt <- rep(X0, length = Nrep)
dt <- timetomat / Nhedge

initialoutlay <- c(F_QP(S0, t, Y0, strike, rUS, rJ, sigmaX, sigmaJ, timetomat))
Vpf <- rep(initialoutlay, length = Nrep)
a <- g_QP(St, t, Y0, strike, rUS, rJ, sigmaX, sigmaJ, timetomat) / Xt
b <- Vpf - a * St * Xt

for(i in 2:Nhedge) {
  ### Value of stock
  W <- cbind(rnorm(Nrep), rnorm(Nrep))
  St <- St * exp((muJ + rJ - t(sigmaX) %*% sigmaJ - 0.5 * norm(sigmaJ, type = "2")^2) * dt 
                 + c(W %*% sigmaJ) * sqrt(dt))
  ### Value of exchange rate
  Xt <- Xt * exp((muX + rUS - rJ - 0.5 * norm(sigmaX, type = "2")^2) * dt + c(W %*% sigmaX) 
                 * sqrt(dt))
  ### Value of portfolio
  Vpf <- a * St * Xt + b * exp(dt * rUS)
  ### Value of portfolio (stock)
  a <- g_QP(St, ((i - 1) * dt), Y0, strike, rUS, rJ, sigmaX, sigmaJ, timetomat) / Xt
  ### Value of portfolio (bank account)
  b <- Vpf - a * St * Xt
}

### Value of stock
W <- cbind(rnorm(Nrep), rnorm(Nrep))
ST <- St * exp((muJ + rJ - t(sigmaX) %*% sigmaJ - 0.5 * norm(sigmaJ, type = "2")^2) * dt 
               + c(W %*% sigmaJ) * sqrt(dt))
### Value of exchange rate
XT <- Xt * exp((muX + rUS - rJ - 0.5 * norm(sigmaX, type = "2")^2) * dt + c(W %*% sigmaX) 
               * sqrt(dt))
### Value of portfolio
Vpf <- a * ST * XT + b * exp(dt * rUS)


### PLOT
### ====

plot(ST, Vpf, col = "blue", xlab = "S(T)", ylab = "Value of hedge portfolio(T)", 
     main = "1st Hedge of Quanto Put Option",
     xlim = range(ST))
text(13000, 200, paste("# hegde points =", Nhedge), adj = 0)
points(sort(ST), Y0 * pmax(strike - sort(ST), 0), type = "l", lwd = 3)


hedgedata <- as.data.frame(cbind(ST, Vpf))


p1_limit <- hedgedata %>%
  ggplot(aes(x = ST, y = Vpf)) +
  geom_point(color = "#901a1E", size = 1.7, shape = 1) +
  theme_bw() +
  xlab("S(T)") + ylab("Value of hedge portfolio(T)") +
  ggtitle("1st Hedge of Quanto Put Option") +
  theme(plot.title = element_text(size=23, hjust=0)) +
  theme(axis.title = element_text(size=17)) +
  theme(axis.text.x = element_text(size=13, angle = 0, vjust = 0.7),
        axis.text.y = element_text(size=13)) +
  geom_line(aes(x = sort(ST), y = Y0 * pmax(strike - sort(ST), 0)), size = 1.3, 
            color = "steelblue") +
  annotate(geom="text", x=57000, y=200, label=paste("# hegde points =", Nhedge),
           color="#39641c", size = 7) +
  scale_x_continuous(breaks=c(25000, 50000, 75000, 100000)) +
  scale_y_continuous(breaks=c(0, 50, 100, 150, 200))



### 2nd HEDGE
### =========

St <- rep(S0, length = Nrep)
Xt <- rep(X0, length = Nrep)
dt <- timetomat / Nhedge

initialoutlay <- c(F_QP(S0, t, Y0, strike, rUS, rJ, sigmaX, sigmaJ, timetomat))
Vpf <- rep(initialoutlay, length = Nrep)
a <- g_QP(St, t, Y0, strike, rUS, rJ, sigmaX, sigmaJ, timetomat) / Xt
b <- (-a) * St
c <- Vpf - a * St * Xt - b * Xt

for(i in 2:Nhedge) {
  #Value of stock
  W <- cbind(rnorm(Nrep), rnorm(Nrep))
  St <- St * exp((muJ + rJ - t(sigmaX) %*% sigmaJ - 0.5 * norm(sigmaJ, type = "2")^2) * dt 
                 + c(W %*% sigmaJ) * sqrt(dt))
  #Value of exchange rate
  Xt <- Xt * exp((muX + rUS - rJ - 0.5 * norm(sigmaX, type = "2")^2) * dt + c(W %*% sigmaX) 
                 * sqrt(dt))
  #Value of portfolio
  Vpf <- a * St * Xt + b * exp(dt * rUS) * Xt + c * exp(dt * rUS)
  #Value of portfolio (stock)
  a <- g_QP(St, ((i - 1) * dt), Y0, strike, rUS, rJ, sigmaX, sigmaJ, timetomat) / Xt
  #Value of portfolio (foreign bank account)
  b <- (-a) * St
  #Value of portfolio (domestic bank account)
  c <- Vpf - a * St * Xt - b * Xt
}

#Value of stock
W <- cbind(rnorm(Nrep), rnorm(Nrep))
ST <- St * exp((muJ + rJ - t(sigmaX) %*% sigmaJ - 0.5 * norm(sigmaJ, type = "2")^2) * dt 
               + c(W %*% sigmaJ) * sqrt(dt))
#Value of exchange rate
XT <- Xt * exp((muX + rUS - rJ - 0.5 * norm(sigmaX, type = "2")^2) * dt + c(W %*% sigmaX) 
               * sqrt(dt))
#Value of portfolio
Vpf <- a * ST * XT + b * exp(dt * rUS) * XT + c * exp(dt * rUS)


### PLOT
### ====

plot(ST, Vpf, col = "blue", xlab = "S(T)", ylab = "Value of hedge portfolio(T)", 
     main = "2nd Hedge of Quanto Put Option",
     xlim = range(ST))
text(13000, 200, paste("# hegde points =", Nhedge), adj = 0)
points(sort(ST), Y0 * pmax(strike - sort(ST), 0), type = "l", lwd = 3)



hedgedata <- as.data.frame(cbind(ST, Vpf))



p2_limit <- hedgedata %>%
  ggplot(aes(x = ST, y = Vpf)) +
  geom_point(color = "#901a1E", size = 1.7, shape = 1) +
  theme_bw() +
  xlab("S(T)") + ylab("Value of hedge portfolio(T)") +
  ggtitle("2nd Hedge of Quanto Put Option") +
  theme(plot.title = element_text(size=23, hjust=0)) +
  theme(axis.title = element_text(size=17)) +
  theme(axis.text.x = element_text(size=13, angle = 0, vjust = 0.7),
        axis.text.y = element_text(size=13)) +
  geom_line(aes(x = sort(ST), y = Y0 * pmax(strike - sort(ST), 0)), size = 1.3,
            color = "steelblue") +
  annotate(geom="text", x=47000, y=200, label=paste("# hegde points =", Nhedge),
           color="#39641c", size = 7) +
  scale_x_continuous(breaks=c(25000, 50000, 75000, 100000)) +
  scale_y_continuous(breaks=c(0, 50, 100, 150, 200))




### FINAL PLOT
### ==========

#png(file="2b.png")


grid.arrange(p1_daily, p1_limit, p2_daily, p2_limit, ncol = 2)


#dev.off()