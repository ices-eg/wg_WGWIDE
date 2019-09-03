
rm(list=ls())

k     <- 0.205
Linf  <- 40
t0    <- 0

t <- seq(from = 0, to = 15, by = 0.1)

L <- Linf*(1-exp(-k*(t-t0)))

plot(t,L)


t50 <- 3.5
L50 <- Linf*(1-exp(-k*(t50-t0)))