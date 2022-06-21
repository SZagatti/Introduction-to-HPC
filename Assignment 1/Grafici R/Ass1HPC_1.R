dev.off()

library(readr)
library(ggplot2)
library(plotrix)
library(dplyr)
library(reshape2)
library("mxmaps")
library(scales)
library(MLmetrics)
library(caret)
# library(rstanarm)
library(ciTools)
library(gridExtra)
library(earth)
library(MASS)
library(rfinterval)
library(ranger)

t_communication <- 1*10^-6
t_read <- 1*10^-4
t_computation <- 2*10^-9
t_serial <- matrix(nrow = 100, ncol = 5)
t_parallel <- matrix(nrow = 100, ncol = 5)
N <- c(20000, 100000, 200000, 1000000, 20000000)
P <- seq(1, 100, 1)
for (k in seq(1, 100, 1)) {
  for (i in seq(1, 5, 1)){
  t_serial[k, i] <- t_read + N[i]*t_computation
  t_parallel[k, i] <- t_computation*(P[k]-1+N[i]/P[k]) + t_read + 2*(P[k]-1)*t_communication
  }
}

scalability <- t_serial/t_parallel
scalability <- as.data.frame(scalability)
scalability2 <- cbind(P, scalability)
scalability <- cbind(P,P, scalability)
colnames(scalability) <- c("P","linear","N=20000","N=100000","N=200000","N=1000000","N=20000000")
colnames(scalability2) <- c("P","N=20000","N=100000","N=200000","N=1000000","N=20000000")
scalability <- melt(scalability, id.vars="P")
colnames(scalability) <- c("P","N", "value")

ggplot(scalability, aes(P,value, col=N)) + 
  geom_point(size=1) +
  geom_line(sixe=2)+
  theme_minimal() +
  ylab("Scalability")