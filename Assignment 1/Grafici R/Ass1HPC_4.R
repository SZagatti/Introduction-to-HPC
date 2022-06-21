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
t_serial <- matrix(nrow = 100, ncol = 9)
t_parallel <- matrix(nrow = 100, ncol = 9)
N <- c(10000, 50000, 100000, 500000, 1000000, 5000000, 10000000, 50000000, 100000000)
P <- seq(1, 100, 1)
for (k in seq(1, 100, 1)) {
  for (i in seq(1, 9, 1)){
    t_serial[k, i] <- t_read + N[i]*t_computation
    t_parallel[k, i] <- t_computation*(P[k]-1+N[i]/P[k]) + t_read + 2*(P[k]-1)*t_communication
  }
}

scalability <- t_serial/t_parallel
scalability <- as.data.frame(scalability)
scalability2 <- cbind(P, scalability)
scalability <- cbind(P,P, scalability)
colnames(scalability) <- c("P","linear","N=10000", "N=50000", "N=100000", "N=500000", "N=1000000", "N=5000000", "N=10000000", "N=50000000", "N=100000000")
colnames(scalability2) <- c("P", "N=10000", "N=50000", "N=100000", "N=500000", "N=1000000", "N=5000000", "N=10000000", "N=50000000", "N=100000000")
scalability <- melt(scalability, id.vars="P")
colnames(scalability) <- c("P","N", "value")

ggplot(scalability, aes(P,value, col=N)) + 
  geom_point(size=1) +
  geom_line(size=1)+
  theme_minimal() + xlab("# of processors (P)") +
  ylab("Scalability") + theme(plot.title = element_text(size=30), axis.text=element_text(size=16), axis.title =element_text(size=20), legend.title = element_text(size=20), legend.text = element_text(size=18), legend.key.size = unit(2, "lines")) + guides(colour = guide_legend(override.aes = list(size=3)))