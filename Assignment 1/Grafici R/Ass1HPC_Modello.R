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


EL_strong_scalability_10to08 <- read_csv("Desktop/EL-strong-scalability-10to08.csv")
EL_strong_scalability_10to09 <- read_csv("Desktop/EL-strong-scalability-10to09.csv")
EL_strong_scalability_10to10 <- read_csv("Desktop/EL-strong-scalability-10to10.csv")
EL_strong_scalability_10to11 <- read_csv("Desktop/EL-strong-scalability-10to11.csv")
EL_weak_scalability_10to08 <- read_csv("Desktop/EL-weak-scalability-10to08.csv")
EL_weak_scalability_10to09 <- read_csv("Desktop/EL-weak-scalability-10to09.csv")
EL_weak_scalability_10to10 <- read_csv("Desktop/EL-weak-scalability-10to10.csv")
EL_weak_scalability_10to11 <- read_csv("Desktop/EL-weak-scalability-10to11.csv")
strong_scalability_10to08 <- read_csv("Desktop/Assignement 1/FILE DA CARICARE/strong-scalability-10to08.csv")
strong_scalability_10to09 <- read_csv("Desktop/Assignement 1/FILE DA CARICARE/strong-scalability-10to09.csv")
strong_scalability_10to10 <- read_csv("Desktop/Assignement 1/FILE DA CARICARE/strong-scalability-10to10.csv")
strong_scalability_10to11 <- read_csv("Desktop/Assignement 1/FILE DA CARICARE/strong-scalability-10to11.csv")
weak_scalability_10to08 <- read_csv("Desktop/Assignement 1/FILE DA CARICARE/weak-scalability-10to08.csv")
weak_scalability_10to09 <- read_csv("Desktop/Assignement 1/FILE DA CARICARE/weak-scalability-10to09.csv")
weak_scalability_10to10 <- read_csv("Desktop/Assignement 1/FILE DA CARICARE/weak-scalability-10to10.csv")
weak_scalability_10to11 <- read_csv("Desktop/Assignement 1/FILE DA CARICARE/weak-scalability-10to11.csv")

EL_strong_scalability_10to08 <- EL_strong_scalability_10to08[,-c(3,4,5,6)]
EL_strong_scalability_10to09 <- EL_strong_scalability_10to09[,-c(3,4,5,6)]
EL_strong_scalability_10to10 <- EL_strong_scalability_10to10[,-c(3,4,5,6)]
EL_strong_scalability_10to11 <- EL_strong_scalability_10to11[,-c(3,4,5,6)]
EL_weak_scalability_10to08 <- EL_weak_scalability_10to08[,-c(3,4,5,6)]
EL_weak_scalability_10to09 <- EL_weak_scalability_10to09[,-c(3,4,5,6)]
EL_weak_scalability_10to10 <- EL_weak_scalability_10to10[,-c(3,4,5,6)]
EL_weak_scalability_10to11 <- EL_weak_scalability_10to11[,-c(3,4,5,6)]
strong_scalability_10to08 <- strong_scalability_10to08[,-c(3,4,5,6)]
strong_scalability_10to09 <- strong_scalability_10to09[,-c(3,4,5,6)]
strong_scalability_10to10 <- strong_scalability_10to10[,-c(3,4,5,6)]
strong_scalability_10to11 <- strong_scalability_10to11[,-c(3,4,5,6)]
weak_scalability_10to08 <- weak_scalability_10to08[,-c(3,4,5,6)]
weak_scalability_10to09 <- weak_scalability_10to09[,-c(3,4,5,6)]
weak_scalability_10to10 <- weak_scalability_10to10[,-c(3,4,5,6)]
weak_scalability_10to11 <- weak_scalability_10to11[,-c(3,4,5,6)]

colnames(EL_strong_scalability_10to08) <- c("P","Elapsed")
colnames(EL_strong_scalability_10to09) <- c("P","Elapsed")
colnames(EL_strong_scalability_10to10) <- c("P","Elapsed")
colnames(EL_strong_scalability_10to11) <- c("P","Elapsed")
colnames(strong_scalability_10to08) <- c("P","Internal")
colnames(strong_scalability_10to09) <- c("P","Internal")
colnames(strong_scalability_10to10) <- c("P","Internal")
colnames(strong_scalability_10to11) <- c("P","Internal")

colnames(EL_weak_scalability_10to08) <- c("P","Elapsed")
colnames(EL_weak_scalability_10to09) <- c("P","Elapsed")
colnames(EL_weak_scalability_10to10) <- c("P","Elapsed")
colnames(EL_weak_scalability_10to11) <- c("P","Elapsed")
colnames(weak_scalability_10to08) <- c("P","Internal")
colnames(weak_scalability_10to09) <- c("P","Internal")
colnames(weak_scalability_10to10) <- c("P","Internal")
colnames(weak_scalability_10to11) <- c("P","Internal")

test_10to08 <- strong_scalability_10to08[,2] +0.27+0.015*strong_scalability_10to08[,1]
test_10to09 <- strong_scalability_10to09[,2]+0.27+0.015*strong_scalability_10to09[,1]
test_10to10 <- strong_scalability_10to10[,2]+0.27+0.015*strong_scalability_10to10[,1]
test_10to11 <- strong_scalability_10to11[,2]+0.27+0.015*strong_scalability_10to11[,1]
colnames(test_10to08) <- "Model"
colnames(test_10to09) <- "Model"
colnames(test_10to10) <- "Model"
colnames(test_10to11) <- "Model"

test_10to08 <- cbind(EL_strong_scalability_10to08,test_10to08)
test_10to09 <- cbind(EL_strong_scalability_10to09,test_10to09)
test_10to10 <- cbind(EL_strong_scalability_10to10,test_10to10)
test_10to11 <- cbind(EL_strong_scalability_10to11,test_10to11)

test_10to08_plot <- melt(test_10to08, id.vars="P")
test_10to09_plot <- melt(test_10to09, id.vars="P")
test_10to10_plot <- melt(test_10to10, id.vars="P")
test_10to11_plot <- melt(test_10to11, id.vars="P")

ggplot(test_10to08_plot, aes(P,value, col=variable)) + 
  geom_point(size=1) +
  geom_line(size=1)+ labs(title="Test 10to08") +
  theme_minimal() + xlab("# of processors (P)") +
  ylab("Run Time [s]") + theme(plot.title = element_text(size=30), axis.text=element_text(size=16), axis.title =element_text(size=20), legend.title = element_text(size=0), legend.text = element_text(size=18), legend.key.size = unit(2, "lines")) + guides(colour = guide_legend(override.aes = list(size=3)))

ggplot(test_10to09_plot, aes(P,value, col=variable)) + 
  geom_point(size=1) +
  geom_line(size=1)+ labs(title="Test 10to09") +
  theme_minimal() + xlab("# of processors (P)") +
  ylab("Run Time [s]") + theme(plot.title = element_text(size=30), axis.text=element_text(size=16), axis.title =element_text(size=20), legend.title = element_text(size=0), legend.text = element_text(size=18), legend.key.size = unit(2, "lines")) + guides(colour = guide_legend(override.aes = list(size=3)))

ggplot(test_10to10_plot, aes(P,value, col=variable)) + 
  geom_point(size=1) +
  geom_line(size=1)+ labs(title="Test 10to10") +
  theme_minimal() + xlab("# of processors (P)") +
  ylab("Run Time [s]") + theme(plot.title = element_text(size=30), axis.text=element_text(size=16), axis.title =element_text(size=20), legend.title = element_text(size=0), legend.text = element_text(size=18), legend.key.size = unit(2, "lines")) + guides(colour = guide_legend(override.aes = list(size=3)))

ggplot(test_10to11_plot, aes(P,value, col=variable)) + 
  geom_point(size=1) +
  geom_line(size=1)+ labs(title="Test 10to11") +
  theme_minimal() + xlab("# of processors (P)") +
  ylab("Run Time [s]") + theme(plot.title = element_text(size=30), axis.text=element_text(size=16), axis.title =element_text(size=20), legend.title = element_text(size=0), legend.text = element_text(size=18), legend.key.size = unit(2, "lines")) + guides(colour = guide_legend(override.aes = list(size=3)))

t_parallel <- read_csv("Desktop/T_parallel.csv")
t_internal <- read_csv("Desktop/T_internal.csv")

t_modello<- t_parallel[,1]

processori <- cbind(t_parallel[,1],t_parallel[,1],t_parallel[,1],t_parallel[,1],t_parallel[,1])
t_parallel <- t_parallel[,-1]
t_internal <- t_internal[,-1]

t_modello[,c(2:5)] <- t_internal-t_parallel
t_modello <- t_modello/processori
t_modello[,1] <- processori[,1]

t_modello <- melt(t_modello, id.vars="P")
t_modello <- t_modello[complete.cases(t_modello), ]

ggplot(t_modello, aes(P,value, col=variable)) + 
  geom_point(size=1) +
  geom_line(size=1)+ labs(title="Communication Time") +
  theme_minimal() + xlab("# of processors (P)") +
  ylab("T_communication") + theme(plot.title = element_text(size=30), axis.text=element_text(size=16), axis.title =element_text(size=20), legend.title = element_text(size=0), legend.text = element_text(size=18), legend.key.size = unit(2, "lines")) + guides(colour = guide_legend(override.aes = list(size=3)))
