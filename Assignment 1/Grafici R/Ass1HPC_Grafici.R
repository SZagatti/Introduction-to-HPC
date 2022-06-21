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

test_10to08 <- melt(test_10to08, id.vars="P")
test_10to09 <- melt(test_10to09, id.vars="P")
test_10to10 <- melt(test_10to10, id.vars="P")
test_10to11 <- melt(test_10to11, id.vars="P")

ggplot(test_10to08, aes(P,value, col=variable)) + 
  geom_point(size=1) +
  geom_line(size=1)+ labs(title="Test 10to08") +
  theme_minimal() + xlab("# of processors (P)") +
  ylab("Run Time [s]") + theme(plot.title = element_text(size=30), axis.text=element_text(size=16), axis.title =element_text(size=20), legend.title = element_text(size=0), legend.text = element_text(size=18), legend.key.size = unit(2, "lines")) + guides(colour = guide_legend(override.aes = list(size=3)))

ggplot(test_10to09, aes(P,value, col=variable)) + 
  geom_point(size=1) +
  geom_line(size=1)+ labs(title="Test 10to09") +
  theme_minimal() + xlab("# of processors (P)") +
  ylab("Run Time [s]") + theme(plot.title = element_text(size=30), axis.text=element_text(size=16), axis.title =element_text(size=20), legend.title = element_text(size=0), legend.text = element_text(size=18), legend.key.size = unit(2, "lines")) + guides(colour = guide_legend(override.aes = list(size=3)))

ggplot(test_10to10, aes(P,value, col=variable)) + 
  geom_point(size=1) +
  geom_line(size=1)+ labs(title="Test 10to10") +
  theme_minimal() + xlab("# of processors (P)") +
  ylab("Run Time [s]") + theme(plot.title = element_text(size=30), axis.text=element_text(size=16), axis.title =element_text(size=20), legend.title = element_text(size=0), legend.text = element_text(size=18), legend.key.size = unit(2, "lines")) + guides(colour = guide_legend(override.aes = list(size=3)))

ggplot(test_10to11, aes(P,value, col=variable)) + 
  geom_point(size=1) +
  geom_line(size=1)+ labs(title="Test 10to11") +
  theme_minimal() + xlab("# of processors (P)") +
  ylab("Run Time [s]") + theme(plot.title = element_text(size=30), axis.text=element_text(size=16), axis.title =element_text(size=20), legend.title = element_text(size=0), legend.text = element_text(size=18), legend.key.size = unit(2, "lines")) + guides(colour = guide_legend(override.aes = list(size=3)))



########### STRONG SCALABILITY: PLOT RUNTIME VS PROCESSORS 10 TO 08 ###########

strong_runtime_vs_proc_10to08 <- cbind.data.frame(EL_strong_scalability_10to08, strong_scalability_10to08)
strong_runtime_vs_proc_10to08 <- strong_runtime_vs_proc_10to08[,-3]
strong_runtime_vs_proc_10to08 <- melt(strong_runtime_vs_proc_10to08, id.vars="P")
ggplot(strong_runtime_vs_proc_10to08, aes(P,value, col=variable)) + 
  geom_point(size=1) +
  geom_line(size=1)+ labs(title="Strong Scalability: Run Time vs Processors 10to08") +
  theme_minimal() + xlab("# of processors (P)") +
  ylab("Run Time [s]") + theme(plot.title = element_text(size=30), axis.text=element_text(size=16), axis.title =element_text(size=20), legend.title = element_text(size=0), legend.text = element_text(size=18), legend.key.size = unit(2, "lines")) + guides(colour = guide_legend(override.aes = list(size=3)))

########### STRONG SCALABILITY: PLOT RUNTIME VS PROCESSORS 10 TO 09 ###########

strong_runtime_vs_proc_10to09 <- cbind.data.frame(EL_strong_scalability_10to09, strong_scalability_10to09)
strong_runtime_vs_proc_10to09 <- strong_runtime_vs_proc_10to09[,-3]
strong_runtime_vs_proc_10to09 <- melt(strong_runtime_vs_proc_10to09, id.vars="P")
ggplot(strong_runtime_vs_proc_10to09, aes(P,value, col=variable)) + 
  geom_point(size=1) +
  geom_line(size=1)+ labs(title="Strong Scalability: Run Time vs Processors 10to09") +
  theme_minimal() + xlab("# of processors (P)") +
  ylab("Run Time [s]") + theme(plot.title = element_text(size=30), axis.text=element_text(size=16), axis.title =element_text(size=20), legend.title = element_text(size=0), legend.text = element_text(size=18), legend.key.size = unit(2, "lines")) + guides(colour = guide_legend(override.aes = list(size=3)))

########### STRONG SCALABILITY: PLOT RUNTIME VS PROCESSORS 10 TO 10 ###########

strong_runtime_vs_proc_10to10 <- cbind.data.frame(EL_strong_scalability_10to10, strong_scalability_10to10)
strong_runtime_vs_proc_10to10 <- strong_runtime_vs_proc_10to10[,-3]
strong_runtime_vs_proc_10to10 <- melt(strong_runtime_vs_proc_10to10, id.vars="P")
ggplot(strong_runtime_vs_proc_10to10, aes(P,value, col=variable)) + 
  geom_point(size=1) +
  geom_line(size=1)+ labs(title="Strong Scalability: Run Time vs Processors 10to10") +
  theme_minimal() + xlab("# of processors (P)") +
  ylab("Run Time [s]") + theme(plot.title = element_text(size=30), axis.text=element_text(size=16), axis.title =element_text(size=20), legend.title = element_text(size=0), legend.text = element_text(size=18), legend.key.size = unit(2, "lines")) + guides(colour = guide_legend(override.aes = list(size=3)))

########### STRONG SCALABILITY: PLOT RUNTIME VS PROCESSORS 10 TO 11 ###########

strong_runtime_vs_proc_10to11 <- cbind.data.frame(EL_strong_scalability_10to11, strong_scalability_10to11)
strong_runtime_vs_proc_10to11 <- strong_runtime_vs_proc_10to11[,-3]
strong_runtime_vs_proc_10to11 <- melt(strong_runtime_vs_proc_10to11, id.vars="P")
ggplot(strong_runtime_vs_proc_10to11, aes(P,value, col=variable)) + 
  geom_point(size=1) +
  geom_line(size=1)+ labs(title="Strong Scalability: Run Time vs Processors 10to11") +
  theme_minimal() + xlab("# of processors (P)") +
  ylab("Run Time [s]") + theme(plot.title = element_text(size=30), axis.text=element_text(size=16), axis.title =element_text(size=20), legend.title = element_text(size=0), legend.text = element_text(size=18), legend.key.size = unit(2, "lines")) + guides(colour = guide_legend(override.aes = list(size=3)))

########### WEAK SCALABILITY: PLOT RUNTIME VS PROCESSORS 10 TO 08 ###########

weak_runtime_vs_proc_10to08 <- cbind.data.frame(EL_weak_scalability_10to08, weak_scalability_10to08)
weak_runtime_vs_proc_10to08 <- weak_runtime_vs_proc_10to08[,-3]
weak_runtime_vs_proc_10to08 <- melt(weak_runtime_vs_proc_10to08, id.vars="P")
ggplot(weak_runtime_vs_proc_10to08, aes(P,value, col=variable)) + 
  geom_point(size=1) +
  geom_line(size=1)+ labs(title="Weak Scalability: Run Time vs Processors 10to08") +
  theme_minimal() + xlab("# of processors (P)") +
  ylab("Run Time [s]") + theme(plot.title = element_text(size=30), axis.text=element_text(size=16), axis.title =element_text(size=20), legend.title = element_text(size=0), legend.text = element_text(size=18), legend.key.size = unit(2, "lines")) + guides(colour = guide_legend(override.aes = list(size=3)))

########### WEAK SCALABILITY: PLOT RUNTIME VS PROCESSORS 10 TO 09 ###########

weak_runtime_vs_proc_10to09 <- cbind.data.frame(EL_weak_scalability_10to09, weak_scalability_10to09)
weak_runtime_vs_proc_10to09 <- weak_runtime_vs_proc_10to09[,-3]
weak_runtime_vs_proc_10to09 <- melt(weak_runtime_vs_proc_10to09, id.vars="P")
ggplot(weak_runtime_vs_proc_10to09, aes(P,value, col=variable)) + 
  geom_point(size=1) +
  geom_line(size=1)+ labs(title="Weak Scalability: Run Time vs Processors 10to09") +
  theme_minimal() + xlab("# of processors (P)") +
  ylab("Run Time [s]") + theme(plot.title = element_text(size=30), axis.text=element_text(size=16), axis.title =element_text(size=20), legend.title = element_text(size=0), legend.text = element_text(size=18), legend.key.size = unit(2, "lines")) + guides(colour = guide_legend(override.aes = list(size=3)))

########### WEAK SCALABILITY: PLOT RUNTIME VS PROCESSORS 10 TO 10 ###########

weak_runtime_vs_proc_10to10 <- cbind.data.frame(EL_weak_scalability_10to10, weak_scalability_10to10)
weak_runtime_vs_proc_10to10 <- weak_runtime_vs_proc_10to10[,-3]
weak_runtime_vs_proc_10to10 <- melt(weak_runtime_vs_proc_10to10, id.vars="P")
ggplot(weak_runtime_vs_proc_10to10, aes(P,value, col=variable)) + 
  geom_point(size=1) +
  geom_line(size=1)+ labs(title="Weak Scalability: Run Time vs Processors 10to10") +
  theme_minimal() + xlab("# of processors (P)") +
  ylab("Run Time [s]") + theme(plot.title = element_text(size=30), axis.text=element_text(size=16), axis.title =element_text(size=20), legend.title = element_text(size=0), legend.text = element_text(size=18), legend.key.size = unit(2, "lines")) + guides(colour = guide_legend(override.aes = list(size=3)))

########### WEAK SCALABILITY: PLOT RUNTIME VS PROCESSORS 10 TO 11 ###########

weak_runtime_vs_proc_10to11 <- cbind.data.frame(EL_weak_scalability_10to11, weak_scalability_10to11)
weak_runtime_vs_proc_10to11 <- weak_runtime_vs_proc_10to11[,-3]
weak_runtime_vs_proc_10to11 <- melt(weak_runtime_vs_proc_10to11, id.vars="P")
ggplot(weak_runtime_vs_proc_10to11, aes(P,value, col=variable)) + 
  geom_point(size=1) +
  geom_line(size=1)+ labs(title="Weak Scalability: Run Time vs Processors 10to11") +
  theme_minimal() + xlab("# of processors (P)") +
  ylab("Run Time [s]") + theme(plot.title = element_text(size=30), axis.text=element_text(size=16), axis.title =element_text(size=20), legend.title = element_text(size=0), legend.text = element_text(size=18), legend.key.size = unit(2, "lines")) + guides(colour = guide_legend(override.aes = list(size=3)))

######################

EL_strong_scalability_10to08[1,2] <- 2.57
EL_strong_scalability_10to09[1,2] <- 25.70
EL_strong_scalability_10to10[1,2] <- 256.90
EL_strong_scalability_10to11[1,2] <- 2570.95
EL_weak_scalability_10to08[1,2] <- 2.57
EL_weak_scalability_10to09[1,2] <- 25.70
EL_weak_scalability_10to10[1,2] <- 256.90
EL_weak_scalability_10to11[1,2] <- 2570.95

EL_strong_scalability_10to08[,2] <- as.numeric(EL_strong_scalability_10to08[1,2])/EL_strong_scalability_10to08[,2]
EL_strong_scalability_10to09[,2] <- as.numeric(EL_strong_scalability_10to09[1,2])/EL_strong_scalability_10to09[,2]
EL_strong_scalability_10to10[,2] <- as.numeric(EL_strong_scalability_10to10[1,2])/EL_strong_scalability_10to10[,2]
EL_strong_scalability_10to11[,2] <- as.numeric(EL_strong_scalability_10to11[1,2])/EL_strong_scalability_10to11[,2]
EL_weak_scalability_10to08[,2] <- as.numeric(EL_weak_scalability_10to08[1,2])/EL_weak_scalability_10to08[,2]
EL_weak_scalability_10to09[,2] <- as.numeric(EL_weak_scalability_10to09[1,2])/EL_weak_scalability_10to09[,2]
EL_weak_scalability_10to10[,2] <- as.numeric(EL_weak_scalability_10to10[1,2])/EL_weak_scalability_10to10[,2]
EL_weak_scalability_10to11[,2] <- as.numeric(EL_weak_scalability_10to11[1,2])/EL_weak_scalability_10to11[,2]
strong_scalability_10to08[,2] <- as.numeric(strong_scalability_10to08[1,2])/strong_scalability_10to08[,2]
strong_scalability_10to09[,2] <- as.numeric(strong_scalability_10to09[1,2])/strong_scalability_10to09[,2]
strong_scalability_10to10[,2] <- as.numeric(strong_scalability_10to10[1,2])/strong_scalability_10to10[,2]
strong_scalability_10to11[,2] <- as.numeric(strong_scalability_10to11[1,2])/strong_scalability_10to11[,2]
weak_scalability_10to08[,2] <- as.numeric(weak_scalability_10to08[1,2])/weak_scalability_10to08[,2]
weak_scalability_10to09[,2] <- as.numeric(weak_scalability_10to09[1,2])/weak_scalability_10to09[,2]
weak_scalability_10to10[,2] <- as.numeric(weak_scalability_10to10[1,2])/weak_scalability_10to10[,2]
weak_scalability_10to11[,2] <- as.numeric(weak_scalability_10to11[1,2])/weak_scalability_10to11[,2]


########### PLOT STRONG SCALABILITY 10 TO 08 ###########

strong_scalability_plot1_10to08 <- full_join(EL_strong_scalability_10to08, strong_scalability_10to08)
strong_scalability_plot1_10to08 <- cbind(EL_strong_scalability_10to08[,1],strong_scalability_plot1_10to08)
colnames(strong_scalability_plot1_10to08) <- c("P","Linear","Elapsed","Internal")
strong_scalability_plot1_10to08 <- melt(strong_scalability_plot1_10to08, id.vars="P")
colnames(strong_scalability_plot1_10to08) <- c("P","Scalability", "value")
ggplot(strong_scalability_plot1_10to08, aes(P,value, col=Scalability)) + 
  geom_point(size=1) +
  geom_line(size=1)+ labs(title="Strong Scalability 10to08") +
  theme_minimal() + xlab("# of processors (P)") +
  ylab("Scalability") + theme(plot.title = element_text(size=30), axis.text=element_text(size=16), axis.title =element_text(size=20), legend.title = element_text(size=0), legend.text = element_text(size=18), legend.key.size = unit(2, "lines")) + guides(colour = guide_legend(override.aes = list(size=3)))

########### PLOT STRONG SCALABILITY 10 TO 09 ###########

strong_scalability_plot1_10to09 <- full_join(EL_strong_scalability_10to09, strong_scalability_10to09)
strong_scalability_plot1_10to09 <- cbind(EL_strong_scalability_10to09[,1],strong_scalability_plot1_10to09)
colnames(strong_scalability_plot1_10to09) <- c("P","Linear","Elapsed","Internal")
strong_scalability_plot1_10to09 <- melt(strong_scalability_plot1_10to09, id.vars="P")
colnames(strong_scalability_plot1_10to09) <- c("P","Scalability", "value")
ggplot(strong_scalability_plot1_10to09, aes(P,value, col=Scalability)) + 
  geom_point(size=1) +
  geom_line(size=1)+ labs(title="Strong Scalability 10to09") +
  theme_minimal() + xlab("# of processors (P)") +
  ylab("Scalability") + theme(plot.title = element_text(size=30), axis.text=element_text(size=16), axis.title =element_text(size=20), legend.title = element_text(size=0), legend.text = element_text(size=18), legend.key.size = unit(2, "lines")) + guides(colour = guide_legend(override.aes = list(size=3)))

########### PLOT STRONG SCALABILITY 10 TO 10 ###########

strong_scalability_plot1_10to10 <- full_join(EL_strong_scalability_10to10, strong_scalability_10to10)
strong_scalability_plot1_10to10 <- cbind(EL_strong_scalability_10to10[,1],strong_scalability_plot1_10to10)
colnames(strong_scalability_plot1_10to10) <- c("P","Linear","Elapsed","Internal")
strong_scalability_plot1_10to10 <- melt(strong_scalability_plot1_10to10, id.vars="P")
colnames(strong_scalability_plot1_10to10) <- c("P","Scalability", "value")
ggplot(strong_scalability_plot1_10to10, aes(P,value, col=Scalability)) + 
  geom_point(size=1) +
  geom_line(size=1)+ labs(title="Strong Scalability 10to10") +
  theme_minimal() + xlab("# of processors (P)") +
  ylab("Scalability") + theme(plot.title = element_text(size=30), axis.text=element_text(size=16), axis.title =element_text(size=20), legend.title = element_text(size=0), legend.text = element_text(size=18), legend.key.size = unit(2, "lines")) + guides(colour = guide_legend(override.aes = list(size=3)))

########### PLOT STRONG SCALABILITY 10 TO 11 ###########

strong_scalability_plot1_10to11 <- full_join(EL_strong_scalability_10to11, strong_scalability_10to11)
strong_scalability_plot1_10to11 <- cbind(EL_strong_scalability_10to11[,1],strong_scalability_plot1_10to11)
colnames(strong_scalability_plot1_10to11) <- c("P","Linear","Elapsed","Internal")
strong_scalability_plot1_10to11 <- melt(strong_scalability_plot1_10to11, id.vars="P")
colnames(strong_scalability_plot1_10to11) <- c("P","Scalability", "value")
ggplot(strong_scalability_plot1_10to11, aes(P,value, col=Scalability)) + 
  geom_point(size=1) +
  geom_line(size=1)+ labs(title="Strong Scalability 10to11") +
  theme_minimal() + xlab("# of processors (P)") +
  ylab("Scalability") + theme(plot.title = element_text(size=30), axis.text=element_text(size=16), axis.title =element_text(size=20), legend.title = element_text(size=0), legend.text = element_text(size=18), legend.key.size = unit(2, "lines")) + guides(colour = guide_legend(override.aes = list(size=3)))

########### PLOT STRONG SCALABILITY 4 CASES ELAPSED ###########

colnames(EL_strong_scalability_10to08) <- c("P","10 to 08")
colnames(EL_strong_scalability_10to09) <- c("P","10 to 09")
colnames(EL_strong_scalability_10to10) <- c("P","10 to 10")
colnames(EL_strong_scalability_10to11) <- c("P","10 to 11")

Elapsed_strong_scalability_final_plot <- full_join(EL_strong_scalability_10to08,EL_strong_scalability_10to09)
Elapsed_strong_scalability_final_plot <- full_join(Elapsed_strong_scalability_final_plot,EL_strong_scalability_10to10)
Elapsed_strong_scalability_final_plot <- full_join(Elapsed_strong_scalability_final_plot,EL_strong_scalability_10to11)
Elapsed_strong_scalability_final_plot <- cbind(Elapsed_strong_scalability_final_plot[,1],Elapsed_strong_scalability_final_plot)
colnames(Elapsed_strong_scalability_final_plot) <- c("P","Linear","10 to 08","10 to 09","10 to 10","10 to 11")
Elapsed_strong_scalability_final_plot <- melt(Elapsed_strong_scalability_final_plot, id.vars="P")
colnames(Elapsed_strong_scalability_final_plot) <- c("P","Scalability", "value")
Elapsed_strong_scalability_final_plot <- Elapsed_strong_scalability_final_plot[complete.cases(Elapsed_strong_scalability_final_plot), ]
ggplot(Elapsed_strong_scalability_final_plot, aes(P,value, col=Scalability)) + 
  geom_point(size=1) +
  geom_line(size=1)+ labs(title="Strong Scalability (Elapsed Time)") +
  theme_minimal() + xlab("# of processors (P)") +
  ylab("Scalability") + theme(plot.title = element_text(size=30), axis.text=element_text(size=16), axis.title =element_text(size=20), legend.title = element_text(size=0), legend.text = element_text(size=18), legend.key.size = unit(2, "lines")) + guides(colour = guide_legend(override.aes = list(size=3)))

########### PLOT STRONG SCALABILITY 4 CASES INTERNAL ###########

colnames(strong_scalability_10to08) <- c("P","10 to 08")
colnames(strong_scalability_10to09) <- c("P","10 to 09")
colnames(strong_scalability_10to10) <- c("P","10 to 10")
colnames(strong_scalability_10to11) <- c("P","10 to 11")

Internal_strong_scalability_final_plot <- full_join(strong_scalability_10to08,strong_scalability_10to09)
Internal_strong_scalability_final_plot <- full_join(Internal_strong_scalability_final_plot,strong_scalability_10to10)
Internal_strong_scalability_final_plot <- full_join(Internal_strong_scalability_final_plot,strong_scalability_10to11)
Internal_strong_scalability_final_plot <- cbind(Internal_strong_scalability_final_plot[,1],Internal_strong_scalability_final_plot)
colnames(Internal_strong_scalability_final_plot) <- c("P","Linear","10 to 08","10 to 09","10 to 10","10 to 11")
Internal_strong_scalability_final_plot <- melt(Internal_strong_scalability_final_plot, id.vars="P")
colnames(Internal_strong_scalability_final_plot) <- c("P","Scalability", "value")
Internal_strong_scalability_final_plot <- Internal_strong_scalability_final_plot[complete.cases(Internal_strong_scalability_final_plot), ]
ggplot(Internal_strong_scalability_final_plot, aes(P,value, col=Scalability)) + 
  geom_point(size=1) +
  geom_line(size=1)+ labs(title="Strong Scalability (Internal Time)") +
  theme_minimal() + xlab("# of processors (P)") +
  ylab("Scalability") + theme(plot.title = element_text(size=30), axis.text=element_text(size=16), axis.title =element_text(size=20), legend.title = element_text(size=0), legend.text = element_text(size=18), legend.key.size = unit(2, "lines")) + guides(colour = guide_legend(override.aes = list(size=3)))


linear <- rep(1, times=13)

########### PLOT weak SCALABILITY 10 TO 08 ###########

weak_scalability_plot1_10to08 <- full_join(EL_weak_scalability_10to08, weak_scalability_10to08)
weak_scalability_plot1_10to08 <- cbind(linear,weak_scalability_plot1_10to08)
colnames(weak_scalability_plot1_10to08) <- c("Linear","P","Elapsed","Internal")
weak_scalability_plot1_10to08 <- melt(weak_scalability_plot1_10to08, id.vars="P")
colnames(weak_scalability_plot1_10to08) <- c("P","Scalability", "value")
ggplot(weak_scalability_plot1_10to08, aes(P,value, col=Scalability)) + 
  geom_point(size=1) +
  geom_line(size=1)+ labs(title="Weak Scalability 10to08") +
  theme_minimal() + xlab("# of processors (P)") +
  ylab("Scalability") + theme(plot.title = element_text(size=30), axis.text=element_text(size=16), axis.title =element_text(size=20), legend.title = element_text(size=0), legend.text = element_text(size=18), legend.key.size = unit(2, "lines")) + guides(colour = guide_legend(override.aes = list(size=3)))

########### PLOT weak SCALABILITY 10 TO 09 ###########

weak_scalability_plot1_10to09 <- full_join(EL_weak_scalability_10to09, weak_scalability_10to09)
weak_scalability_plot1_10to09 <- cbind(linear,weak_scalability_plot1_10to09)
colnames(weak_scalability_plot1_10to09) <- c("Linear","P","Elapsed","Internal")
weak_scalability_plot1_10to09 <- melt(weak_scalability_plot1_10to09, id.vars="P")
colnames(weak_scalability_plot1_10to09) <- c("P","Scalability", "value")
ggplot(weak_scalability_plot1_10to09, aes(P,value, col=Scalability)) + 
  geom_point(size=1) +
  geom_line(size=1)+ labs(title="Weak Scalability 10to09") +
  theme_minimal() + xlab("# of processors (P)") +
  ylab("Scalability") + theme(plot.title = element_text(size=30), axis.text=element_text(size=16), axis.title =element_text(size=20), legend.title = element_text(size=0), legend.text = element_text(size=18), legend.key.size = unit(2, "lines")) + guides(colour = guide_legend(override.aes = list(size=3)))

########### PLOT weak SCALABILITY 10 TO 10 ###########

weak_scalability_plot1_10to10 <- full_join(EL_weak_scalability_10to10, weak_scalability_10to10)
weak_scalability_plot1_10to10 <- cbind(linear,weak_scalability_plot1_10to10)
colnames(weak_scalability_plot1_10to10) <- c("Linear","P","Elapsed","Internal")
weak_scalability_plot1_10to10 <- melt(weak_scalability_plot1_10to10, id.vars="P")
colnames(weak_scalability_plot1_10to10) <- c("P","Scalability", "value")
ggplot(weak_scalability_plot1_10to10, aes(P,value, col=Scalability)) + 
  geom_point(size=1) +
  geom_line(size=1)+ labs(title="Weak Scalability 10to10") +
  theme_minimal() + xlab("# of processors (P)") +
  ylab("Scalability") + theme(plot.title = element_text(size=30), axis.text=element_text(size=16), axis.title =element_text(size=20), legend.title = element_text(size=0), legend.text = element_text(size=18), legend.key.size = unit(2, "lines")) + guides(colour = guide_legend(override.aes = list(size=3)))

########### PLOT weak SCALABILITY 10 TO 11 ###########

linear <- rep(1, times=4)

weak_scalability_plot1_10to11 <- full_join(EL_weak_scalability_10to11, weak_scalability_10to11)
weak_scalability_plot1_10to11 <- cbind(linear,weak_scalability_plot1_10to11)
colnames(weak_scalability_plot1_10to11) <- c("Linear","P","Elapsed","Internal")
weak_scalability_plot1_10to11 <- melt(weak_scalability_plot1_10to11, id.vars="P")
colnames(weak_scalability_plot1_10to11) <- c("P","Scalability", "value")
ggplot(weak_scalability_plot1_10to11, aes(P,value, col=Scalability)) + 
  geom_point(size=1) +
  geom_line(size=1)+ labs(title="Weak Scalability 10to11") +
  theme_minimal() + xlab("# of processors (P)") +
  ylab("Scalability") + theme(plot.title = element_text(size=30), axis.text=element_text(size=16), axis.title =element_text(size=20), legend.title = element_text(size=0), legend.text = element_text(size=18), legend.key.size = unit(2, "lines")) + guides(colour = guide_legend(override.aes = list(size=3)))

########### PLOT weak SCALABILITY 4 CASES ELAPSED ###########

linear <- rep(1, times=13)

colnames(EL_weak_scalability_10to08) <- c("P","10 to 08")
colnames(EL_weak_scalability_10to09) <- c("P","10 to 09")
colnames(EL_weak_scalability_10to10) <- c("P","10 to 10")
colnames(EL_weak_scalability_10to11) <- c("P","10 to 11")

Elapsed_weak_scalability_final_plot <- full_join(EL_weak_scalability_10to08,EL_weak_scalability_10to09)
Elapsed_weak_scalability_final_plot <- full_join(Elapsed_weak_scalability_final_plot,EL_weak_scalability_10to10)
Elapsed_weak_scalability_final_plot <- full_join(Elapsed_weak_scalability_final_plot,EL_weak_scalability_10to11)
Elapsed_weak_scalability_final_plot <- cbind(linear,Elapsed_weak_scalability_final_plot)
colnames(Elapsed_weak_scalability_final_plot) <- c("Linear","P", "10 to 08","10 to 09","10 to 10","10 to 11")
Elapsed_weak_scalability_final_plot <- melt(Elapsed_weak_scalability_final_plot, id.vars="P")
colnames(Elapsed_weak_scalability_final_plot) <- c("P","Scalability", "value")
Elapsed_weak_scalability_final_plot <- Elapsed_weak_scalability_final_plot[complete.cases(Elapsed_weak_scalability_final_plot), ]
ggplot(Elapsed_weak_scalability_final_plot, aes(P,value, col=Scalability)) + 
  geom_point(size=1) +
  geom_line(size=1)+ labs(title="Weak Scalability (Elapsed Time)") +
  theme_minimal() + xlab("# of processors (P)") +
  ylab("Scalability") + theme(plot.title = element_text(size=30), axis.text=element_text(size=16), axis.title =element_text(size=20), legend.title = element_text(size=0), legend.text = element_text(size=18), legend.key.size = unit(2, "lines")) + guides(colour = guide_legend(override.aes = list(size=3)))

########### PLOT weak SCALABILITY 4 CASES INTERNAL ###########

colnames(weak_scalability_10to08) <- c("P","10 to 08")
colnames(weak_scalability_10to09) <- c("P","10 to 09")
colnames(weak_scalability_10to10) <- c("P","10 to 10")
colnames(weak_scalability_10to11) <- c("P","10 to 11")

Internal_weak_scalability_final_plot <- full_join(weak_scalability_10to08,weak_scalability_10to09)
Internal_weak_scalability_final_plot <- full_join(Internal_weak_scalability_final_plot,weak_scalability_10to10)
Internal_weak_scalability_final_plot <- full_join(Internal_weak_scalability_final_plot,weak_scalability_10to11)
Internal_weak_scalability_final_plot <- cbind(linear,Internal_weak_scalability_final_plot)
colnames(Internal_weak_scalability_final_plot) <- c("Linear","P","10 to 08","10 to 09","10 to 10","10 to 11")
Internal_weak_scalability_final_plot <- melt(Internal_weak_scalability_final_plot, id.vars="P")
colnames(Internal_weak_scalability_final_plot) <- c("P","Scalability", "value")
Internal_weak_scalability_final_plot <- Internal_weak_scalability_final_plot[complete.cases(Internal_weak_scalability_final_plot), ]
ggplot(Internal_weak_scalability_final_plot, aes(P,value, col=Scalability)) + 
  geom_point(size=1) +
  geom_line(size=1)+ labs(title="Weak Efficiency (Internal Time)") +
  theme_minimal() + xlab("# of processors (P)") +
  ylab("Weak Efficiency") + theme(plot.title = element_text(size=30), axis.text=element_text(size=16), axis.title =element_text(size=20), legend.title = element_text(size=0), legend.text = element_text(size=18), legend.key.size = unit(2, "lines")) + guides(colour = guide_legend(override.aes = list(size=3)))
