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

mpi_strong_scalability <- read_csv("Desktop/Scalability/mpi_strong_thin.csv")
mpi_weak_scalability <- read_csv("Desktop/Scalability/mpi_weak_thin.csv")
omp_strong_scalability <- read_csv("Desktop/Scalability/omp_strong_thin.csv")
omp_weak_scalability <- read_csv("Desktop/Scalability/omp_weak_thin.csv")

mpi_strong_scalability <- mpi_strong_scalability[,-c(2,3)]
mpi_weak_scalability <- mpi_weak_scalability[,-c(2,3)]
omp_strong_scalability <- omp_strong_scalability[,-c(2,3)]
omp_weak_scalability <- omp_weak_scalability[,-c(2,3)]

mpi_strong_scalability <- cbind(mpi_strong_scalability[,1], mpi_strong_scalability)
mpi_weak_scalability <- cbind(1, mpi_weak_scalability)
omp_strong_scalability <- cbind(omp_strong_scalability[,1], omp_strong_scalability)
omp_weak_scalability <- cbind(1, omp_weak_scalability)

colnames(mpi_strong_scalability) <- c("procs","linear","11x11 ref.val=69,61s", "31x31 ref.val=555,23s")
colnames(mpi_weak_scalability) <- c("linear","procs","11x11 ref.val=29,45s", "31x31 ref.val=234.48s")
colnames(omp_strong_scalability) <- c("threads","linear","11x11 ref.val=72.37s", "31x31 ref.val=590.15s")
colnames(omp_weak_scalability) <- c("linear","threads","11x11 ref.val=30.58s", "31x31 ref.val=249.54s")

mpi_strong_scalability <- melt(mpi_strong_scalability, id.vars="procs")
mpi_weak_scalability <- melt(mpi_weak_scalability, id.vars="procs")
omp_strong_scalability <- melt(omp_strong_scalability, id.vars="threads")
omp_weak_scalability <- melt(omp_weak_scalability, id.vars="threads")

ggplot(mpi_strong_scalability, aes(procs,value, col=variable)) + 
  geom_point(size=1) +
  geom_line(size=1)+ labs(title="MPI strong scalability on thin node") +
  theme_minimal() + xlab("# of procs (P)") +
  ylab("Scalability") + theme(plot.title = element_text(size=30), axis.text=element_text(size=16), axis.title =element_text(size=20), legend.title = element_text(size=0), legend.text = element_text(size=18), legend.key.size = unit(2, "lines")) + guides(colour = guide_legend(override.aes = list(size=3)))

ggplot(mpi_weak_scalability, aes(procs,value, col=variable)) + 
  geom_point(size=1) +
  geom_line(size=1)+ labs(title="MPI weak scalability on thin node") +
  theme_minimal() + xlab("# of procs (P)") +
  ylab("Scalability") + theme(plot.title = element_text(size=30), axis.text=element_text(size=16), axis.title =element_text(size=20), legend.title = element_text(size=0), legend.text = element_text(size=18), legend.key.size = unit(2, "lines")) + guides(colour = guide_legend(override.aes = list(size=3)))

ggplot(omp_strong_scalability, aes(threads,value, col=variable)) + 
  geom_point(size=1) +
  geom_line(size=1)+ labs(title="OpenMP strong scalability on thin node") +
  theme_minimal() + xlab("# of threads") +
  ylab("Scalability") + theme(plot.title = element_text(size=30), axis.text=element_text(size=16), axis.title =element_text(size=20), legend.title = element_text(size=0), legend.text = element_text(size=18), legend.key.size = unit(2, "lines")) + guides(colour = guide_legend(override.aes = list(size=3)))

ggplot(omp_weak_scalability, aes(threads,value, col=variable)) + 
  geom_point(size=1) +
  geom_line(size=1)+ labs(title="OpenMP weak scalability on thin node") +
  theme_minimal() + xlab("# of threads") +
  ylab("Scalability") + theme(plot.title = element_text(size=30), axis.text=element_text(size=16), axis.title =element_text(size=20), legend.title = element_text(size=0), legend.text = element_text(size=18), legend.key.size = unit(2, "lines")) + guides(colour = guide_legend(override.aes = list(size=3)))

