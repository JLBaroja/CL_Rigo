rm(list=ls())
library('ltm')
library('eRm')
library('mirt')
library('mokken')
library('KernSmoothIRT')

setwd('/Users/Luis/Documents/Luis/Research Projects/CowLogRigo/Data/')
load('misconceptions.Rdata')
misconceptions[misconceptions==0] <- NA
misconceptions <- misconceptions-1
misconceptions_binary <- array(dim=dim(misconceptions))
misconceptions_binary[misconceptions<=2] <- 0
misconceptions_binary[misconceptions>=3] <- 1
rasch_miscon_bin <- RM(misconceptions_binary)
names(rasch_miscon_bin)
summary(rasch_miscon_bin)
twoPL_miscon_bin <- ltm(misconceptions_binary~z1,IRT.param = T)
summary(twoPL_miscon_bin)
treePL_miscon_bin <- tpm(misconceptions_binary)
layout(matrix(1:3,ncol=3))
plotjointICC(rasch_miscon_bin)
plot(twoPL_miscon_bin)
plot(treePL_miscon_bin)

onePL_miscon_bin <- rasch(misconceptions_binary)
plotPImap(onePL_miscon_bin)
