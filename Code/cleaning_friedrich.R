rm(list=ls())
setwd("~/Documents/Luis/Research Projects/CowLogRigo/Data")
load('friedrich.Rdata')
friedrich_reversed_questions <- c(8,9,10,14,17,19,20)
friedrich_filler_questions <- c(1,2,5,11,15,21)
for(rq in friedrich_reversed_questions){
  friedrich[,rq] <- -friedrich[,rq]+5
}
colnames(friedrich) <- paste('Q',1:21,sep='')
clean_friedrich <- friedrich[,-friedrich_filler_questions]
# save(clean_friedrich,file='clean_friedrich.Rdata')
head(clean_friedrich)
hist(apply(clean_friedrich,MARGIN=1,FUN=sum),breaks=100)
pc <- princomp(clean_friedrich,cor=T)
summary(pc)
pc$loadings
plot(pc$sdev^2,axes=F)
factanal(clean_friedrich,rotation = 'varimax',factors=3)
factanal(clean_friedrich,rotation = 'none',factors=3)
factanal(clean_friedrich,rotation = 'promax',factors=3)


