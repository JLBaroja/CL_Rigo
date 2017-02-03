rm(list=ls())
setwd("~/Documents/Luis/Research Projects/CowLogRigo/Data")
load('clean_friedrich.Rdata')
load('misconceptions.Rdata')

nice_plot <- function(base){
  plot(0,type='n',axes=F,ann=F,
       ylim=c(1,dim(base)[2]),xlim=c(1-(dim(base)[1]*.01),dim(base)[1]*1.01))
  for(p in 1:dim(base)[1]){
    for(q in 1:dim(base)[2]){
      # points(p,q,pch=21,bg=cols[base[p,q]+1],cex=.8,lwd=.3,col=NA)
      points(p,q,pch=21,bg=cols[base[p,q]],cex=.8,lwd=.3,col=NA)
    }
  }
  axis(2,at=1:dim(base)[2],labels=colnames(base),
       cex.axis=.4,las=1,pos=-1,tck=-.005,hadj=0.5,col='white',col.axis='white')
}

setwd("/Users/Luis/Documents/Luis/Research Projects/CowLogRigo/Graphs")
pdf(file='nice_summary.pdf',width=20,height=1.9)
par(mar=c(1,1,1,1),xaxs='i',bg='black')
# cols <- c('#FF0000F0','#00FF00F0')
cols <- c('#FF0000D0','#FF000070','#00FF0070','#00FF00D0')
nice_plot(misconceptions)
nice_plot(clean_friedrich)
dev.off()


scrs_m <- apply(misconceptions,MARGIN=1,FUN=sum)
scrs_f <- apply(clean_friedrich,MARGIN=1,FUN=sum)
cases <- table(scrs_m,scrs_f)

plot(scrs_f,scrs_m,type='n')
for(c in as.numeric(colnames(cases))){
  for(r in as.numeric(rownames(cases))){
    points(c,r,pch=16,col='#00000030',cex=cases[paste(r),paste(c)])
  }
}

plot(0,type='n',xlim=c(-.5,4.5),ylim=c(-.5,4.5))
cases <- table(clean_friedrich[,'Q3'],clean_friedrich[,'Q21'])
for(c in as.numeric(colnames(cases))){
  for(r in as.numeric(rownames(cases))){
    points(c,r,pch=16,col='#00000030',cex=cases[paste(r),paste(c)]*.3)
  }
}


