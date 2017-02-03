rm(list=ls())
library('foreign')
setwd("~/Documents/Luis/Research Projects/CowLogRigo/Data")
dir()
rigo <- read.spss('N = 327 Seis carreras Bach Ped y Psi dos instrumentos.sav',to.data.frame = T)
# write.csv(rigo,file='base_rigo.csv')
class(rigo)
mode(rigo)
dim(rigo)
names(rigo)
head(rigo,3)
for(i in 1:dim(rigo)[2]){
  print(c(i,names(rigo)[i],unique(rigo[,i]),levels(rigo[,i])))
}

misconceptions <- array(dim=c(nrow(rigo),25))
c_m <- 0
for(i in 6:30){
  c_m <- c_m+1
  misconceptions[,c_m] <- as.numeric(rigo[,i])
}
save(misconceptions,file='misconceptions.Rdata')

friedrich <- array(dim=c(nrow(rigo),21))
c_f <- 0
for(i in 31:51){
  c_f <- c_f+1
  friedrich[,c_f] <- as.numeric(rigo[,i])
}
save(friedrich,file='friedrich.Rdata')

jarrett <- function(matrix,title,max_s=8,min_s=0,pch='normal'){
  plot(0,type='n',ylim=c(-ncol(matrix),-1),xlim=c(0,6),axes=F,xlab='',ylab='')
  for(r in 1:nrow(matrix)){
    lines(matrix[r,],-1:-ncol(matrix),col=rgb(0,0,0,alpha=.005),lwd=7)
  }
  for(c in 1:ncol(matrix)){
    sizes <- vector(length=7)
    sizes[1] <- ((max_s-min_s)/nrow(matrix))*sum(matrix[,c]==0,na.rm=T)+min_s
    sizes[2] <- ((max_s-min_s)/nrow(matrix))*sum(matrix[,c]==1,na.rm=T)+min_s
    sizes[3] <- ((max_s-min_s)/nrow(matrix))*sum(matrix[,c]==2,na.rm=T)+min_s
    sizes[4] <- ((max_s-min_s)/nrow(matrix))*sum(matrix[,c]==3,na.rm=T)+min_s
    sizes[5] <- ((max_s-min_s)/nrow(matrix))*sum(matrix[,c]==4,na.rm=T)+min_s
    sizes[6] <- ((max_s-min_s)/nrow(matrix))*sum(matrix[,c]>4,na.rm=T)+min_s
    sizes[7] <- ((max_s-min_s)/nrow(matrix))*sum(is.na(matrix[,c]))+min_s
    cases <<- vector(length=7)
    cases[1] <<- sum(matrix[,c]==0,na.rm=T)
    cases[2] <<- sum(matrix[,c]==1,na.rm=T)
    cases[3] <<- sum(matrix[,c]==2,na.rm=T)
    cases[4] <<- sum(matrix[,c]==3,na.rm=T)
    cases[5] <<- sum(matrix[,c]==4,na.rm=T)
    cases[6] <<- sum(matrix[,c]>4,na.rm=T)
    cases[7] <<- sum(is.na(matrix[,c]))
    if(pch=='normal'){
      pch_g <- 0
    }
    if(pch=='cases'){
      pch_g <<- paste(cases)
      sizes <- rep(2,7)
    }
    text(0:6,rep(-c,7),pch_g,cex=sizes,lwd=2)
  }
  axis(1,at=0:6,labels=c(paste(0:4),'>4','NA'))
  axis(2,at=-1:-ncol(matrix),labels=paste(1:ncol(matrix)),las=1)
  mtext('Respuesta',1,line=2.5)
  mtext('Pregunta',2,line=2.5)
  mtext(title,3,line=1,cex=2)
}

setwd('/Users/Luis/Documents/Luis/Research Projects/CowLogRigo/Graphs')
pdf('patterns_one.pdf',width=8,height=11)
jarrett(misconceptions,'misconceptions',pch='cases')
jarrett(friedrich,'friedrich',pch='cases')
dev.off()

coltrane <- function(matrix,var_1,var_2,max_s=5,min_s=0){
  plot(0,type='n',xlim=c(0.5,4.5),ylim=c(.5,4.5),axes=F)
  for(i in 1:4){
    for(j in 1:4){
      cases <- sum(matrix[,var_1]==i&matrix[,var_2]==j,na.rm=T)
      size <- ((max_s-min_s)/nrow(matrix))*cases+min_s
      points(j,i,pch=0,cex=size)
    }
  }
}


matrix <- misconceptions
title <- 'misconceptions'
base_matrix <- matrix(ncol=ncol(matrix),nrow=ncol(matrix))
upper <- 0
for(r in 1:ncol(matrix)){
  row <- c(seq(upper+1,by=1,length.out=r),rep(0,ncol(matrix)-r))
  base_matrix[r,] <- row
  upper <- max(row)
}
setwd('/Users/Luis/Documents/Luis/Research Projects/CowLogRigo/Graphs')
archive <- paste('correlation_',title,'.pdf',sep='')
pdf(archive,width=ncol(matrix)*1.2,height=ncol(matrix)*.7)
layout(base_matrix)
par(mar=rep(1,4),oma=c(3,3,0,0))
for(var_1 in 1:ncol(matrix)){
  for(var_2 in 1:var_1){
    max_size <- 5
    if(var_1!=var_2){
      max_size <- 8
    }
    coltrane(matrix,var_1,var_2,max_s = max_size,min_s = .3)
    if(var_2==1){
      mtext(paste(var_1),2,cex=2,las=1,line=2,adj=0.5,col='gray80')
    }
    if(var_1==ncol(matrix)){
      mtext(paste(var_2),1,cex=2,line=2,col='gray80')
    }
  }
}
dev.off()

















mis_con <- misconceptions

setwd("/Users/Luis/Documents/Luis/Research Projects/CowLogRigo/Graphs")
pdf(file='nice_graph.pdf',width=20,height=1.9)
par(mar=c(1,1,1,1),xaxs='i',bg='black')
# cols <- c('#FF0000F0','#00FF00F0')
cols <- c('#FF0000D0','#FF000050','#00FF0050','#00FF00D0')
plot(0,type='n',axes=F,ann=F,
     ylim=c(1,dim(mis_con)[2]),xlim=c(1-(dim(mis_con)[1]*.01),dim(mis_con)[1]*1.01))
for(p in 1:dim(mis_con)[1]){
  for(q in 1:dim(mis_con)[2]){
    # points(p,q,pch=21,bg=cols[mis_con[p,q]+1],cex=.8,lwd=.3,col=NA)
    points(p,q,pch=21,bg=cols[mis_con[p,q]],cex=.8,lwd=.3,col=NA)
  }
}
axis(2,at=1:25,cex.axis=.8,las=1,pos=-1,tck=-.005,hadj=0.5)
dev.off()




library('R2jags')
mis_con <- array(dim=dim(misconceptions))
mis_con[misconceptions<=2] <- 0
mis_con[misconceptions>=3] <- 1
n_participants <- dim(mis_con)[1]
n_questions <- dim(mis_con)[2]
data_jags <- list('mis_con','n_participants','n_questions')

# m1_MC: 'model 1, misconceptions'
write('model{
  # Observed level
  for(p in 1:n_participants){
    for(q in 1:n_questions){
      mis_con[p,q]~dbern(theta[p,q])
      predictive[p,q]~dbern(theta[p,q])
      
      # No Rasch:      
      # theta[p,q] <- lat_par[p]*lat_ques[q]

      # Rasch model:
      theta[p,q] <- exp(lat_par[p]-lat_ques[q])/(1+exp(lat_par[p]-lat_ques[q]))
    }
  }
  
  # Priors
  for(p in 1:n_participants){
    lat_par[p]~dunif(-5,5)
  }
  for(q in 1:n_questions){
    lat_ques[q]~dunif(-5,5)
  }
}','m1_MC.bug')

nodes <- c('theta','lat_par','lat_ques','predictive')

start <- list(list(lat_par=runif(n_participants,-3,3),
                   lat_ques=runif(n_questions,-3,3)),
              list(lat_par=runif(n_participants,-3,3),
                   lat_ques=runif(n_questions,-3,3)),
              list(lat_par=runif(n_participants,-3,3),
                   lat_ques=runif(n_questions,-3,3)))

posteriors <- jags(
  data=data_jags,
  inits=start,
  parameters.to.save=nodes,
  model.file='m1_MC.bug',
  n.chains=3,
  n.iter=20000,
  n.burnin=15000,
  n.thin=5)
posteriors$observed_data <- mis_con
summary(posteriors$BUGSoutput$summary)

setwd("/Users/Luis/Documents/Luis/Research Projects/CowLogRigo/Results")
save(posteriors,file='miscon_binary_rasch.Rdata')
# save(posteriors,file='miscon_binary_NOrasch.Rdata')
# 
# setwd("~/ArtWork/CoolPosteriors")
# save(posteriors,file='cool_posteriors_RASCH123.Rdata')




rm(list=ls())
source('~/Documents/Luis/Research Projects/Machine Learning and Data Mining/Tic2000/mining_coil2000_functions.R')
setwd("~/Documents/Luis/Research Projects/CowLogRigo/Results/")
archive <- 'miscon_binary_rasch.Rdata'
load(archive)

observed <- posteriors$observed_data
lat_par <- posteriors$BUGSoutput$sims.list$lat_par
lat_ques <- posteriors$BUGSoutput$sims.list$lat_ques
predictive <- posteriors$BUGSoutput$sims.list$predictive
n_part <- dim(observed)[1]
n_quest <- dim(observed)[2]
model <- capture.output(posteriors$model)

# mean prediction
mean_pred <- array(dim=dim(predictive)[2:3])
for(p in 1:dim(mean_pred)[1]){
  for(q in 1:dim(mean_pred)[2]){
    mean_pred[p,q] <- mean(predictive[,p,q])
  }
}
prediction <- array(dim=dim(mean_pred))
prediction[mean_pred>=.5] <- 1
prediction[mean_pred<.5] <- 0


base_plot_0 <- function(){
  plot(0,type='n',axes=F,ann=F,
       ylim=c(1,n_quest),xlim=c(1-n_part*.01,n_part*1.06))
  axis(2,at=c(1,seq(5,25,5)),cex.axis=.8,las=1,pos=-1,tck=-.025,hadj=0.25,col=col_axes,col.axis=col_axes)
  axis(1,at=c(1,seq(100,300,100),n_part),tck=-0.025,padj=-1.5,col=col_axes,col.axis=col_axes,cex.axis=.8)
  # axis(4,col=col_axes,col.axis=col_axes)
}

setwd("/Users/Luis/Documents/Luis/Research Projects/CowLogRigo/Graphs")
pdf(file=paste('summary_',strsplit(archive,split='.Rdata')[[1]],'.pdf',sep=''),width=24,height=12.4)
col_axes <- 'gray50'

layout(matrix(c(1,1,1,
                2,2,2,
                3,5,6,
                4,5,6),ncol=3,byrow=T),heights=c(.35,.35,1))

par(mar=rep(0,4),xaxs='i',bg='black')

base_plot_0()
cols <- c('#FF0000F0','#00FF00F0')
for(p in 1:n_part){
  for(q in 1:n_quest){
    points(p,q,pch=22,bg=cols[observed[p,q]+1],cex=.8,lwd=.3,col=NA)
  }
}
legend(n_part+1,n_quest/2,xjust = 0,yjust = 0.5,
       legend=c('En Desacuerdo','De Acuerdo'),
       pt.bg=cols,pch=22,pt.cex=2,col=NA,
       text.col='gray60',
       box.lty='blank',cex=.8)


base_plot_0()
cols_pred <- c('#FF0000F0','#00FF00F0')
count_agreements <- 0
for(p in 1:n_part){
  for(q in 1:n_quest){
    if(observed[p,q]==prediction[p,q]){
      cex=.8
      count_agreements <- count_agreements+1
    }
    else{
      cex=0
    }
    points(p,q,pch=22,bg=cols_pred[prediction[p,q]+1],col=NA,cex=cex)
  }
}
text(n_part+n_part*.03,n_quest/2,
     paste(round(count_agreements*100/cumprod(dim(observed))[2],2),'%',sep=''),
     col='gray60',cex=1.8,adj=0.5)

plot(0,type='n',xlim=c(-10,10),ylim=c(0,10),axes=F,ann=F)
axis(1,col.axis=col_axes,col=col_axes)
abline(v=0.5,lty='dashed',col=col_axes)
for(p in 1:n_part){
  lines(density(lat_par[,p]),
        col='#BBBBBBB0',lwd=.5)
}

plot(0,type='n',xlim=c(0,1),ylim=c(0,80),axes=F,ann=F)
axis(1,col.axis=col_axes,col=col_axes)
abline(v=0.5,lty='dashed',col=col_axes)
for(q in 1:n_quest){
  lines(density(lat_ques[,q]),
        col='#BBBBBBB0',lwd=3)
  text(mean(lat_ques[,q]),
       max(density(lat_ques[,q])$y*1.1),
       paste(q),font=2,col='#DDDDDDB0',cex=2)
}

plot(0,type='n',axes=F,ann=F)
text(1,seq(.8,-.8,length.out=length(model)),model,col='gray80',cex=2)

classifier <- cbind(as.vector(mean_pred),as.vector(observed))
colnames(classifier) <- c('score','target')
roc <- roc_values(classifier,bootstrapping = F)
plot(0,type = 'n',xlim=c(0,1),ylim=c(0,1),axes=F,ann=F)
lines(roc$normal_roc$fp,roc$normal_roc$tp_rates,col='#EEEEEEB0',lwd=3)
# points(roc$normal_roc$fp,roc$normal_roc$tp_rates)
axis(3,pos=1,col=col_axes)
axis(2,pos=0,col=col_axes)
text(.75,.25,
     paste(round(roc$normal_roc$auc,3)),cex=2,font=2,col='gray70')

dev.off()





























rm(list=ls())
setwd("~/Documents/Luis/Research Projects/CowLogRigo/Results/")
archive <- 'miscon_binary_rasch.Rdata'
load(archive)
observed <- posteriors$observed_data
lat_par <- posteriors$BUGSoutput$sims.list$lat_par
lat_ques <- posteriors$BUGSoutput$sims.list$lat_ques
# predictive <- posteriors$BUGSoutput$sims.list$predictive
# n_part <- dim(observed)[1]
n_quest <- dim(observed)[2]
model <- capture.output(posteriors$model)

setwd("/Users/Luis/Documents/Luis/Research Projects/CowLogRigo/Graphs")
pdf(file=paste('ICCs_',strsplit(archive,split='.Rdata')[[1]],'.pdf',sep=''),width=24,height=12.4)
layout(matrix(1:25,ncol=5,byrow=T))
support <- seq(-10,10,0.05)
par(mar=rep(2,4))
for(q in 1:n_quest){
  plot(0,type='n',xlim=c(-10,10),ylim=c(0,1),ann=F)
  mtext(paste('Question',q))
  abline(v=0,lty='dashed')
  abline(h=0.5,lty='dashed')
  for(i in 1:dim(lat_ques)[1]){
    lines(support,exp(support-lat_ques[i,q])/(1+exp(support-lat_ques[i,q])),
          col='#0000FF03')
  }
}
dev.off()



















rm(list=ls())
setwd("~/Documents/Luis/Research Projects/CowLogRigo/Data")
dir()
load('clean_friedrich.Rdata')
clean_friedrich
load('misconceptions.Rdata')
misconceptions

base <- misconceptions
# base <- clean_friedrich
layout(1:2)
plot(0,type='n',xlim=c(1,4),ylim=c(0,1))
for(question in 1:ncol(base)){
  points(table(base[,question])/nrow(base),type='o',pch=16)
}
plot(0,type='n',xlim=c(1,4),ylim=c(0,1))
for(participante in 1:nrow(base)){
  points(table(base[participante,])/ncol(base),type='o',pch=16)
}




