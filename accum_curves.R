
library(Hmisc)
library(ggfortify)
library(ggplot2)

### MPH shore

MPHS <- read.table("MPH_Shore_Data.txt", header=TRUE)

MPHS.sel <- MPHS[,-1]

MPHS.org <- MPHS.sel[which(MPHS.sel$type=="org"),]

MPHS.conv <- MPHS.sel[which(MPHS.sel$type=="conv"),]

Nrep <- 500
data.l <- list(a1=MPHS.sel[,-1],a2=MPHS.conv[,-1],a3=MPHS.org[,-1])
Srichnesslist<-c()
for (line.i in 1:3){
  X <- data.l[[line.i]]
  SrichnesSites <-matrix(NA,nrow=nrow(X),ncol=Nrep)
  for (Nsites in 1:nrow(X)){
    for (irep in 1:Nrep){  
      sel <-sample(1:nrow(X),Nsites)
      SrichnesSites[Nsites,irep]<-sum( colSums(X[sel,]) > 0)
    }
  }
  
  mean.x <- rowMeans(SrichnesSites)
  sd.x <- apply(SrichnesSites, 1, sd)
  
  
  Srichnesslist <-c(Srichnesslist,list(SrichnesSites))
  
  errbar(1:nrow(X),mean.x,mean.x+sd.x,mean.x-sd.x,add=(line.i>1), xlab="Sites", ylab= "Taxa",pch= 21, cex=1.2, bg=c("seashell2","darkblue", "chartreuse3")[line.i])
}




### MPH Emerg


MPH_emerg <- read.table("MPH_water_emergent_allponds.txt", header=TRUE)

MPH_emerg.sel <- MPH_emerg[apply(MPH_emerg[,-c(1)], 1, function(x) !all(x==0)),] ## remove rows with only 0


MPH_emerg.org <- MPH_emerg.sel[which(MPH_emerg.sel$type=="org"),]

MPH_emerg.conv <- MPH_emerg.sel[which(MPH_emerg.sel$type=="conv"),]


Nrep <- 500
data.l <- list(a1=MPH_emerg.sel[,-1],a2=MPH_emerg.conv[,-1],a3=MPH_emerg.org[,-1])
Srichnesslist<-c()
for (line.i in 1:3){
  X <- data.l[[line.i]]
  SrichnesSites <-matrix(NA,nrow=nrow(X),ncol=Nrep)
  for (Nsites in 1:nrow(X)){
    for (irep in 1:Nrep){  
      sel <-sample(1:nrow(X),Nsites)
      SrichnesSites[Nsites,irep]<-sum( colSums(X[sel,]) > 0)
    }
  }
  
  mean.x <- rowMeans(SrichnesSites)
  sd.x <- apply(SrichnesSites, 1, sd)
  
  
  Srichnesslist <-c(Srichnesslist,list(SrichnesSites))
  
  errbar(1:nrow(X),mean.x,mean.x+sd.x,mean.x-sd.x,add=(line.i>1), xlab="Sites", ylab= "Taxa",pch= 21, cex=1.2, bg=c("seashell2","darkblue", "chartreuse3")[line.i])
}


### MPH Submerg

MPH_submerg <- read.table("MPH_submerged.txt", header=TRUE)

MPH_submerg.sel <- MPH_submerg[apply(MPH_submerg[,-c(1)], 1, function(x) !all(x==0)),] ## remove rows with only 0

MPH_submerg.org <- MPH_submerg.sel[which(MPH_submerg.sel$type=="org"),]

MPH_submerg.conv <- MPH_submerg.sel[which(MPH_submerg.sel$type=="conv"),]


Nrep <- 500
data.l <- list(a1=MPH_submerg.sel[,-1],a2=MPH_submerg.conv[,-1],a3=MPH_submerg.org[,-1])
Srichnesslist<-c()
for (line.i in 1:3){
  X <- data.l[[line.i]]
  SrichnesSites <-matrix(NA,nrow=nrow(X),ncol=Nrep)
  for (Nsites in 1:nrow(X)){
    for (irep in 1:Nrep){  
      sel <-sample(1:nrow(X),Nsites)
      SrichnesSites[Nsites,irep]<-sum( colSums(X[sel,]) > 0)
    }
  }
  
  mean.x <- rowMeans(SrichnesSites)
  sd.x <- apply(SrichnesSites, 1, sd)
  
  
  Srichnesslist <-c(Srichnesslist,list(SrichnesSites))
  
  errbar(1:nrow(X),mean.x,mean.x+sd.x,mean.x-sd.x,add=(line.i>1), xlab="Sites", ylab= "Taxa",pch= 21, cex=1.2, bg=c("seashell2","darkblue", "chartreuse3")[line.i])
}


### ZP

zp <- read.table("zp_data.txt", header=TRUE)


zp.sel <-  zp[,-c(2, 41,42)] #without copepods

zp.org <- zp.sel[which(zp.sel$Type=="org"),]

zp.conv <- zp.sel[which(zp.sel$Type=="conv"),]


Nrep <- 500
data.l <- list(a1=zp.sel[,-1],a2=zp.conv[,-1],a3=zp.org[,-1])
Srichnesslist<-c()
for (line.i in 1:3){
  X <- data.l[[line.i]]
  SrichnesSites <-matrix(NA,nrow=nrow(X),ncol=Nrep)
  for (Nsites in 1:nrow(X)){
    for (irep in 1:Nrep){  
      sel <-sample(1:nrow(X),Nsites)
      SrichnesSites[Nsites,irep]<-sum( colSums(X[sel,]) > 0)
    }
  }
  
  mean.x <- rowMeans(SrichnesSites)
  sd.x <- apply(SrichnesSites, 1, sd)
  
  
  Srichnesslist <-c(Srichnesslist,list(SrichnesSites))
  
  
  errbar(1:nrow(X),mean.x,mean.x+sd.x,mean.x-sd.x,add=(line.i>1), xlab="Sites", ylab= "Taxa",pch= 21, cex=1.2, bg=c("seashell2","darkblue", "chartreuse3")[line.i])
}



#### Col

MI_Col<- read.table("Coleoptera.txt", header=TRUE)


MI_Col.org <- MI_Col[which(MI_Col$type=="org"),]

MI_Col.conv <- MI_Col[which(MI_Col$type=="conv"),]

Nrep <- 500
data.l <- list(a1=MI_Col[,-c(1,2)],a2=MI_Col.conv[,-c(1,2)],a3=MI_Col.org[,-c(1,2)])
Srichnesslist<-c()
for (line.i in 1:3){
  X <- data.l[[line.i]]
  SrichnesSites <-matrix(NA,nrow=nrow(X),ncol=Nrep)
  for (Nsites in 1:nrow(X)){
    for (irep in 1:Nrep){  
      sel <-sample(1:nrow(X),Nsites)
      SrichnesSites[Nsites,irep]<-sum( colSums(X[sel,]) > 0)
    }
  }
  
  mean.x <- rowMeans(SrichnesSites)
  sd.x <- apply(SrichnesSites, 1, sd)
  
  
  Srichnesslist <-c(Srichnesslist,list(SrichnesSites))
  
  errbar(1:nrow(X),mean.x,mean.x+sd.x,mean.x-sd.x,add=(line.i>1), xlab="Sites", ylab= "Taxa",pch= 21, cex=1.2, bg=c("seashell2","darkblue", "chartreuse3")[line.i])
}


###Heteroptera


MI_Het<- read.table("Heteroptera.txt", header=TRUE)



MI_Het.org <- MI_Het[which(MI_Het$type=="org"),]

MI_Het.conv <- MI_Het[which(MI_Het$type=="conv"),]

Nrep <- 500
data.l <- list(a1=MI_Het[,-c(1,2)],a2=MI_Het.conv[,-c(1,2)],a3=MI_Het.org[,-c(1,2)])
Srichnesslist<-c()
for (line.i in 1:3){
  X <- data.l[[line.i]]
  SrichnesSites <-matrix(NA,nrow=nrow(X),ncol=Nrep)
  for (Nsites in 1:nrow(X)){
    for (irep in 1:Nrep){  
      sel <-sample(1:nrow(X),Nsites)
      SrichnesSites[Nsites,irep]<-sum( colSums(X[sel,]) > 0)
    }
  }
  
  mean.x <- rowMeans(SrichnesSites)
  sd.x <- apply(SrichnesSites, 1, sd)
  
  
  Srichnesslist <-c(Srichnesslist,list(SrichnesSites))
  
  errbar(1:nrow(X),mean.x,mean.x+sd.x,mean.x-sd.x,add=(line.i>1), xlab="Sites", ylab= "Taxa",pch= 21, cex=1.2, bg=c("seashell2","darkblue", "chartreuse3")[line.i])
}

###Gastropoda



MI_Gast<- read.table("gastropoda.txt", header=TRUE)



MI_Gast.org <- MI_Gast[which(MI_Gast$type=="org"),]

MI_Gast.conv <- MI_Gast[which(MI_Gast$type=="conv"),]

Nrep <- 500
data.l <- list(a1=MI_Gast[,-c(1,2)],a2=MI_Gast.conv[,-c(1,2)],a3=MI_Gast.org[,-c(1,2)])
Srichnesslist<-c()
for (line.i in 1:3){
  X <- data.l[[line.i]]
  SrichnesSites <-matrix(NA,nrow=nrow(X),ncol=Nrep)
  for (Nsites in 1:nrow(X)){
    for (irep in 1:Nrep){  
      sel <-sample(1:nrow(X),Nsites)
      SrichnesSites[Nsites,irep]<-sum( colSums(X[sel,]) > 0)
    }
  }
  
  mean.x <- rowMeans(SrichnesSites)
  sd.x <- apply(SrichnesSites, 1, sd)
  
  
  Srichnesslist <-c(Srichnesslist,list(SrichnesSites))
  
  errbar(1:nrow(X),mean.x,mean.x+sd.x,mean.x-sd.x,add=(line.i>1), xlab="Sites", ylab= "Taxa",pch= 21, cex=1.2, bg=c("seashell2","darkblue", "chartreuse3")[line.i])
}


