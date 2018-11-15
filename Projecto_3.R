


#install.packages("boot")
#install.packages("bootstrap")

library("boot")
library("bootstrap")

#In this project you will conduct a Monte Carlo Study to estimate the coverage probabilities of the standard
#normal bootstrap confidence interval, the basic bootstrap confidence interval, and the percentile confidence
#interval. Discuss the simulation results for the cases where the sampled population is
#i. N(10, 2)
#ii. £q
#2
#(1), and
#iii. Exp(rate = 1)
#In each case, Check the empirical coverage rate for the sample mean. Find the proportion of times that the
#confidence intervals miss on the left and the proportion of times that the confidence intervals miss on the
#right.


############

m<-1000
dat<-matrix(nrow = m,ncol=3)
dat[,1]<-rnorm(m,mean=10,sd=2)
dat[,2]<-rchisq(m,df=2)
dat[,3]<-rexp(m,rate=1)
trails<-1000
TABLE<-matrix(nrow=3,ncol=9)
rownames(TABLE, do.NULL = TRUE, prefix = "row")
rownames(TABLE) <- c("Normal","Chi","Expon")

colnames(TABLE, do.NULL = TRUE, prefix = "col")
colnames(TABLE) <- c("Norm Left","Norm Right", "Norm Perc","Basic Left","Basic Right", "Basic Perc","Perc Left","Perc Right", "Perc Perc")

for(k in 1:3){
samplemean <-function(dat,ind){
  return(mean(dat[ind]))
}
boot.obj<-boot(dat[,k],statistic = samplemean,R=2000)
boot.ci<-(boot.ci(boot.obj,type = c("basic","norm","perc")))
left<-0
right<-0
percent<-0
left1<-0
right1<-0
percent1<-0
left2<-0
right2<-0
percent2<-0
for(i in 1:trails){
  
  dat<-matrix(nrow = m,ncol=3)
  dat[,1]<-rnorm(m,mean=10,sd=2)
  dat[,2]<-rchisq(m,df=2)
  dat[,3]<-rexp(m,rate=1)
  if (mean(dat[,k])<=boot.ci$normal[1,3]&&mean(dat[,k])>=boot.ci$normal[1,2]){
    percent<-percent+1
  }
  if (mean(dat[,k])>boot.ci$normal[1,3]){
    right<-right+1
  }
  if (mean(dat[,k])<boot.ci$normal[1,2]){
    left<-left+1
  }

  TABLE[k,1]<-left/trails
  TABLE[k,2]<-right/trails
  TABLE[k,3]<-percent/trails
 
  #Basic
  if (mean(dat[,k])<=boot.ci$basic[1,5]&&mean(dat[,k])>=boot.ci$basic[1,4]){
    percent1<-percent1+1
  }
  if (mean(dat[,k])>boot.ci$basic[1,5]){
    right1<-right1+1
  }
  if (mean(dat[,k])<boot.ci$basic[1,4]){
    left1<-left1+1
  }
  
  TABLE[k,4]<-left1/trails
  TABLE[k,5]<-right1/trails
  TABLE[k,6]<-percent1/trails
  
 #Percent
  if (mean(dat[,k])<=boot.ci$percent[1,5]&&mean(dat[,k])>=boot.ci$percent[1,4]){
    percent2<-percent2+1
  }
  if (mean(dat[,k])>boot.ci$percent[1,5]){
    right2<-right2+1
  }
  if (mean(dat[,k])<boot.ci$percent[1,4]){
    left2<-left2+1
  }
  
  TABLE[k,7]<-left2/trails
  TABLE[k,8]<-right2/trails
  TABLE[k,9]<-percent2/trails
  
  
}


}
#Table Creation


print(TABLE)
