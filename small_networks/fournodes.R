
source("~/Dropbox/projects/literably_equating/src/propogate2.R")
out<-list()
N<-4
sigma<-1
nr<-250

rms<-list()
for (ii in 1:25) {
    b<-c(0,rnorm(N,mean=.1,sd=sigma))
    thresholds<-matrix(c(1,2,.9),nrow=1,byrow=TRUE)
    out<-list()
    for (i in 1:(length(b)-1)) for (j in (i+1):length(b)) {
                                   if (j==5) thr<-0.25 else thr<-2
                                   n<-sample(1:round(thr*nr),1)
                                   sc<-rnorm(n)
                                   s1<-sc+b[i]+rnorm(n,sd=.1) 
                                   s2<-sc+b[j]+rnorm(n,sd=.1)
                                   out[[paste(i,j)]]<-cbind(i,j,s1,s2)
                               }
    df<-data.frame(do.call("rbind",out))
    names(df)<-c("t1","t2","s1","s2")
    #add name for each node
    test<-df$t1<df$t2
    t1<-df$t1
    t2<-df$t2
    s1<-df$s1
    s2<-df$s2
    df$t1<-ifelse(test,t1,t2)
    df$t2<-ifelse(test,t2,t1)
    df$s1<-ifelse(test,s1,s2)
    df$s2<-ifelse(test,s2,s1)
    df$nm<-paste(df$t1,df$t2,sep="-")
    #
    b.list<-propogate(df,niter=10000)
    est<-b.list[[length(b.list)]]
    se<-(b-est)^2
    rmse1<-sqrt(mean((b[-length(b)]-b[length(b)]) - (est[-length(est)]-est[length(est)]))^2)
    tmp<-df[df$t1==5 | df$t2==5,]
    tmp$del<-tmp$s1-tmp$s2
    z<-by(tmp$del,tmp$nm,mean)
    rmse2<-sqrt(mean((b[-length(b)]-b[length(b)]) - z)^2)
    rms[[ii]]<-c(rmse1,rmse2)
}
rms<-do.call("rbind",rms)
colMeans(rms)
