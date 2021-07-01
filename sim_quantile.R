##make data
sim<-function(b) {
    out<-list()
    for (i in 1:(length(b)-1)) for (j in (i+1):length(b)) {
                                   test<-runif(1)
                                   if (test>.5) {
                                       n<-sample(1:250,1)
                                       sc<-rnorm(n)
                                       #qu<-pnorm(sc)
                                       qu<-ifelse(sc>0,1,0.25)
                                       s1<-sc+qu*b[i]+rnorm(n,sd=.1)
                                       s2<-sc+qu*b[j]+rnorm(n,sd=.1)
                                       out[[paste(i,j)]]<-cbind(i,j,s1,s2)
                                   }
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
    df
}



source("propogate2_qu.R")
b<-c(0,rnorm(25,sd=5))
df<-sim(b)
b2<-propogate(df,tau=.2,niter=2000)
b8<-propogate(df,tau=.8,niter=20000)

cor(data.frame(b,b2[[length(b2)]],b8[[length(b8)]]))
plot(b2[[length(b2)]],b8[[length(b8)]],xlim=range(b8[[length(b8)]]),ylim=range(b8[[length(b8)]])); abline(0,1)
summary(b2[[length(b2)]])
summary(b8[[length(b8)]])
