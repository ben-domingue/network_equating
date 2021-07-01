##make data
sim<-function(b, #the book-specific offsets (recovering these is the ultimate goal)
              nr #the max # of readings on a given edge
              ) {
                                        #this simulates data in a fairly vanlila way (edges are constructed for pairs of nodes (lines 9-10) based on a common score for a person, the text-specific offset, and noise)
    out<-list()
    for (i in 1:(length(b)-1)) for (j in (i+1):length(b)) {
                                   test<-runif(1)
                                   if (test>.5) { #i only want to populate some edges
                                       n<-sample(1:nr,1) #and i want the edges to be differentially weighted
                                       sc<-rnorm(n)
                                       s1<-sc+b[i]+rnorm(n,sd=.1) 
                                       s2<-sc+b[j]+rnorm(n,sd=.1)
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


##this block simulates data and then 
source("propogate_meandiff.R")
out<-list()
N<-200
sigma<-1
nr<-200
b<-c(0,rnorm(N,mean=.1,sd=sigma)) #the first book is the baseline book, note it has an offset of 0
df<-sim(b,nr=nr)
b.list<-propogate(df,niter=4000) #this is the list of estimates carried from iteration to iteration
est<-do.call("rbind",b.list) 


##visualize (hopefully relatively straightfwd but not going to comment until we have something more funcational)
par(mfrow=c(3,1),mgp=c(2,1,0),mar=c(3,3,1,1))
matplot(est,type='l',lty=1,col='darkgray',ylim=c(-4,4))
legend("bottomleft",bty='n',"movement of estimates")
#text(rep(length(b.list),length(b)),b.list[length(b.list)],1:length(b),pos=4,cex=.8)
##
plot(b,b.list[[length(b.list)]],xlab="true",ylab="est"); abline(0,1)
rmse<-sqrt(mean((b-b.list[[length(b.list)]])^2))
b0<-b.list[[1]]
b1<-b.list[[length(b.list)]]
test<-b0!=0
rmse0<-sqrt(mean((b[test]-b0[test])^2))
rmse1<-sqrt(mean((b[test]-b1[test])^2))
r<-cor(b,b1)
legend("topleft",bty='n',c(
                             paste("rmse=",format(rmse,digits=3),sep=''),
                             paste("r=",format(r,digits=3),sep=''),
                             paste("ratio=",format(rmse1/rmse0,digits=3),sep='')
                         )
       )
##
z<-b.list[seq(1,length(b.list),length.out=100)]
zz<-numeric()
for (i in 1:length(z)) zz[[i]]<-mean((z[[i]]-b)^2)
plot(zz,type='b',xlab="iteration (1-beginning, 100-end)",ylab="MSE")
