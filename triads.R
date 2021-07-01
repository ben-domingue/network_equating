

triad<-function(df,
                N=20,
                eqfun=mean,
                Nsort=TRUE,
                ...) {
    sc<-c(df$s1,df$s2)
    sig<-sd(sc,na.rm=TRUE)
    df$s1<-df$s1/sig
    df$s2<-df$s2/sig
    ##
    tab<-table(df$nm)
    tab<-sort(tab,decreasing=TRUE)
    if (!Nsort) {
        tab<-tab[tab>10]
        tab<-sample(tab)
    } 
    tab<-tab[1:N]
    ##
    nodes<-names(tab)
    titles<-strsplit(nodes,'-')
    titles<-unique(unlist(titles))
    ##
    f<-function(x,eqfun,...) {
        tt<-c(x$t1,x$t2)
        ss<-c(x$s1,x$s2)
        nms<-strsplit(unique(x$nm),'-')[[1]]
        m1<-eqfun(ss[tt==nms[1]],...)
        m2<-eqfun(ss[tt==nms[2]],...)
        m1-m2
    }
    out<-list()
    for (i in 1:length(nodes)) {
        print(rep(i,100))
        x1<-df[df$nm==nodes[i],]
        base.titles<-unique(c(x1$t1,x1$t2))
        for (j in titles) {
            print(j)
            if (!(j %in% base.titles)) {
                nm2<-paste(sort(c(base.titles[1],j)),collapse="-")
                x2<-df[df$nm==nm2,]
                nm3<-paste(sort(c(base.titles[2],j)),collapse="-")
                x3<-df[df$nm==nm3,]
                if (nrow(x2)>0 & nrow(x3)>0) {
                    del.base<-f(x1,eqfun=eqfun,...)
                    del2<-f(x2,eqfun=eqfun,...)
                    del3<-f(x3,eqfun=eqfun,...)
                    sign2<-ifelse(base.titles[1]>j,-1,+1)
                    sign3<-ifelse(base.titles[2]<j,-1,+1)
                    del.triad<-sign2*del2+sign3*del3
                    out[[paste(i,j)]]<-c(nodes[i],j,del.base,del.triad,nrow(x1),(nrow(x2)+nrow(x3)))
                }
            }
        }
    }
    ##
    x<-data.frame(do.call("rbind",out))
    for (i in 2:ncol(x)) x[,i]<-as.numeric(x[,i])
    x
}

source("emp_prep.R")
emp<-df
load("net_sim.Rdata")
sim<-df

##########################################3
##mean
##empirical
x1<-triad(emp,N=25)
##simulated
load("net_sim.Rdata")
x2<-triad(sim,N=25)

par(mgp=c(2,1,0))
plot(x1[,6]/x1[,5],x1[,3]-x1[,4],pch=19,xlab="sample size ratio",ylab="direct minus implied",col='red',cex=.8)
abline(h=0,col='gray')
points(x2[,6]/x2[,5],x2[,3]-x2[,4],pch=19,cex=.4,col='gray')


##mean
##empirical
x1<-triad(emp,N=100,Nsort=FALSE)
##simulated
load("net_sim.Rdata")
x2<-triad(sim,N=100,Nsort=FALSE)

par(mfrow=c(4,1),mgp=c(2,1,0),mar=c(3,3,1,1),oma=rep(.5,4))
qu<-quantile(x1[,5],c(.25,.5,.75))
g1<-cut(x1[,5],c(-Inf,qu,Inf))
g2<-cut(x2[,5],c(-Inf,qu,Inf))
for (i in levels(g1)) {
    tmp<-x1[g1==i,]
    plot(tmp[,6]/tmp[,5],tmp[,3]-tmp[,4],pch=19,xlab="sample size ratio",ylab="direct minus implied",col='red',cex=.8)
    abline(h=0,col='gray')
    tmp<-x2[g2==i,]
    cc<-col2rgb("blue")
    c1<-rgb(cc[1],cc[2],cc[3],max=255,alpha=50)
    points(tmp[,6]/tmp[,5],tmp[,3]-tmp[,4],pch=19,cex=.8,col=c1)
    legend("bottomright",bty='n',legend=i)
}

##########################################3
##quantiles
x1<-x2<-list()
for (qu in c(.1,.25,.75,.9)) {
    x1[[as.character(qu)]]<-triad(emp,N=100,eqfun=quantile,probs=.2)
    x2[[as.character(qu)]]<-triad(sim,N=100,eqfun=quantile,probs=.2)
}

par(mfrow=c(2,2),mgp=c(2,1,0),mar=c(3,3,1,1),oma=rep(.5,4))
for (i in 1:length(x1)) {
    nm<-names(x1)[i]
    plot(x1[[i]][,6]/x1[[i]][,5],x1[[i]][,3]-x1[[i]][,4],pch=19,xlab="sample size ratio",ylab="direct minus implied",col='red',cex=.8)
    points(x2[[i]][,6]/x2[[i]][,5],x2[[i]][,3]-x2[[i]][,4],pch=19,cex=.4,col='gray')
    abline(h=0,col='gray')
    legend("topright",bty='n',paste("quantile",nm))
}
