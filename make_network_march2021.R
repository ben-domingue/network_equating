"literably_full_equating_dataset_latest.csv"->fn
read.csv(fn)->x
unique(x$title)->dict
data.frame(num=1:length(dict),title=dict)->dict
match(x$title,dict$title)->x$title
split(x,x$student_id)->L

f<-function(x) {
    if (nrow(x)>1) {
        nrow(x)->N
        matrix(x$timestamp,nrow=N,ncol=N,byrow=TRUE)->a
        matrix(x$timestamp,nrow=N,ncol=N,byrow=FALSE)->b
        a-b -> del
        matrix(x$score,nrow=N,ncol=N,byrow=TRUE)->sc1
        matrix(x$score,nrow=N,ncol=N,byrow=FALSE)->sc2
        matrix(x$title,nrow=N,ncol=N,byrow=TRUE)->t1
        matrix(x$title,nrow=N,ncol=N,byrow=FALSE)->t2
        ##
        L<-list(del=del,title1=t1,title2=t2,score1=sc1,score2=sc2)
        upper.tri(del,diag=FALSE)->index
        for (i in 1:length(L)) L[[i]][index]->L[[i]]
        data.frame(L)->L
        unique(x$student_id)->L$student_id
        L
    } else NULL
}
lapply(L,f)->L
data.frame(do.call("rbind",L))->df

df$del/(60*60*24)-> del.days
## del.days[del.days<100]->del.days
## hist(del.days,breaks=25,col="blue")
df[del.days<=365,]->df
table(df$title1!=df$title2)
df[df$title1!=df$title2,]->df

df$del<-df$del/(60*60*24)
df[df$del<30,]->df

df$id<-paste(df$student_id,1:nrow(df),sep="_")
df$student_id<-NULL
z1<-df[,c("id","title1","score1")]
z2<-df[,c("id","title2","score2")]
names(z2)<-names(z1)
df<-data.frame(rbind(z1,z2))

save(df,file="/tmp/net_new.Rdata")
