##prep empirical data
load("net_new.Rdata")

df1<-df[!duplicated(df$id),]
names(df1)<-c("id","t1","s1")
df2<-df[duplicated(df$id),]
names(df2)<-c("id","t2","s2")
df<-merge(df1,df2)

titles<-unique(c(df$t1,df$t2))
all(sort(titles)==1:length(titles)) #if this is not true, won't work

df$id<-NULL
df<-df[,c("t1","t2","s1","s2")]

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
