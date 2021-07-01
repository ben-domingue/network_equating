load("net_new.Rdata")

## library(lme4)
## lmer(score1~(1|id)+(1|title1),df)
## Random effects:
##  Groups   Name        Std.Dev.
##  id       (Intercept) 21.7    
##  title1   (Intercept) 24.5    
##  Residual             14.0    

#merge(df[!duplicated(df$id),],df[duplicated(df$id),],by='id')->z
#cor(z$score1.x,z$score1.y,use='p') #.83

nb<-unique(df$title1)
books<-data.frame(title1=unique(df$title1),book=rnorm(nb,sd=20))
df<-merge(df,books)
df$score1<-NULL
df<-df[,c("id","title1","book")]

df1<-df[!duplicated(df$id),]
names(df1)<-c("id","t1","book1")
df2<-df[duplicated(df$id),]
names(df2)<-c("id","t2","book2")
df<-merge(df1,df2)

df$th<-rnorm(nrow(df),sd=25)
df$s1<-df$th+df$book1+rnorm(nrow(df),sd=14) 
df$s2<-df$th+df$book2+rnorm(nrow(df),sd=14) 

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

save(df,file="net_sim.Rdata")

