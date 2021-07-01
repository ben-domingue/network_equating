source("emp_prep.R")


source("propogate2.R")
b.list<-propogate(df,niter=50000)

est<-do.call("rbind",b.list)
##visualize
matplot(est,type='l',lty=1,col='darkgray')
