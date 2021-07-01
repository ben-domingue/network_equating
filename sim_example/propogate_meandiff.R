
propogate<-function(df,
                    niter=50000
                    ) {
    ##1. get facts about network
    net<-table(df$nm)
    nms<-strsplit(names(net),"-")
    nms<-do.call("rbind",nms)
    nbooks<-length(unique(c(nms[,1],nms[,2])))
    ##2. prepare for estimation. i want to initialize a vector of text-specific offsets (the objects of interest in this example; i.e. the very simple "maps")
    node<-1 #always start at the first node
    ##initialize by getting mean difference of an edge (for edges that exist and sticking with 0 for those that dont)
    bhat<-rep(0,nbooks)
    bhat.N<-rep(0,nbooks)
    for (i in 2:nbooks) {
        tmp<-df[df$nm==paste("1-",i,sep=''),]
        if (nrow(tmp)>0) {
            tt<-c(tmp$t1,tmp$t2)
            ss<-c(tmp$s1,tmp$s2)
            m1<-mean(ss[tt==1])
            mm<-mean(ss[tt==i])
            bhat[i]<-mm-m1
        }
    }
    ##setting up stuff for iteration
    node.counter<-rep(0,nbooks)
    b.list<-list()
    b.list[[1]]<-bhat
    ncycle<-counter<-1 #counter counts all iterations, #ncycle counts iterations for a given chain
    del<-numeric()
    s<-sd(c(df$s1,df$s2))
    mdel.max<-s/250 #this is the convergence criteria. s/N of the SD of the scores
    mdel<-s*100 
    ##3. iterate
    while (counter<niter & mdel>mdel.max) {
        node.counter[node]<-node.counter[node]+1
        counter<-counter+1
        bhat<-b.list[[counter-1]]
        bhat[1]<-0
        test1<-which(nms[,1]==node)
        test2<-which(nms[,2]==node)
        subnet<-net[c(test1,test2)] #this gets the subnetwork consisting of just those nodes connected to the current focal node. 
        ii<-which(rmultinom(1,1,subnet/sum(subnet))[,1]==1) #now i randomly pick a node in the subnet
        tmp<-df[df$nm==names(subnet)[ii],]
        txt<-strsplit(names(subnet)[ii],"-")[[1]]
        nn<-which(txt!=node)
        next.node<-as.numeric(txt[nn])
        test<-tmp$t1[1]==node
        if (test) { #here i'm going to just get the scores on the edge in a common order
            s1<-tmp$s1
            s2<-tmp$s2
        } else {
            s1<-tmp$s2
            s2<-tmp$s1
        }
        ##
        if (next.node!=1) { #don't update the base node!
            zz<-s2-(s1-bhat[node]) #this identifies a map based on the mean difference between s2 and s1 (after mapping s1 to the base text)
            candidate<-mean(zz) #this is the new offset estimate for the text!! but i do not update with certainty (see update.flag). this was probably silly. 
            update.flag<-rbinom(1,1,length(s2)/(length(s2)+bhat.N[next.node]))
            if (update.flag==1) {
                bhat[next.node]<-candidate
                bhat.N[next.node]<-length(s2)
            }
        } 
        ##
        b.list[[counter]]<-bhat
        del[[counter-1]]<-sqrt(mean((bhat-b.list[[counter-1]])^2))
        if (counter>5000) {
            mdel<-mean(rev(del)[1:1000]) 
        } else mdel<-100
        ##next node
        if (ncycle>4 & counter>50 & runif(1)<.5) {
            #get unsampled nodes
            p<- 1-node.counter/sum(node.counter)
            p<-p/sum(p)
            node<-which(rmultinom(1,1,p)[,1]==1)
            ncycle<-1
        }
        else {
            ncycle<-ncycle+1
            node<-next.node
        }
        print(c(counter,node,mdel/mdel.max))
    }
    est<-do.call("rbind",b.list)
    #list(est=est,node.counter=node.counter)
    b.list
}
