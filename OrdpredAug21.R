########################

Ordpredf2 <- function(form, datlearn,datpred,k,nodesize,maxnodes,mmtry,ntree,serious){ 
  ##### 11/2020
  
  ### derzeit mit Hornung, nodesize,maxnodes,mmtry ignored 
  
  ##mit Korrektur für leere Datensätze
  #predRF<-Ordpredf2(formpropodds,trsel,preddatsel,k,nodesize,maxnodes,mtry,ntree,serious)
  ###serious  >0: nodesize,maxnodes,mmtry übernehmen
  #           <=0: default values
  ##############prediction adj cat datalearn: lerndaten, datpred: prädiktionsdaten, k Anzahl Kategorien)
  ################Cat ist response  automatisch in resp verwandelt 
  #form <- formpropodds
  #datlearn <-trsel
  #datpred <- preddatsel
  ####just to have serious in the function
  #nserious <- serious
  ######
  
  ##
  preddim <-dim(datpred)
  np <- preddim[1]
  
  datdum2 <- as.numeric(datpred$Cat)
  
  trcomp <- matrix(0,nrow=np,ncol=k)
  datl <- datlearn
  
  weight <- matrix(0,k,1)
  nweight <- matrix(0,k,1)
  
  summary(datl)
  summary(dat)
  summary(datlearn)
  
  ####################
  for(l in 2:k) {
    
    dat  <- subset(datl, Cat == l | Cat == l-1)
    xdim<-dim(dat)
    n <- xdim[1]
    weight[l,1]<- n
    
    datdum1 <- as.numeric(dat$Cat)
    
    ###check
    dat1  <- subset(datl,  Cat == l-1)
    ndat1 <- dim (dat1)[1]
    dat2  <- subset(datl,  Cat == l)
    ndat2 <- dim (dat2)[1]
    nind <- min(ndat1,ndat2)
    ###
    
    dat$resp <- 0
    datpred$resp <- 0
    
    for(i in 1:n){if(datdum1[i] >=l) {dat$resp[i] <- 1}}
    for(i in 1:np){if(datdum2[i] >=l) {datpred$resp[i] <- 1}}
    
    dat$resp <- as.factor(dat$resp)
    datpred$resp <- as.factor(datpred$resp)
    
    #
    xt <- gsub(" ","",form[3])
    groupvars <- unlist(strsplit(xt,"\\+"))
    RFform <- as.formula(paste("resp", paste(groupvars, collapse=" + "), sep=" ~ "))
    
    #trial  verzichtbar
    dat$resp<- as.numeric(dat$resp)
    dat$resp <- as.factor(dat$resp)
    datpred$resp<- as.numeric(datpred$resp)
    datpred$resp <- as.factor(datpred$resp)
    ###
    
    
    #randomForest
    #if(serious > 0)output.forest <- randomForest(RFform, data = dat,nodesize=nodesize,maxnodes=maxnodes,mtry=mmtry,ntree=ntree)
    #if(serious <= 0)output.forest <- randomForest(RFform, data = dat,ntree=ntree)
    #conf.pred <- predict(output.forest, datpred, type="prob")
    
    #Hornung
    
    groupvars3 <- c(groupvars, "resp")
    trsel  <- dat[groupvars3] 
    datpredsel <- datpred[groupvars3]
    
    summary(trsel)
    
    if(nind >0){
    ordforr <- ordfor(depvar="resp", data=trsel, nsets=100,  ntreeperdiv=100,ntree=ntree)
    preds <- predict(ordforr, newdata=datpredsel)
    conf.pred <- preds$classprobs
    } 
    
    if(nind <=0){conf.pred <- matrix(0,np,1)
        for(m in 1:np){conf.pred[m,1]<- (ndat1+0.00005)/(ndat1+ndat2+0.0001)
                   conf.pred[m,2]<- (ndat2+0.00005)/(ndat1+ndat2+0.0001)
                   }
    }
    
    
    #Korrektur
    minn<-0.0001
    conf.pred[,2]<- (1-2*minn)*conf.pred[,2]+minn
    #
    #tr <- log(conf.pred[,2]/(1-conf.pred[,2]))
    trcomp[,l]<- log(conf.pred[,2]/(1-conf.pred[,2]))
  }###end l loop
  
  ######rescaling trees
  
  #rowtrcomp <- rowSums(trcomp)
  #rowtrcomp <- matrix(rowtrcomp,np,1)
  #trcompres <- matrix(0,nrow=np,ncol=k)
  #for(j in 1:k) {trcompres[,j]<- trcomp[,j]-rowtrcomp/k}
  #trcomp <- trcompres
  
  
  ####################### 
  ##############weights symmetrisch
  
  #for(l in 1:k){
  #  datweight  <- subset(datl, Cat == l)
  #  nweight[l,1] <- dim(datweight)[1]}
  
  #sumweight <- sum(nweight)
  #for(j in 1:k) {nweight[j,1]<- nweight[j,1]/sumweight}
  
  #########  weights funktioniert nicht
  #exp(trcomp)
  #sumweights <- sum(weight)
  #weight <- weight/sumweights 
  
  #for(l in 2:k) nweight[l,1] <- weight[l,1]-1/(k-1)
  #row <- matrix(rowSums(trcomp)/(k-1),np,1)
  #row  <- abs(row)
  #for(l in 2:k) trcomp[,l] <- trcomp[,l]+row*nweight[l,1]
  
  ########################
  
  sumtr <- matrix(0,nrow=np,ncol=k)
  
  for(j in 2:k) {sumtr[,j]<- sumtr[,j-1]+trcomp[,j]}
  
  totsum1 <-rowSums(exp(sumtr))
  totsumexp <- matrix(totsum1,nrow=np,ncol=1)
  
  prob <- matrix(0,np,k)
  for(j in 1:k){prob[,j] <- exp(sumtr[,j])/totsumexp[,1]}
  
  ##check
  #log(prob[,2]/prob[,1])
  #rowSums(prob)
  
  
  
  
  return(prob)
  
}  




########################

Ordpredf2cforest <- function(form, datlearn,datpred,k,nodesize,maxnodes,mmtry,ntree){
  ##### 9/2020
  
  
  ##############prediction adj cat datalearn: lerndaten, datpred: prädiktionsdaten, k Anzahl Kategorien)
  ################Cat ist response  automatisch in resp verwandelt 
  
  ####### for cforest only
  
  
  preddim <-dim(datpred)
  np <- preddim[1]
  
  trcomp <- matrix(0,nrow=np,ncol=k)
  
  ####################
  for(l in 2:k){
    
    
    dat <- datlearn
    dat  <- subset(dat, Cat == l | Cat == l-1)
    xdim<-dim(dat)
    n <- xdim[1]
    
    dat$Cat <- as.numeric(dat$Cat)
    datpred$resp <- 0
    dat$resp <- 0
    
    for(i in 1:n){if(dat$Cat[i] >=l) {dat$resp[i] <- 1}}
    dat$resp <- as.factor(dat$resp)
    #new
    xt <- gsub(" ","",form[3])
    groupvars <- unlist(strsplit(xt,"\\+"))
    RFform <- as.formula(paste("resp", paste(groupvars, collapse=" + "), sep=" ~ "))
    #
    output.forest <- cforest(RFform, data = dat)
    
    
    mpred <- predict(output.forest, newdata = datpred, type="prob",OOB = FALSE,  scale = TRUE)
    
    conf.pred <- matrix(unlist(mpred), nrow = dim(datpred)[1], byrow = TRUE)
    
    #Korrektur
    minn<-0.002
    conf.pred[,2]<- (1-2*minn)*conf.pred[,2]+minn
    #
    #tr <- log(conf.pred[,2]/(1-conf.pred[,2]))
    trcomp[,l]<- log(conf.pred[,2]/(1-conf.pred[,2]))
  }
  ####################### 
  
  sum <- matrix(0,nrow=np,ncol=k)
  
  
  
  for(i in 1:np){
    for(r in 2:k){sumdum <- matrix(0,nrow=np,ncol=k)
    for(j in 2:r) {sumdum[i,j]<- sumdum[i,j-1]+trcomp[i,j]}
    sum[i,r] <-sumdum[i,r] 
    }
  }
  
  sumtot <-rowSums(exp(sum))
  prob <- matrix(0,nrow=np,ncol=k)
  prob <- exp(sum)/sumtot
  
  return(prob)
}

########################neu für Split variables

OrdpredSplitWeights <- function(form, datlearn,preddat,k,nodesize,maxnodes,mmtry,ntree, gamma){
  ##### 12.11/2020
  
  ######## mit Horung gefittet
  
  ##############prediction adj cat datalearn: lerndaten, datpred: prädiktionsdaten, k Anzahl Kategorien)
  ################Cat ist response  automatisch in resp verwandelt 
  
  dat <- datlearn
  n <- dim(dat)[1]
  
  datpred <- preddat
  np <- dim(datpred)[1]
  
  splitprob <- matrix(0,nrow=np,ncol=k)
  
  
  xt <- gsub(" ","",form[3])
  groupvars <- unlist(strsplit(xt,"\\+"))
  RFform <- as.formula(paste("resp", paste(groupvars, collapse=" + "), sep=" ~ "))
  groupvars2 <- c(groupvars,"resp")
  
  
  summary(dat)
  
  ####################
  
  for(l in 2:k){
    
    datdum <- as.numeric(dat$Cat)
    
    dat$resp <- 0
    for(i in 1:n){if(datdum[i] >=l) {dat$resp[i] <- 1}}
    dat$resp <- as.factor(dat$resp)
    
    datpred$resp <- 0
    datdumn <- as.numeric(datpred$Cat)
    for(i in 1:np){if(datdumn[i] >=l) {datpred$resp[i] <- 1}}
    datpred$resp <- as.factor(datpred$resp)
    
    
    #new
    
    ####
    #randomForest
    
    #output.forest <- randomForest(RFform, data = dat,ntree=ntree)
    #conf.pred <- predict(output.forest, datpred, type="prob")
    
    #Hornung
    
    #trsel  <- dat[groupvars2] 
    #datpredn <- datpred[groupvars2]
    
    #ordforr <- ordfor(depvar="resp", data=trsel, nsets=50,  ntreeperdiv=100,ntree=ntree)
    #preds <- predict(ordforr, newdata=datpredn)
    #conf.pred <- preds$classprobs
    
    #####parametric
    
    dat$resp<- as.ordered(dat$resp)
    datpred$resp<- as.ordered(datpred$resp)
    pomn <- vglm(RFform, family = cumulative(parallel=TRUE), data=dat)
    
    summary(pomn)
    #pred<-matrix(0,di,k)
    conf.pred <- predict(pomn, datpred, type="response")
    
    
    ####
    splitprob[,l]<- conf.pred[,2]
  }
  ####################### end l splits
  
  prob <- matrix(0,nrow=np,ncol=k)
  dumprob <- matrix(0,nrow=np,ncol=k)
  
  ##first split
  #for(i in 1:np){
    dumprob[,1]<-1-splitprob[,2]
    for(r1 in 2:k){dumprob[,r1] <- splitprob[,2]/(k-1)} #(r1-1)^gamma}
  #}
  rowSums(dumprob)
  #prob <- dumprob/rowSums(dumprob)
  prob <- dumprob
  
  
  ##next splits
  for(r in 3:k){ 
    #for(i in 1:np){
      rdum <- r-1
      for(r1 in 1:rdum){dumprob[,r1]<-(1-splitprob[,r])/rdum }   #((r-r1)^gamma))}
      for(r1 in r:k){dumprob[,r1] <- splitprob[,r]/(k-rdum) }       ##((r1-r+1)^gamma)}
    #}
    prob <- prob +dumprob
  }
  prob <- prob/(k-1)
  #rowSums(prob)
  
  return(prob)
}

########################


################################################
Ordpredf2Par <- function(form, datlearn,datpred,k,nodesize,maxnodes,mmtry,ntree){ 
  ##### 9/2020
  
  ###
  ##############prediction mit binary fit
  #cat datalearn: lerndaten, datpred: prädiktionsdaten, k Anzahl Kategorien)
  #################Cat ist response  automatisch in resp verwandelt 
  
  
  
  ##
  preddim <-dim(datpred)
  np <- preddim[1]
  
  trcomp <- matrix(0,nrow=np,ncol=k)
  datl <- datlearn
  
  weight <- matrix(0,k,1)
  nweight <- matrix(0,k,1)
  
  summary(datl)
  
  
  
  ####################
  for(l in 2:k) {
    
    dat  <- subset(datl, Cat == l | Cat == l-1)
    xdim<-dim(dat)
    n <- xdim[1]
    weight[l,1]<- n
    
    dat$Cat <- as.numeric(dat$Cat)
    datpred$Cat <- as.numeric(datpred$Cat)
    
    dat$resp <- 0
    datpred$resp <- 0
    
    for(i in 1:n){if(dat$Cat[i] >=l) {dat$resp[i] <- 1}}
    for(i in 1:np){if(datpred$Cat[i] >=l) {datpred$resp[i] <- 1}}
    
    dat$resp <- as.ordered(dat$resp)
    datpred$resp <- as.ordered(datpred$resp)
    
    # 
    xt <- gsub(" ","",form[3])
    groupvars <- unlist(strsplit(xt,"\\+"))
    RFform <- as.formula(paste("resp", paste(groupvars, collapse=" + "), sep=" ~ "))
    #
    #if(serious > 0)output.forest <- randomForest(RFform, data = dat,nodesize=nodesize,maxnodes=maxnodes,mtry=mmtry,ntree=ntree)
    
    
    
    
    
    pom <- vglm(RFform, family = cumulative (parallel=TRUE), data=dat)
    
    #conf.pred <- predict(output.forest, datpred, type="prob")
    conf.pred <- predict(pom, datpred, type="response")
    
    
    #Korrektur
    minn<-0.0001
    conf.pred[,2]<- (1-2*minn)*conf.pred[,2]+minn
    #
    #tr <- log(conf.pred[,2]/(1-conf.pred[,2]))
    trcomp[,l]<- log(conf.pred[,2]/(1-conf.pred[,2]))
  }###end l loop
  
  
  sumtr <- matrix(0,nrow=np,ncol=k)
  
  for(j in 2:k) {sumtr[,j]<- sumtr[,j-1]+trcomp[,j]}
  
  totsum1 <-rowSums(exp(sumtr))
  totsumexp <- matrix(totsum1,nrow=np,ncol=1)
  
  prob <- matrix(0,np,k)
  for(j in 1:k){prob[,j] <- exp(sumtr[,j])/totsumexp[,1]}
  
  ##check
  #log(prob[,2]/prob[,1])
  #rowSums(prob)
  
  
  
  
  return(prob)
  
}  



#############################################################
###################################

Ordpredf2flex <- function(form, datlearn,datpred,k,nodesize,maxnodes,mmtry,ntree, nset, ntreeperdiv,
                          serious, indicator){ 
  ##### (formpropodds,trsel,preddatsel,k,nodesize,maxnodes,mtry,ntree,nset, ntreeperdiv,  serious,indicator)
  
  ### fits adjacent categories approach
  
  ##mit Korrektur für leere Datensätze
  #predRF<-Ordpredf2(formpropodds,trsel,preddatsel,k,nodesize,maxnodes,mtry,ntree,serious)
  
  ###serious  >0: nodesize,maxnodes,mmtry übernehmen
  #           <=0: default values
  ##############prediction adj cat datalearn: lerndaten, datpred: prädiktionsdaten, k Anzahl Kategorien)
  
  ################Cat ist response  automatisch in resp verwandelt 
  #form <- formpropodds
  #datlearn <-trsel
  #datpred <- preddatsel
  
  ######
  thresh <- 5  ### number needed to fit
  ##
  preddim <-dim(datpred)
  np <- preddim[1]
  
  datdum2 <- as.numeric(datpred$Cat)
  
  trcomp <- matrix(0,nrow=np,ncol=k)
  datl <- datlearn
  
  weight <- matrix(0,k,1)
  nweight <- matrix(0,k,1)
  
  summary(datl)
  summary(datlearn)
  
  ####################
  for(l in 2:k) {
    
    dat  <- subset(datl, Cat == l | Cat == l-1)
    xdim<-dim(dat)
    n <- xdim[1]
    weight[l,1]<- n
    
    datdum1 <- as.numeric(dat$Cat)
    
    ###check
    dat1  <- subset(datl,  Cat == l-1)
    ndat1 <- dim (dat1)[1]
    dat2  <- subset(datl,  Cat == l)
    ndat2 <- dim (dat2)[1]
    nind <- min(ndat1,ndat2)
    ###
    
    dat$resp <- 0
    datpred$resp <- 0
    
    for(i in 1:n){if(datdum1[i] >=l) {dat$resp[i] <- 1}}
    for(i in 1:np){if(datdum2[i] >=l) {datpred$resp[i] <- 1}}
    
    dat$resp <- as.factor(dat$resp)
    datpred$resp <- as.factor(datpred$resp)
    
    #
    xt <- gsub(" ","",form[3])
    groupvars <- unlist(strsplit(xt,"\\+"))
    RFform <- as.formula(paste("resp", paste(groupvars, collapse=" + "), sep=" ~ "))
    
    #trial  verzichtbar
    dat$resp<- as.numeric(dat$resp)
    dat$resp <- as.factor(dat$resp)
    datpred$resp<- as.numeric(datpred$resp)
    datpred$resp <- as.factor(datpred$resp)
    ###
    
    
    #Hornung
    groupvars3 <- c(groupvars, "resp")
    
    trsel  <- dat[groupvars3] 
    datpredsel <- datpred[groupvars3]
    
    summary(trsel)
    
    ##########
    ### !!!!!
    if(nind >thresh){
    ####Hornung
      if(indicator == "H"){
      
        if(serious > 0)  {ordforr <- ordfor(depvar="resp", data=trsel, nsets=nset,  ntreeperdiv=ntreeperdiv,ntreefinal
                                         = ntree)}
        if(serious <= 0)  {ordforr <- ordfor(depvar="resp", data=trsel)}  
      
        preds <- predict(ordforr, newdata=datpredsel)
      conf.pred <- preds$classprobs
    } ####end Hornung
    
      #randomForest
      if(indicator == "RF"){
      if(serious > 0)output.forest <- randomForest(RFform, data = dat,nodesize=nodesize,maxnodes=maxnodes,mtry=mmtry,ntree=ntree)
      if(serious <= 0)output.forest <- randomForest(RFform, data = dat)
      conf.pred <- predict(output.forest, datpred, type="prob")
      }###end randomforest
      
      
      ####conditional
      if(indicator == "CF"){
      RFform <- as.formula(paste("resp", paste(groupvars, collapse=" + "), sep=" ~ "))
      output.forest <- cforest(RFform, data = dat)
      mpred <- predict(output.forest, newdata = datpred, type="prob",OOB = FALSE,  scale = TRUE)
      conf.pred <- matrix(unlist(mpred), nrow = dim(datpred)[1], byrow = TRUE)
      }####end conditional
      
      
      
      } #####end nind 
    
    
    
    #!!!!!  
    if(nind <= thresh){conf.pred <- matrix(0,np,2)
    for(m in 1:np){conf.pred[m,1]<- (ndat1+0.00005)/(ndat1+ndat2+0.0001)
    conf.pred[m,2]<- (ndat2+0.00005)/(ndat1+ndat2+0.0001)
    }
    }
    
    
    
    
    
    ##!!!
    #Korrektur
    minn<-0.001
    if(k > 5)minn<- 0.05
    conf.pred[,2]<- (1-2*minn)*conf.pred[,2]+minn
    #
    #tr <- log(conf.pred[,2]/(1-conf.pred[,2]))
    trcomp[,l]<- log(conf.pred[,2]/(1-conf.pred[,2]))
  }###end l loop
  
  
  ########################
  
  sumtr <- matrix(0,nrow=np,ncol=k)
  
  for(j in 2:k) {sumtr[,j]<- sumtr[,j-1]+trcomp[,j]}
  
  totsum1 <-rowSums(exp(sumtr))
  totsumexp <- matrix(totsum1,nrow=np,ncol=1)
  
  prob <- matrix(0,np,k)
  for(j in 1:k){prob[,j] <- exp(sumtr[,j])/totsumexp[,1]}
  
  ##check
  #log(prob[,2]/prob[,1])
  #rowSums(prob)
  
  
  
  
  return(prob)
  
}  


########################neu für Split variables

OrdpredSplitWeightsNew <- function(form, datlearn,preddat,k,nodesize,maxnodes,mmtry,ntree,nset, ntreeperdiv, 
                                   serious,indicator){
# OrdpredSplitWeightsNew(formpropodds,train,preddat,k,nodesize,maxnodes,mtry,ntree,nset, ntreeperdiv,serious, 0.000)                        serious, 0.000)  
  ##### 12.11/2020
  
  ######## mit kumulativen wkeiten und isotoner regression nur RF, Hornung!
  
  ##############prediction adj cat datalearn: lerndaten, datpred: prädiktionsdaten, k Anzahl Kategorien)
  ################Cat ist response  automatisch in resp verwandelt 
  
  dat <- datlearn
  n <- dim(dat)[1]
  
  datpred <- preddat
  np <- dim(datpred)[1]
  
  cumprob <- matrix(0,nrow=np,ncol=k)
  
  
  xt <- gsub(" ","",form[3])
  groupvars <- unlist(strsplit(xt,"\\+"))
  RFform <- as.formula(paste("resp", paste(groupvars, collapse=" + "), sep=" ~ "))
  groupvars2 <- c(groupvars,"resp")
  
  
  summary(dat)
  
  ####################
  
  for(l in 2:k){
    
    datdum <- as.numeric(dat$Cat)
        dat$resp <- 0
    n1<-0
    for(i in 1:n){if(datdum[i] >=l) {n1<-n1+1
      dat$resp[i] <- 1}}
    dat$resp <- as.factor(dat$resp)
    
    datpred$resp <- 0
    datdumn <- as.numeric(datpred$Cat)
    for(i in 1:np){if(datdumn[i] >=l) {datpred$resp[i] <- 1}}
    datpred$resp <- as.factor(datpred$resp)
    
    ###check !!!!
    n0 <- n-n1
    nind <- min(n1,n0)
    ###
    
    
    #new
    
    ####!!!!!
    #thresh<-0
    
    #####
    #if(nind >=  thresh){
    #randomForest
    if(indicator == "RF"){
    #output.forest <- randomForest(RFform, data = dat,ntree=ntree)
    output.forest <- randomForest(RFform, data = dat)
    conf.pred <- predict(output.forest, datpred, type="prob")
    }
    #Hornung
    if(indicator == "H"){
    trsel  <- dat[groupvars2] 
    datpredn <- datpred[groupvars2]
    
    if(serious > 0){ordforr <- ordfor(depvar="resp", data=trsel, nsets=nset,  ntreeperdiv=ntreeperdiv,
                                      ntreefinal=ntree)}
    if(serious <= 0){ordforr <- ordfor(depvar="resp", data=trsel)}
    preds <- predict(ordforr, newdata=datpredn)
    conf.pred <- preds$classprobs
    }
    #####parametric
    
    #dat$resp<- as.ordered(dat$resp)
    #datpred$resp<- as.ordered(datpred$resp)
    #pomn <- vglm(RFform, family = cumulative(parallel=TRUE), data=dat)
    #conf.pred <- predict(pomn, datpred, type="response")
    
    
    #### now cumulative
    cumprob[,l-1]<- conf.pred[,1]
  #} #end if thresh
  
    } ### end l
  
    
  cumprob[,k]<-1
  
  ###!!!!!
  k1<-k-1
  fac<-1
  incr<- 0
  if(k > 2) incr<-0.05
  #if(k > 5) fac<-0.95
  ###isotonic regr
    for(i in 1:np){yd <- isoreg( cumprob[i,])
    yd$yf
    yr <- t(as.matrix(yd$yf,1,k)) 
    cumprob[i,]<- yr
    #for(i1 in 2:k1)cumprob[i,i1]<-0.5+fac*(cumprob[i,i1]-0.5)
    #for(i1 in 2:k)cumprob[i,i1]<-cumprob[i,i1]+(i1-1)*incr
    for(i1 in 1:k)cumprob[i,i1]<-cumprob[i,i1]+i1*incr
    for(i1 in 1:k)cumprob[i,i1]<-cumprob[i,i1]/cumprob[i,k]
    #cumprob[i,1]<-cumprob[i,1]+(1-fac)*(cumprob[i,2]-cumprob[i,1])
    }
  
  
  ##############################
  ####################### end l splits
  
  prob <- matrix(0,nrow=np,ncol=k)
  
  prob[,1]<- cumprob[,1]
  for(r in 2:k){prob[,r]<- cumprob[,r]- cumprob[,r-1]}
  
  
  #
  return(prob)
  #return(cumprob)
  }

########################



