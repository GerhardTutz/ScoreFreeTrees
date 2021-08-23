

##########################################
# Programs for Evaluation of Score-Free Random Forests (including score)



################################################


#######################################################
########################

EvalLoopflex <- function(nloop,k,tr,trainnum,formpropodds,nodesize,maxnodes,mtry,ntree,
                         nset, ntreeperdiv, serious, indicator){

  #Computes scores for single splits of data including RPS, log,..  
  
  ###indicator  "H" Hornung ordinal forest
  
  # serious: >0: takes nodesize,maxnodes,mtry seriously for randomforests and Hornung 
  
  
    
  ### nloop number of data splits
  ### tr train data
  ### formpropodds formula with Cat as response
  ### calls Ordpredf2
  
  #### notation:
  #ohne    pom  
  #RF      random Forest mit adj  
  #RFSimp  simple RF (alt)
  #H      Hornung  
  #ens1   ensemble 1
  #ens2
  #acat    adjacent cat parametric
  #RFsplit concept with RF
  
  
  
  
  RPScoreLoop<- matrix(0,nloop,1)
  RPScoreRFLoop <- matrix(0,nloop,1)       
  RPScoreRFLoopSimp <- matrix(0,nloop,1)
  RPScoreLoopH <- matrix(0,nloop,1)#
  RPScoreLoopens1 <- matrix(0,nloop,1)
  RPScoreLoopens2 <- matrix(0,nloop,1)
  RPScoreLoopens3 <- matrix(0,nloop,1)
  RPScoreLoopacat <- matrix(0,nloop,1)
  RPScoreRFSplitLoop<- matrix(0,nloop,1)
  RPScoreRFBuri<- matrix(0,nloop,1)
  
  Distpom <- matrix(0,nloop,1)
  DistSimpleRF <- matrix(0,nloop,1)
  DistRF <- matrix(0,nloop,1)
  DistordRF <- matrix(0,nloop,1)
  DistH <- matrix(0,nloop,1)
  Distacat<- matrix(0,nloop,1)
  Distens1 <- matrix(0,nloop,1)
  Distens2 <- matrix(0,nloop,1)
  Distens3 <- matrix(0,nloop,1)
  DistRFSplit<- matrix(0,nloop,1)
  DistRFBuri<- matrix(0,nloop,1)
  
  Logpom <- matrix(0,nloop,1)
  LogSimpleRF <- matrix(0,nloop,1)
  LogRF <- matrix(0,nloop,1)
  LogordRF <- matrix(0,nloop,1)
  LogH <- matrix(0,nloop,1)
  Logacat<- matrix(0,nloop,1)
  Logens1 <- matrix(0,nloop,1)
  Logens2 <- matrix(0,nloop,1)
  Logens3 <- matrix(0,nloop,1)
  LogRFSplit<- matrix(0,nloop,1)
  LogRFBuri<- matrix(0,nloop,1)
  
  qpom <- matrix(0,nloop,1)
  qSimpleRF <- matrix(0,nloop,1)
  qRF <- matrix(0,nloop,1)
  qordRF <- matrix(0,nloop,1)
  qH <- matrix(0,nloop,1)
  qacat<- matrix(0,nloop,1)
  qens1 <- matrix(0,nloop,1)
  qens2 <- matrix(0,nloop,1)
  qens3 <- matrix(0,nloop,1)
  qRFSplit<- matrix(0,nloop,1)
  qRFBuri<- matrix(0,nloop,1)
  
  mweightpom <- matrix(0,nloop,1) 
  mweightRF <- matrix(0,nloop,1) 
  mweightord <- matrix(0,nloop,1) 
  mweightacat<- matrix(0,nloop,1)
  mweightRFSplit<- matrix(0,nloop,1)
  
  ##########################
  for(l in 1:nloop){
    
    trainrow=sample(1:nrow(tr),trainnum,replace = FALSE)
    
    ###train are learn data, preddat validation data
    
    train <- tr[trainrow,]
    preddat <- tr[-trainrow,]
    
    dim(preddat)
    ###########computation performance
    
    
    di<-dim(preddat)[1]
    
    dumtrue<-matrix(0,di,k)
    cumtrue <- matrix(0,di,k)
    
    #trueclass<-as.factor(preddat$Cat)
    for(i in 1:di){
      for(j in 1:k){if(preddat$Cat[i] ==j) dumtrue[i,j]=1}
    }
    
    cumtrue[,1]<-dumtrue[,1]
    
    #for(i in 1:di){
    for(j in 2:k){cumtrue[,j]<-cumtrue[,j-1]+dumtrue[,j]}
    #} 
    
    dumvar <- as.numeric(preddat$Cat)
    true <- matrix(dumvar,di,1)
    
    #################################################
    ###################### pred pom
    
    train$Cat<- as.ordered(train$Cat)
    preddat$Cat<- as.ordered(preddat$Cat)
    
    pomn <- vglm(formpropodds, family = cumulative(parallel=TRUE), data=train)
    pred<-matrix(0,di,k)
    pred <- predict(pomn, preddat, type="response")
    
    scorep <- Scores(true,dumtrue,cumtrue, pred,k)
    Distpom[l,1] <- scorep$Dist
    RPScoreLoop[l,1]<- scorep$RPS
    Logpom[l,1] <- scorep$log
    qpom[l,1] <- scorep$qs
    #################################################
    ###################### pred adj
    
    train$Cat<- as.ordered(train$Cat)
    adj <- vglm(formpropodds, family = acat(parallel=TRUE), data=train)
    preda <- predict(adj, preddat, type="response")
    
    scorea <- Scores(true,dumtrue,cumtrue, preda,k)
    Distacat[l,1] <- scorea$Dist
    RPScoreLoopacat[l,1]<- scorea$RPS
    Logacat[l,1] <- scorea$log
    qacat[l,1] <- scorea$qs
    
    #######################################################
    #######pred RF flexible
    xt <- gsub(" ","",formpropodds[3])
    groupvars <- unlist(strsplit(xt,"\\+"))
    
    groupvars2 <- c(groupvars, "Cat")
    trsel  <- train[groupvars2] 
    preddatsel <- preddat[groupvars2]
    
    predRF<-Ordpredf2flex(formpropodds,trsel,preddatsel,k,nodesize,maxnodes,mtry,ntree,nset, ntreeperdiv,
                          serious,indicator)
    #rowSums(predRF)
    #dim(predRFflex)
    #### das wäre die Version mit mit OrdpredSplit
    #predRF<-OrdpredSplitWeights(formpropodds,train,preddat,k,nodesize,maxnodes,mtry,ntree,0.001)
    
    ######alternativ mit cforest
    #predRF<-Ordpredf2cforest(formpropodds,train,preddat,k,nodesize,maxnodes,mtry,ntree)
    
    scoreRF <- Scores(true,dumtrue,cumtrue, predRF,k)
    DistRF[l,1] <- scoreRF$Dist
    RPScoreRFLoop[l,1]<- scoreRF$RPS
    LogRF[l,1] <- scoreRF$log
    qRF[l,1] <- scoreRF$qs
    
    #################################################
    ####  mit OrdpredSplit
    predRFSplit<-OrdpredSplitWeightsNew(formpropodds,train,preddat,k,nodesize,maxnodes,mtry,ntree,nset, ntreeperdiv,
                                        serious, indicator)
    #
    scoreRFSplit <- Scores(true,dumtrue,cumtrue, predRFSplit,k)
    DistRFSplit[l,1] <- scoreRFSplit$Dist
    RPScoreRFSplitLoop[l,1]<- scoreRFSplit$RPS
    LogRFSplit[l,1] <- scoreRFSplit$log
    qRFSplit[l,1] <- scoreRFSplit$qs
     rowSums(predRFSplit)
    
    #############Hornung var selection schon oben
    
    if(serious > 0) {ordforres <- ordfor(depvar="Cat", data=trsel, nsets=nset,  ntreeperdiv=ntreeperdiv, 
                                         ntreefinal = ntree)}
    
    if(serious <= 0) {ordforres <- ordfor(depvar="Cat", data=trsel) }
    
    preds <- predict(ordforres, newdata=preddatsel)
    
    scoreordRF <- Scores(true,dumtrue,cumtrue, preds$classprobs,k)
    
    DistH[l,1] <- scoreordRF$Dist
    RPScoreLoopH[l,1]<- scoreordRF$RPS
    LogH[l,1] <- scoreordRF$log
    qH[l,1] <- scoreordRF$qs
    ####################################
    
    ###### Buri
    logLik(m_theta <- Polr(Cat ~ 1, data = trsel))
    ordBuri <- traforest(m_theta, formula = formpropodds, data = trsel)
    
    prB  <- predict(ordBuri, newdata = preddatsel, cores = 1, type = "density")
    
    prBuri <- matrix(0,di,k)
    
    for(ll in 1:di) prBuri[ll,]<-t(prB[[ll]])
    scoreordBuri <- Scores(true,dumtrue,cumtrue, prBuri,k)
    DistRFBuri[l,1] <- scoreordBuri$Dist
    RPScoreRFBuri[l,1]<- scoreordBuri$RPS
    LogRFBuri[l,1] <- scoreordBuri$log
    qRFBuri[l,1] <- scoreordBuri$qs
    
    
    
    ##########################################
    ####**#*******einfacher randomForest funktioniert aber auskommentiert
    
    #train$Cat<- as.factor(train$Cat)
    
    #if(serious > 0)output.forest <- randomForest(formpropodds, data = train,mtry=mtry,
    #                                             nodesize=nodesize,maxnodes=maxnodes,ntree=ntree)
    #if(serious <= 0)output.forest <- randomForest(formpropodds, data = train,ntree=ntree)
    #
    #predRFSimp <- predict(output.forest, preddat, type="prob")
    #scoreSimpleRF <- Scores(true,dumtrue,cumtrue, predRFSimp,k)
    #DistSimpleRF[l,1] <- scoreSimpleRF$Dist
    #RPScoreRFLoopSimp[l,1] <- scoreSimpleRF$RPS
    ##############################################
    
    
    
    
    #####ensembles
    enssplits <- 3
    nweightRF2 <- 0
    nweightord2 <- 0
    nweightpom2 <- 0
    nweightRF3 <- 0
    nweightord3 <- 0
    nweightpom3 <- 0
    nweightacat3 <- 0
    nweightRFSplit3 <- 0
    
    for(le in 1:enssplits){
    numb <- as.integer(trainnum*0.7)
    #sequ <- c(1:numb)
    seq <- sample( nrow(train),numb, replace = FALSE, prob = NULL)
    trainnew <- train[seq,]
    valnew <- train[-seq,]
    numvalnew <- dim(valnew)[1]
    
    dumvaltrue<-matrix(0,numvalnew,k)
    
    for(i in 1:numvalnew){
      for(j in 1:k){if(valnew$Cat[i] ==j) dumvaltrue[i,j]=1}
    }
    
    ### Brier score pom
    pomnew <- vglm(formpropodds, family = cumulative (parallel=TRUE), data=trainnew)
    predpomval <- predict(pomnew, valnew, type="response")
    Difpom <- sum(rowSums((dumvaltrue-predpomval)^2)/k)/numvalnew
    weightpom <-1-Difpom
    
    ### Brier split
    predRFnew<-Ordpredf2flex(formpropodds,trainnew,valnew,k,nodesize,maxnodes,mtry,ntree,nset, ntreeperdiv,
                             serious,indicator)
    DifRFval <- sum(rowSums((dumvaltrue-predRFnew)^2)/k)/numvalnew
    weightRF <- 1-DifRFval
    
    ### Brier Hornung
    trselnew  <- trainnew[groupvars2]
    trselval  <- valnew[groupvars2]
    ordforresnew <- ordfor(depvar="Cat", data=trselnew, nsets=50, nbest=5, ntreeperdiv=100,ntreefinal=1000)
    predordval <- predict(ordforresnew, newdata=trselval)
    Difordval <- sum(rowSums((dumvaltrue-predordval$classprobs)^2)/k)/numvalnew
    weightord <- 1-Difordval
    
    ###fBrier acat
    acatnew <- vglm(formpropodds, family = acat (parallel=TRUE), data=trainnew)
    predacatval <- predict(acatnew, valnew, type="response")
    Difacat <- sum(rowSums((dumvaltrue-predacatval)^2)/k)/numvalnew
    weightacat <-1-Difacat
    
    ### Brier splits
    predRFSplitnew<-OrdpredSplitWeightsNew(form=formpropodds,datlearn=trainnew,preddat=valnew,k,nodesize,
                                           maxnodes,mtry,ntree,nset, ntreeperdiv,serious,indicator)
    DifRFSplitval <- sum(rowSums((dumvaltrue-predRFSplitnew)^2)/k)/numvalnew
    weightRFSplit <- 1-DifRFSplitval
    
    ## weights pom, split, Hornung
    minw<- min(weightpom,weightRF,weightord)
    maxw<- max(weightpom,weightRF,weightord)
    differenc <- maxw-minw
    
    ## in first paper version
    #weightpom <- weightpom+((weightpom-minw)/differenc)*minw
    #weightRF <- weightRF+((weightRF-minw)/differenc)*minw
    #weightord <- weightord+((weightord-minw)/differenc)*minw
    
    #mweightpom[l,1]<-weightpom/(weightpom+weightRF+weightord)
    #mweightRF[l,1] <- weightRF/(weightpom+weightRF+weightord)
    #mweightord[l,1]<- weightord/(weightpom+weightRF+weightord)
    
    
    
    ###ens1, zwei kombiniert
    #predens1 <- (weightRF*predRF+weightord*preds$classprobs)/(weightRF+weightord) ###adj+Hornung
    
    #scoreens1 <- Scores(true,dumtrue,cumtrue, predens1,k)
    #Distens1[l,1] <- scoreens1$Dist
    #RPScoreLoopens1[l,1] <- scoreens1$RPS
    
    ###ens2  drei kombiniert
    #predens2 <- (weightRF*predRF+weightord*preds$classprobs+weightpom*pred)/(weightRF+weightord+weightpom) ###
    
    ##new
    min<- min(DifRFval,Difordval,Difpom )
    max<- max(DifRFval,Difordval,Difpom )
    nweightRF2 <- nweightRF2+weightensembles(DifRFval, 3, min, max)/enssplits
    nweightord2 <- nweightord2+weightensembles(Difordval, 3, min, max)/enssplits
    nweightpom2 <- nweightpom2+weightensembles(Difpom, 3, min, max)/enssplits
    ####
    
    
    ###weights for five methods
    
    #minw<- min(weightpom,weightRF,weightord,weightacat,weightRFSplit)
    #maxw<- max(weightpom,weightRF,weightord,weightacat,weightRFSplit)
    #differenc <- maxw-minw
    #weightpom <- weightpom+((weightpom-minw)/differenc)*minw
    #weightRF <- weightRF+((weightRF-minw)/differenc)*minw
    #weightord <- weightord+((weightord-minw)/differenc)*minw
    #weightacat<- weightacat+((weightacat-minw)/differenc)*minw
    #weightRFSplit<-weightRFSplit+((weightRFSplit-minw)/differenc)*minw
    
    #mweightpom[l,1]<-weightpom/(weightpom+weightRF+weightord+weightacat+weightRFSplit)
    #mweightRF[l,1] <- weightRF/(weightpom+weightRF+weightord+weightacat+weightRFSplit)
    #mweightord[l,1]<- weightord/(weightpom+weightRF+weightord+weightacat+weightRFSplit)
    #mweightacat[l,1]<-weightacat/(weightpom+weightRF+weightord+weightacat+weightRFSplit)
    #mweightRFSplit[l,1]<-weightRFSplit/(weightpom+weightRF+weightord+weightacat+weightRFSplit)
    
    #predens3 <- (weightRF*predRF+weightord*preds$classprobs+weightpom*pred+weightacat*preda
    #             +weightRFSplit*predRFSplit)/(weightpom+weightRF+weightord+weightacat+weightRFSplit) ###
    
    
    
    min<- min(DifRFval,Difordval,Difpom,Difacat,DifRFSplitval )
    max<- max(DifRFval,Difordval,Difpom,Difacat,DifRFSplitval )
    
    nweightRF3 <- nweightRF3+weightensembles(DifRFval, 5, min, max)/enssplits
    nweightord3 <- nweightord3+weightensembles(Difordval, 5, min, max)/enssplits
    nweightpom3 <- nweightpom3+weightensembles(Difpom, 5, min, max)/enssplits
    nweightacat3 <- nweightacat3+weightensembles(Difacat, 5, min, max)/enssplits
    nweightRFSplit3 <- nweightRFSplit3+weightensembles(DifRFSplitval, 5, min, max)/enssplits
    } ### end enssplit
    
    
    ####
    predens2 <- (nweightRF2*predRF+nweightord2*preds$classprobs+nweightpom2*pred)/(nweightRF2+nweightord2+nweightpom2)
    scoreens2 <- Scores(true,dumtrue,cumtrue, predens2,k)
    Distens2[l,1] <- scoreens2$Dist
    RPScoreLoopens2[l,1] <- scoreens2$RPS
    Logens2[l,1] <- scoreens2$log
    qens2[l,1] <- scoreens2$qs
    ###
    predens3 <- (nweightRF3*predRF+nweightord3*preds$classprobs+nweightpom3*pred+nweightacat3*preda
                 +nweightRFSplit3*predRFSplit)/(nweightpom3+nweightRF3+nweightord3+nweightacat3+nweightRFSplit3) ###
    scoreens3 <- Scores(true,dumtrue,cumtrue, predens3,k)
    Distens3[l,1] <- scoreens3$Dist
    RPScoreLoopens3[l,1] <- scoreens3$RPS
    Logens3[l,1] <- scoreens3$log
    qens3[l,1] <- scoreens3$qs
    ###
    
    
    
    ###### summary computation
    
    #RPScore <- rowSums((cumtrue-cumpred)^2)
    #RPScoreRFSimp <- rowSums((cumtrue-cumpredRFSimp)^2)
    #RPScoreRF <- rowSums((cumtrue-cumpredRF)^2)
    #RPScoreH <- rowSums((cumtrue-cumpredH)^2)
    
    ##absolutbetrag
    #RPScore <- rowSums(abs(cumtrue-cumpred))
    #RPScoreRFSimp <- rowSums(abs(cumtrue-cumpredRFSimp))
    #RPScoreRF <- rowSums(abs(cumtrue-cumpredRF))
    
    
    #RPScoreLoop[l]<- sum(RPScore)/di
    #RPScoreRFLoop[l] <- sum(RPScoreRF)/di
    #RPScoreRFSimpLoop[l] <- sum(RPScoreRFSimp)/di
    #RPScoreLoopH[l] <- sum(RPScoreH)/di
    
    
    ####for ensembles
    
    #RPScoreens1 <- rowSums((cumtrue-cumpredens1)^2)
    #RPScoreLoopens1[l] <- sum(RPScoreens1)/di
    #RPScoreens2 <- rowSums((cumtrue-cumpredens2)^2)
    #RPScoreLoopens2[l] <- sum(RPScoreens2)/di
  }
  
  
  newList <- list("RPSPom" = RPScoreLoop,"Distpom"=Distpom,
                  "RPSRFadj"= RPScoreRFLoop, "DistRF"=DistRF,
                  "RPSH"= RPScoreLoopH, "DistH"=DistH,
                  "RPSens1" =RPScoreLoopens1, "Distens1" =Distens1,
                  "RPSens2" =RPScoreLoopens2, "Distens2" =Distens2,
                  "RPSens3" =RPScoreLoopens3, "Distens3" =Distens3,
                  "weightpom"=mweightpom, "weightRF"=mweightRF,"weightord"=mweightord,
                  "RPSacat" = RPScoreLoopacat, "Distacat"=Distacat, 
                  "RPSSplit"=RPScoreRFSplitLoop, "Distsplit"=DistRFSplit,
                  "RPSSimp"=RPScoreRFLoopSimp, "DistSimpRF"=DistSimpleRF,
                  "Logpom"=Logpom, "LogRF"=LogRF,"Logacat"=Logacat, "LogRFSplit"=LogRFSplit,
                  "LogH"=LogH,"Logens2"=Logens2,"Logens3"=Logens3,
                  "qpom"=qpom, "qRF"=qRF,"qacat"=qacat, "qRFSplit"=qRFSplit,
                  "qH"=qH,"qens2"=qens2,"qens3"=qens3,   
                  "RPSBuri"=RPScoreRFBuri,"DistBuri"=DistRFBuri,"LogBuri"=LogRFBuri,
                  "qBuri"=qRFBuri
                  ) 
  
  return(newList)
}

###############################################
weightensembles <- function(val, f, min, max){
### f is factor
  slope <- min*(1-f)/(max-min)
  weight <- slope*(val-max)+min
return(weight)
}

####################################################
predmedianf <- function(prob,di, k){
  ##probability matrix di x k 
  #output argmin_r (cum prob(r) >= 0.5 )
  cumpred <- matrix(0,di,k)
  
  cumpred[,1]<-prob[ ,1]
  
  #for(i in 1:di){
  for(j in 2:k){cumpred[,j]<-cumpred[,j-1]+prob[,j]}
  #}
  
  predmeddum <- matrix(k,di,k)
  predmedian <- matrix(1,di,1)
  
  for(i in 1:di){
    for(j in 1:k){if (cumpred[i,j] >= 0.5) predmeddum[i,j] <- j}
  } 
  
  for(i in 1:di){predmedian[i,1] <- min(predmeddum[i,])
  } 
  
  return(predmedian)
}





####### Older Version

####################   
EvalLoop <- function(nloop,k,tr,trainnum,formpropodds,nodesize,maxnodes,mtry,ntree,serious){
  #EvalLoop         (nloop,k,tr,trainnum,formpropodds,nodesizenew,maxnodesnew,mtrynew,ntree, serious)
  
  # serious: >0: nimmt nodesize,maxnodes,mtry ernst
  
  # version mit splits enthalten,  
  ### nloop number of data splits
  ### tr train data
  ### formpropodds formula mit Cat as response
  ### calls Ordpredf2
  
  #ohne    pom  
  #RF      random Forest mit adj  
  #RFSimp  simple Rf (alt)
  #H      Hornung  
  #ens1
  #ens2
  #acat    adjecent cat parametric
  #RFsplit concept mit RF
  
  ##dummy
  serious <- serious
  
  RPScoreLoop<- matrix(0,nloop,1)
  RPScoreRFLoop <- matrix(0,nloop,1)       
  RPScoreRFLoopSimp <- matrix(0,nloop,1)
  RPScoreLoopH <- matrix(0,nloop,1)#
  RPScoreLoopens1 <- matrix(0,nloop,1)
  RPScoreLoopens2 <- matrix(0,nloop,1)
  RPScoreLoopacat <- matrix(0,nloop,1)
  RPScoreRFSplitLoop<- matrix(0,nloop,1)
  
  
  Distpom <- matrix(0,nloop,1)
  DistSimpleRF <- matrix(0,nloop,1)
  DistRF <- matrix(0,nloop,1)
  DistordRF <- matrix(0,nloop,1)
  DistH <- matrix(0,nloop,1)
  Distacat<- matrix(0,nloop,1)
  Distens1 <- matrix(0,nloop,1)
  Distens2 <- matrix(0,nloop,1)
  DistRFSplit<- matrix(0,nloop,1)
  
  
  
  mweightpom <- matrix(0,nloop,1) 
  mweightRF <- matrix(0,nloop,1) 
  mweightord <- matrix(0,nloop,1) 
  
  
  
  ##########################
  for(l in 1:nloop){
    
    trainrow=sample(1:nrow(tr),trainnum,replace = FALSE)
    
    ###train are learn data, preddat validation data
    
    train <- tr[trainrow,]
    preddat <- tr[-trainrow,]
    
    
    ###########computation performance
    
    
    di<-dim(preddat)[1]
    
    dumtrue<-matrix(0,di,k)
    cumtrue <- matrix(0,di,k)
    
    #trueclass<-as.factor(preddat$Cat)
    for(i in 1:di){
      for(j in 1:k){if(preddat$Cat[i] ==j) dumtrue[i,j]=1}
    }
    
    cumtrue[,1]<-dumtrue[,1]
    
    #for(i in 1:di){
    for(j in 2:k){cumtrue[,j]<-cumtrue[,j-1]+dumtrue[,j]}
    #} 
    
    dumvar <- as.numeric(preddat$Cat)
    true <- matrix(dumvar,di,1)
    
    #################################################
    ###################### pred pom
    
    train$Cat<- as.ordered(train$Cat)
    preddat$Cat<- as.ordered(preddat$Cat)
    pomn <- vglm(formpropodds, family = cumulative(parallel=TRUE), data=train)
    
    summary(pomn)
    pred<-matrix(0,di,k)
    pred <- predict(pomn, preddat, type="response")
    
    scorep <- Scores(true,dumtrue,cumtrue, pred,k)
    Distpom[l,1] <- scorep$Dist
    RPScoreLoop[l,1]<- scorep$RPS
    
    
    #################################################
    ###################### pred adj
    
    train$Cat<- as.ordered(train$Cat)
    adj <- vglm(formpropodds, family = acat(parallel=TRUE), data=train)
    summary(adj)
    preda <- predict(adj, preddat, type="response")
    
    scorea <- Scores(true,dumtrue,cumtrue, preda,k)
    Distacat[l,1] <- scorea$Dist
    RPScoreLoopacat[l,1]<- scorea$RPS
    
    
    
    #######################################################
    #######pred RF mit adjacent jetzt mit Hornung
    xt <- gsub(" ","",formpropodds[3])
    groupvars <- unlist(strsplit(xt,"\\+"))
    
    groupvars2 <- c(groupvars, "Cat")
    trsel  <- train[groupvars2] 
    preddatsel <- preddat[groupvars2]
    
    predRF<-Ordpredf2(formpropodds,trsel,preddatsel,k,nodesize,maxnodes,mtry,ntree,serious)
    
    
    #### das wäre die Version mit mit OrdpredSplit
    #predRF<-OrdpredSplitWeights(formpropodds,train,preddat,k,nodesize,maxnodes,mtry,ntree,0.001)
    
    ######alternativ mit cforest
    #predRF<-Ordpredf2cforest(formpropodds,train,preddat,k,nodesize,maxnodes,mtry,ntree)
    
    scoreRF <- Scores(true,dumtrue,cumtrue, predRF,k)
    DistRF[l,1] <- scoreRF$Dist
    RPScoreRFLoop[l,1]<- scoreRF$RPS
    
    
    #################################################
    ####  mit OrdpredSplit
    #predRFSplit<-OrdpredSplitWeights(formpropodds,train,preddat,k,nodesize,maxnodes,mtry,ntree,0.000)
    #
    #scoreRFSplit <- Scores(true,dumtrue,cumtrue, predRFSplit,k)
    #DistRFSplit[l,1] <- scoreRFSplit$Dist
    #RPScoreRFSplitLoop[l,1]<- scoreRFSplit$RPS
    
    
    
    
    #############Hornung var selection schon oben
    
    #xt <- gsub(" ","",formpropodds[3])
    #groupvars <- unlist(strsplit(xt,"\\+"))
    #groupvars2 <- c(groupvars, "Cat")
    #trsel  <- train[groupvars2] 
    #preddatsel <- preddat[groupvars2]
    
    #ordforres <- ordfor(depvar="Cat", data=trsel, nsets=1000, ntreeperdiv=100,ntreefinal=5000 )
    #ordforres <- ordfor(depvar="Cat", data=trsel, nsets=50, nbest=5, ntreeperdiv=100,ntree=1000)
    
    ordforres <- ordfor(depvar="Cat", data=trsel, nsets=50,  ntreeperdiv=100,ntree=ntree)
    
    preds <- predict(ordforres, newdata=preddatsel)
    
    scoreordRF <- Scores(true,dumtrue,cumtrue, preds$classprobs,k)
    DistH[l,1] <- scoreordRF$Dist
    RPScoreLoopH[l,1]<- scoreordRF$RPS
    
    ####################################
    
    ##########################################
    ####**#*******einfacher randomForest funktioniert aber auskommentiert
    
    #train$Cat<- as.factor(train$Cat)
    
    #if(serious > 0)output.forest <- randomForest(formpropodds, data = train,mtry=mtry,
    #                                             nodesize=nodesize,maxnodes=maxnodes,ntree=ntree)
    #if(serious <= 0)output.forest <- randomForest(formpropodds, data = train,ntree=ntree)
    #
    #predRFSimp <- predict(output.forest, preddat, type="prob")
    #scoreSimpleRF <- Scores(true,dumtrue,cumtrue, predRFSimp,k)
    #DistSimpleRF[l,1] <- scoreSimpleRF$Dist
    #RPScoreRFLoopSimp[l,1] <- scoreSimpleRF$RPS
    ##############################################
    
    
    
    
    #####ensembles
    numb <- as.integer(trainnum*0.7)
    sequ <- c(1:numb)
    trainnew <- train[sequ,]
    valnew <- train[-sequ,]
    numvalnew <- dim(valnew)[1]
    
    dumvaltrue<-matrix(0,numvalnew,k)
    
    for(i in 1:numvalnew){
      for(j in 1:k){if(valnew$Cat[i] ==j) dumvaltrue[i,j]=1}
    }
    
    pomnew <- vglm(formpropodds, family = cumulative (parallel=TRUE), data=trainnew)
    predpomval <- predict(pomnew, valnew, type="response")
    Difpom <- sum(rowSums((dumvaltrue-predpomval)^2)/k)/numvalnew
    weightpom <-1-Difpom
    
    predRFnew<-Ordpredf2(formpropodds,trainnew,valnew,k,nodesize,maxnodes,mtry,ntree,serious)
    DifRFval <- sum(rowSums((dumvaltrue-predRFnew)^2)/k)/numvalnew
    weightRF <- 1-DifRFval
    
    trselnew  <- trainnew[groupvars2]
    trselval  <- valnew[groupvars2]
    ordforresnew <- ordfor(depvar="Cat", data=trselnew, nsets=50, nbest=5, ntreeperdiv=100,ntreefinal=1000)
    predordval <- predict(ordforresnew, newdata=trselval)
    Difordval <- sum(rowSums((dumvaltrue-predordval$classprobs)^2)/k)/numvalnew
    weightord <- 1-Difordval
    
    #####
    #Difpomval <- sum(rowSums((dumtrue-pred)^2)/k)/di
    #DifRFval <- sum(rowSums((dumtrue-predRF)^2)/k)/di
    #weightpom <-1-sum(Difpomval)/di
    
    
    minw<- min(weightpom,weightRF,weightord)
    maxw<- max(weightpom,weightRF,weightord)
    differenc <- maxw-minw
    weightpom <- weightpom+((weightpom-minw)/differenc)*minw
    weightRF <- weightRF+((weightRF-minw)/differenc)*minw
    weightord <- weightord+((weightord-minw)/differenc)*minw
    
    mweightpom[l,1]<-weightpom/(weightpom+weightRF+weightord)
    mweightRF[l,1] <- weightRF/(weightpom+weightRF+weightord)
    mweightord[l,1]<- weightord/(weightpom+weightRF+weightord)
    
    ###ens1
    predens1 <- (weightRF*predRF+weightord*preds$classprobs)/(weightRF+weightord) ###adj+Hornung
    
    scoreens1 <- Scores(true,dumtrue,cumtrue, predens1,k)
    Distens1[l,1] <- scoreens1$Dist
    RPScoreLoopens1[l,1] <- scoreens1$RPS
    
    ###ens2
    predens2 <- (weightRF*predRF+weightord*preds$classprobs+weightpom*pred)/(weightRF+weightord+weightpom) ###
    
    scoreens2 <- Scores(true,dumtrue,cumtrue, predens2,k)
    Distens2[l,1] <- scoreens2$Dist
    RPScoreLoopens2[l,1] <- scoreens2$RPS
    ###### summary computation
    
    #RPScore <- rowSums((cumtrue-cumpred)^2)
    #RPScoreRFSimp <- rowSums((cumtrue-cumpredRFSimp)^2)
    #RPScoreRF <- rowSums((cumtrue-cumpredRF)^2)
    #RPScoreH <- rowSums((cumtrue-cumpredH)^2)
    
    ##absolutbetrag
    #RPScore <- rowSums(abs(cumtrue-cumpred))
    #RPScoreRFSimp <- rowSums(abs(cumtrue-cumpredRFSimp))
    #RPScoreRF <- rowSums(abs(cumtrue-cumpredRF))
    
    
    #RPScoreLoop[l]<- sum(RPScore)/di
    #RPScoreRFLoop[l] <- sum(RPScoreRF)/di
    #RPScoreRFSimpLoop[l] <- sum(RPScoreRFSimp)/di
    #RPScoreLoopH[l] <- sum(RPScoreH)/di
    
    
    ####for ensembles
    
    #RPScoreens1 <- rowSums((cumtrue-cumpredens1)^2)
    #RPScoreLoopens1[l] <- sum(RPScoreens1)/di
    #RPScoreens2 <- rowSums((cumtrue-cumpredens2)^2)
    #RPScoreLoopens2[l] <- sum(RPScoreens2)/di
  }
  
  
  newList <- list("RPSPom" = RPScoreLoop,"Distpom"=Distpom,
                  "RPSRFadj"= RPScoreRFLoop, "DistRF"=DistRF,
                  "RPSH"= RPScoreLoopH, "DistH"=DistH,
                  "RPSens1" =RPScoreLoopens1, "Distens1" =Distens1,
                  "RPSens2" =RPScoreLoopens2, "Distens2" =Distens2,
                  "weightpom"=mweightpom, "weightRF"=mweightRF,"weightord"=mweightord,
                  "RPSacat" = RPScoreLoopacat, "Distacat"=Distacat, 
                  "RPSSplit"=RPScoreRFSplitLoop, "Distsplit"=DistRFSplit,
                  "RPSSimp"=RPScoreRFLoopSimp, "DistSimpRF"=DistSimpleRF
  ) 
  
  return(newList)
}

##########################################################################
##########################################################################

Scores <- function(true,dumtrue,cumtrue, pred,k){
  #
  ##true vector with categories
  #
  #dumtrue matrix with 01 dummies
  #pred probabilities matrix()
  
  #
  dih <- dim(dumtrue)[1]
  dummscore <-matrix(0,dih,k)
  
  cumtrue[,1]<-dumtrue[ ,1]
  
  for(j in 2:k){cumtrue[,j]<-cumtrue[,j-1]+dumtrue[,j]}
  
  #for(i in 1:dih){
  #  for(j in 2:k){cumtrue[i,j]<-cumtrue[i,j-1]+dumtrue[i,j]}  } 
  
  cumpred <- matrix(0,dih,k)
  cumpred[,1]<-pred[ ,1]
  
  #for(i in 1:dih){
  for(j in 2:k){cumpred[,j]<-cumpred[,j-1]+pred[,j]}
  #}
  
  #true <-as.numeric(preddat$Cat)
  #true <- as.matrix(true)
  
  predmedian <-predmedianf(pred,dih, k)
  
  errateDist <- (abs(predmedian- true))
  Dist <- colSums(errateDist)/dih
  
  RPScore <- sum(rowSums((cumtrue-cumpred)^2))/dih
  
  dummscore <-matrix(0,dih,1)
  for(i in 1:dih){dummscore[i,1]<- -log(pred[i,true[i,1]])
  }
  logscore <- sum(dummscore)/dih
  
  qscore <- sum(rowSums((dumtrue-pred)^2))/dih
  
  ## 0-1 
  dummscore <-matrix(0,dih,k)
  for(i in 1:dih){if (true[i,1] != predmedian[i,1]) dummscore[i,1]<- 1}
  zerone <- sum(dummscore)/dih
  
  newList <- list("Dist" = Dist, "RPS"= RPScore,"log"=logscore, "qs"=qscore, "zerone"=zerone)
  
  return(newList)
  
  
  
  
}




