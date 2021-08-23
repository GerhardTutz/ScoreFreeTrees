
####################  Fit with Split concept


library("ordinal")
library("catdata")
library("AER")
library("xtable")
library("ordDisp") #Moritz Packet
library("randomForest")
library("VGAM")
library("ordinalForest")
library("party")
library(plyr)
library("MASS")
library("trtf")
library("nloptr")
library("alabama")
library("tram")
library("mlt")
library("lessR")

source("EvalLoopAug21.R")
source("OrdpredAug21.R")



######## Example   birthweight
#############################

data("birthwt",package="MASS")
birthwt$Category <- ifelse(birthwt$bwt <= 2500, 3,
                           ifelse(birthwt$bwt <= 3000, 2,
                                  ifelse(birthwt$bwt <= 3500, 1, 0)))
birthwt$Cat <- ifelse(birthwt$bwt <= 2500, 1,
                      ifelse(birthwt$bwt <= 3000, 2,
                             ifelse(birthwt$bwt <= 3500, 3, 4)))
birthwt$Category<- as.factor(birthwt$Category)
birthwt$Cat <- as.factor(birthwt$Cat )
summary(birthwt)
birthwt$race <- as.factor(birthwt$race )
birthwt$smoke <- as.factor(birthwt$smoke )
birthwt$ht <- as.factor(birthwt$ht )
birthwt$ui <- as.factor(birthwt$ui )
summary(birthwt)


####  specifications:

formpropodds <-Cat ~ age + lwt  + smoke + ptl + ht + ui + ftv+race  ### formula with Cat as response
k <-4   ### number of response categories

serious <-0  ##  0 means default values for trees, 1: means values have to be specified

### selection random forest for binary models
#indicator <- "H"  #### Hornung
indicator <- "RF"  #### randomForest
#indicator <- "CF" #### conditional forest

#### if serious <-1 some parameters have to be specified:

#parameter randomForests
nodesize  <- 5 
mtry  <-  3 ##default sqrt(p)
maxnodes  <- 8

#parameter Hornung
ntree <- 100 ###ntreefinal
nset<- 100
ntreeperdiv <- 50


#### learning and validation data
train <- birthwt         ### choice training set
newdat <-  birthwt  #### new data set to be predicted

## selecting variables specified in formula
xt <- gsub(" ","",formpropodds[3]) 
groupvars <- unlist(strsplit(xt,"\\+"))
groupvars2 <- c(groupvars, "Cat")
train  <- train[groupvars2] 
newdat <- newdat[groupvars2]



## computes predicted probabilities for newdat

predRF<-Ordpredf2flex(formpropodds,train,newdat,k,nodesize,maxnodes,mtry,ntree,nset, ntreeperdiv,
                      serious,indicator)

### plots for observations
obs<- 25  ### choose observation
x<- seq(1,k,1)
plot(x,predRF[obs,], type="b", ylim=c(0,max(predRF[obs,])),cex=2.0,
     main = "Predicted Probabilities and True Category ",cex.axis=1.5,cex.lab=1.6,cex.main=1.6,)
true<-newdat$Cat[obs]
lines(true,0.005,type="p",pch=19,cex=2.0)



