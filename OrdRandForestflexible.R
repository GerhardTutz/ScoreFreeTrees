
### Comparison of fits ordinal trees  for various data sets 


#### choose one data set and specification (first part) 
#### then compare prediction using EvalLoopflex (starting around line 420) 
#### then plots can be made


# set working directory to source file location 

#getwd() # check 
setwd("C:\\Users\\Gerhard Tutz\\LRZ Sync+Share\\TuTreeRandomForestScaleFree\\ROrdinalForests")
 

install.packages("ordinal")
install.packages("catdata")
install.packages("AER")
install.packages("xtable")
install.packages("VGAM")
install.packages("randomForest")
install.packages("ordinalForest")

# load relevant packages 
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
#source("scores.R")


###Data and specification, tr is training data, k is number of response categories

########################################
##########################heart data
#########################################################
library("ordinalForest")

data(hearth)
names(hearth)[names(hearth)=="Class"] <- "Cat"


table(hearth$Cat)
dim(hearth)
head(hearth)
summary(hearth)

formpropodds <-Cat ~   age+sex+trestbps+chol+fbs+restecg+thalach+exang+oldpeak +chest_pain
tr <- hearth  ###training data
k <-5
trainnum <- 200


###############################
########birthweight
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

####
formpropodds <-Cat ~ age + lwt  + smoke + ptl + ht + ui + ftv+race
#formRF<- resp ~ age + lwt +  smoke +  ptl + ht + ui + ftv
tr <- birthwt
k <-4
trainnum <- 160


############################################
############housing data
#########################################
install.packages("mlbench")


data("BostonHousing2",package="mlbench")
summary(BostonHousing2)
BostonHousing2$Cat <- ifelse(BostonHousing2$cmedv <= 17, 1,
                      ifelse(BostonHousing2$cmedv <= 21, 2,
                             ifelse(BostonHousing2$cmedv <= 25, 3, 4)))
BostonHousing2$Cat<- as.factor(BostonHousing2$Cat)

summary(BostonHousing2)

Housing2Finer <-BostonHousing2
Housing2Finer$Cat <-ifelse(Housing2Finer$cmedv <= 15, 1,
                           ifelse(Housing2Finer$cmedv <= 19, 2,
                                  ifelse(Housing2Finer$cmedv <= 22, 3, 
                                         ifelse(Housing2Finer$cmedv <= 25, 4,
                                               ifelse(Housing2Finer$cmedv <= 32, 5,   
                                              6)))))
Housing2Finer$Cat<- as.factor(Housing2Finer$Cat)

summary(Housing2Finer)                                         


#with additional dummies
Housing2Finer$v1 <-rnorm(dim(Housing2Finer)[1], mean=0, sd=1)
Housing2Finer$v2 <-rnorm(dim(Housing2Finer)[1], mean=0, sd=1)
Housing2Finer$v3 <-rnorm(dim(Housing2Finer)[1], mean=0, sd=1)
Housing2Finer$v4 <-rnorm(dim(Housing2Finer)[1], mean=0, sd=1)
Housing2Finer$v5 <-rnorm(dim(Housing2Finer)[1], mean=0, sd=1)

tr <- Housing2Finer
k <-6
formpropodds <-Cat ~ crim+zn+indus+nox+rm+indus+chas+age+dis+rad+tax+ptratio+b+lstat
formRF<- resp ~ crim+zn+indus+nox+rm+indus+chas+age+dis+rad+tax+ptratio+b+lstat
trainnum <- 400


################################################
##########################CUB data                                         
###################################################
library("CUB")
data(relgoods)
summary(relgoods)


####CUB1 response safety
myvars <- c("Gender", "Safety", "BirthYear", "EducationDegree", "residence","WalkAlone","Family")
newdata <- relgoods[myvars]

newdata$age <- 2014 -newdata$BirthYear

summary(newdata)
newdata <- newdata[newdata$age>=18,]
newdata <- newdata[newdata$age<=80,]
newdata <- newdata[newdata$residence<=4,]
summary(newdata)

newdata <- na.omit(newdata) 
names(newdata)<-c("Gender", "Cat","Birthyear","Education", "Residence", "Walk","Family","Age")
newdata$Residence <- as.factor(newdata$Residence)

relgood <- newdata
summary(relgood)
formpropodds <-Cat ~ Gender+Education+Walk+Family+Age+Residence
dim(relgood)

set.seed(102)
trainrow=sample(1:nrow(relgood),500,replace = FALSE)
tr <- relgood[trainrow,]
k <-10
trainnum <- 400

###full
#tr <- relgood
#k <-10
#trainnum <- 400

#####################
####CUB2 response environment

myvars <- c("Gender", "Safety", "BirthYear", "EducationDegree", "WalkAlone","RelNeighbours", "Environment")
newdata <- relgoods[myvars]

newdata$age <- 2014 -newdata$BirthYear

summary(newdata)
newdata <- newdata[newdata$age>=18,]
newdata <- newdata[newdata$age<=80,]
summary(newdata)

newdata <- na.omit(newdata) 
names(newdata)<-c("Gender", "Safety","Birthyear","Education","Walk","Neighbours","Cat","Age")


set.seed(101)
trainrow=sample(1:nrow(relgood2),600,replace = FALSE)
tr <- relgood2[trainrow,]
relgood2 <- newdata
summary(relgood2)


formpropodds <-Cat ~ Gender+Education+Walk+Neighbours+Age
dim(tr)
 
k <-10
trainnum <- 300






######################################
##### retino, 
###############################################

library(catdata)
retino<- read.table("C:\\Users\\Gerhard Tutz\\LRZ Sync+Share\\TuTreeRandomForests\\R\\retinopathie06.txt",
                    header=T)

summary(retino)

retino$RET <- retino$RET+1
names(retino)[names(retino)=="RET"] <- "Cat"
dim(retino)

########## 
formpropodds <-Cat ~ SM + DIAB + GH + BP
dim(retino)
tr <- retino
k <-3
trainnum <- 500







######################################
#######################  GESIS
###########################################

load("GLES17angst.rda")
names(GLES)[names(GLES)=="NuclearEnergy"] <- "Cat"
#names(GLES)[names(GLES)=="ClimateChange"] <- "Cat"
summary(GLES)
dim(GLES)

set.seed(102)
trainrow=sample(1:nrow(GLES),800,replace = FALSE)
tr <- GLES[trainrow,]
 
k <-7
trainnum <- 300
tr$Cat<- as.ordered(tr$Cat)
summary(tr)
#small
formpropodds <-Cat ~ Age + Gender + EastWest + Abitur
formpropodds <-Cat ~  Age+ Gender + EastWest + Abitur ## 6.7.21
###more variables
formpropodds <-Cat ~ Age + Gender + EastWest + Abitur+ ClimateChange+Terrorism+Globalization #''6.7.21 second
#formRF<- resp ~ Age + Gender + EastWest + Abitur+Unemployment+ ClimateChange+Terrorism+Globalization


pom <- vglm(Cat ~ Age+ Gender + EastWest + Abitur+Unemployment, 
            family = cumulative (parallel=TRUE), data =tr)
summary(pom)



######################################

#######################################
### wine 
#########################################

winequality <- read.csv("http://archive.ics.uci.edu/ml/machine-learning-databases/wine-quality/winequality-white.csv", sep = ";")
summary(winequality)
#winequality$quality<-winequality$quality-2 

winequality$quality <-ifelse(winequality$quality <= 4, 1,
                           ifelse(winequality$quality <= 5, 2,
                                  ifelse(winequality$quality <= 6, 3, 
                                         ifelse(winequality$quality <= 7, 4,
                                                 5))))
winequality$quality<-as.factor(winequality$quality)
summary(winequality)


set.seed(101)
trainrow=sample(1:nrow(winequality),500,replace = FALSE)
tr <- winequality[trainrow,]
summary(tr)
dim(tr)
names(tr)[names(tr)=="quality"] <- "Cat"

k <-5 # used
trainnum <- 160
#trainnum <- 200
tr$Cat<-as.ordered(tr$Cat)
formpropodds <-Cat ~ fixed.acidity + volatile.acidity + citric.acid+ residual.sugar + chlorides+ free.sulfur.dioxide

#full, not used
formpropodds <-Cat ~ fixed.acidity + volatile.acidity + citric.acid+ residual.sugar + chlorides+ 
  free.sulfur.dioxide+total.sulfur.dioxide+ density + pH  +  sulphates +alcohol

pom <- vglm(formpropodds, 
            family = cumulative (parallel=TRUE), data =tr)
summary(pom)


############################################
##### air quality
#######################################

data(airquality)
## remove observations with mising values
airquality <- airquality[ !apply(is.na(airquality), 1,any), ]
myairquality <- airquality
names(myairquality)[names(myairquality) == "Ozone"] <- "resp"
summary(myairquality)
myairquality$Month <-myairquality$Month-4


datatrial <-myairquality
dr <- density(datatrial$resp, bw = "sj")
#dr <- density(datatrial$Temp, bw = "sj")
plot(dr)

quantile(myairquality$resp, probs = seq(0, 1, 0.20), na.rm = FALSE)
quantile(myairquality$resp, probs = seq(0, 1, 0.16), na.rm = FALSE)
myairquality$Cat <-ifelse(myairquality$resp <= 14, 1,
                           ifelse(myairquality$resp <= 23, 2,
                                  ifelse(myairquality$resp <= 39, 3, 
                                         ifelse(myairquality$resp <= 73, 4,
                                                   5)))) 

k <-5

myairquality$Cat <-ifelse(myairquality$resp <= 13, 1,
                          ifelse(myairquality$resp <= 20, 2,
                                 ifelse(myairquality$resp <= 30, 3, 
                                        ifelse(myairquality$resp <= 44, 4,
                                               ifelse(myairquality$resp <= 73, 5,
                                                   6))))) 

k <-6
myairquality$Cat<-as.factor(myairquality$Cat)

myairquality$Month <-as.factor(myairquality$Month )
summary(myairquality)

tr <- myairquality

dim(tr)
trainnum <- 160
formpropodds <-Cat ~ Solar.R  +  Wind  +Temp   +  Month  +  Day


### medical care
library("AER")
data("NMES1988")

datcare<-NMES1988
summary(datcare)
datcare

datcare <- datcare[datcare$gender =="male",] 
datcare <- na.omit(datcarem)

summary(datcare)

dr <- density(datcare$visits, bw = "sj")
plot(dr)

quantile(datcare$visits, probs = seq(0, 1, 0.20), na.rm = FALSE)


datcare$Cat <-ifelse(datcare$visits <= 0, 1,
                          ifelse(datcare$visits <= 1, 2,
                                 ifelse(datcare$visits <= 3, 3, 
                                        ifelse(datcare$visits <= 6, 4,
                                               ifelse(datcare$visits <= 8, 5,   
                                               6))))) 
k <-6
#finer, is used
datcare$Cat <-ifelse(datcare$visits <= 0, 1,
                     ifelse(datcare$visits <= 1, 2,
                            ifelse(datcare$visits <= 3, 3, 
                                   ifelse(datcare$visits <= 6, 4,
                                          ifelse(datcare$visits <= 8, 5,   
                                                 ifelse(datcare$visits <= 11, 6,
                                                  7))))))

k <-7
datcare$Cat<- as.factor(datcare$Cat)
summary(datcare)

set.seed(101)
trainrow=sample(1:nrow(datcare),500,replace = FALSE)
tr <- datcare[trainrow,]


summary(tr)
trainnum <- 300
formpropodds <-Cat ~  emergency   +  hospital+health+chronic+adl+age+afam+married+employed+insurance

tr$Cat<- as.ordered(tr$Cat)
pom <- vglm(Cat ~  emergency   +  hospital+health+chronic+adl+age+afam+married+employed+insurance, 
            family = cumulative (parallel=TRUE), data =tr)
summary(pom)



###########################various fits   now 12.11
 


###  Parameters needed:

serious <-0  ##  0 means default values for trees, 1: means values have to be specified
nloop <- 2   ### number of data splits


### selection random forest for binary models
#indicator <- "H"  #### Hornung
indicator <- "RF"  #### randomForest
#indicator <- "CF" #### conditional forest


#### if serious <-1 some parameters have to be specified:

#parameter randomForests
nodesize  <- 5 #default 1
mtry  <-  3 #dim(tr)[1]         #default sqrt(p)
maxnodes  <- 8

#parameter Hornung
ntree <- 100 ###ntreefinal
nset<- 100
ntreeperdiv <- 50


###Fitting

Triall2 <- EvalLoopflex(nloop,k,tr,trainnum,formpropodds,nodesize ,maxnodes ,mtry ,ntree,
                        nset, ntreeperdiv,serious,indicator)



###################
#save(Triall2, file = "winefull")
#load( file = "relgood")

##mit cv!!!
#cv<-10 ###number of cross validation
#mval  <-5  #Anzahl Gitterpunkte
#Triall2 <- EvalLoopCV(nloop,k,tr,trainnum,formpropodds,ntree,cv,mval) 
##########################


#####plots 

##########################################
# with Buri


###RPS
boxplotmatrix <-data.frame(Triall2$RPSPom, Triall2$RPSacat, Triall2$RPSH,Triall2$RPSBuri, Triall2$RPSSplit,
                           Triall2$RPSRFadj,Triall2$RPSens2,Triall2$RPSens3)

names1 <- c("Pom", "Adj", "RFord","RFT", "RFSp", "RFadj", "Ens3","Ens5")  ##ens 2 meint zwei kombiniert
boxplot(boxplotmatrix,names=names1,cex.axis=1.5,cex.lab=1.5, cex.main=1.6, main=c("RPS"))

### Dist
boxplotmatrix <-data.frame(Triall2$Distpom,Triall2$Distacat, Triall2$DistH,Triall2$DistBuri, Triall2$Distsplit, 
                           Triall2$DistRF,Triall2$Distens2,Triall2$Distens3)
#names1 <- c("Pom", "Acat", "RFordinal", "RFadj" ,"Ens2","Ens3")
#boxplot(boxplotmatrix,names=names1,cex.axis=1.5,cex.lab=1.5) 
boxplot(boxplotmatrix,names=names1,cex.axis=1.5,cex.lab=1.5, cex.main=1.6,main=c("Distance") 
)

### log
Triall2$LogRFSplit<- Inf
boxplotmatrix <-data.frame(Triall2$Logpom,Triall2$Logacat, Triall2$LogH,Triall2$LogBuri, Triall2$LogRFSplit,
                           Triall2$LogRF,Triall2$Logens2,Triall2$Logens3)
boxplot(boxplotmatrix,names=names1,cex.axis=1.5,cex.lab=1.5, cex.main=1.6,main="Log scores")

### quadratic . Brier
boxplotmatrix <-data.frame(Triall2$qpom,Triall2$qacat, Triall2$qH, Triall2$qBuri, Triall2$qRFSplit,
                           Triall2$qRF,Triall2$qens2,Triall2$qens3)

boxplot(boxplotmatrix,names=names1,cex.axis=1.5,cex.lab=1.5, cex.main=1.6,main="Quadratic scores")
       

###########################################




#######################
# plots without Buri

###RPS
boxplotmatrix <-data.frame(Triall2$RPSPom, Triall2$RPSacat, Triall2$RPSH, Triall2$RPSSplit,
                           Triall2$RPSRFadj,Triall2$RPSens2,Triall2$RPSens3)
boxplot(boxplotmatrix)
names1 <- c("Pom", "Adj", "RFord", "RFSplit", "RFadj", "Ens3","Ens5")  ##ens 2 meint zwei kombiniert
#boxplot(boxplotmatrix,names=names1,cex.axis=1.5,cex.lab=1.5) 
boxplot(boxplotmatrix,names=names1,cex.axis=1.5,cex.lab=1.5, cex.main=1.5, main="RPS: Birthweight (n_L = 160)") 

### Dist
boxplotmatrix <-data.frame(Triall2$Distpom,Triall2$Distacat, Triall2$DistH, Triall2$Distsplit,
                           Triall2$DistRF,Triall2$Distens2,Triall2$Distens3)
boxplot(boxplotmatrix)
#names1 <- c("Pom", "Acat", "RFordinal", "RFadj" ,"Ens2","Ens3")
#boxplot(boxplotmatrix,names=names1,cex.axis=1.5,cex.lab=1.5) 
boxplot(boxplotmatrix,names=names1,cex.axis=1.5,cex.lab=1.5, cex.main=1.5,main="Distance: Birthweight (n_L = 160)")

### log
 
boxplotmatrix <-data.frame(Triall2$Logpom,Triall2$Logacat, Triall2$LogH, 
                           Triall2$LogRF,Triall2$Logens2,Triall2$Logens3)
boxplot(boxplotmatrix)
#names1 <- c("Pom", "Acat", "RFordinal", "RFadj" ,"Ens2","Ens3")
#boxplot(boxplotmatrix,names=names1,cex.axis=1.5,cex.lab=1.5) 
boxplot(boxplotmatrix,names=names1,cex.axis=1.5,cex.lab=1.5, cex.main=1.5,main="Log score: Birthweight (n_L = 160)")

### quadratic . Brier
boxplotmatrix <-data.frame(Triall2$qpom,Triall2$qacat, Triall2$qH, Triall2$qRFSplit,
                           Triall2$qRF,Triall2$qens2,Triall2$qens3)
boxplot(boxplotmatrix)
#names1 <- c("Pom", "Acat", "RFordinal", "RFadj" ,"Ens2","Ens3")
#boxplot(boxplotmatrix,names=names1,cex.axis=1.5,cex.lab=1.5) 
boxplot(boxplotmatrix,names=names1,cex.axis=1.5,cex.lab=1.5, cex.main=1.5,main="Quadratic Scores: Birthweight (n_L = 160)")

####################################

########################################################






####### Further evaluations, very specific:



########comparison adj methods
nloop <- 20
TriallH <- EvalLoopflex(nloop,k,tr,trainnum,formpropodds,nodesizenew,maxnodesnew,mtrynew,ntree,serious,"H")
TriallRF <- EvalLoopflex(nloop,k,tr,trainnum,formpropodds,nodesizenew,maxnodesnew,mtrynew,ntree,serious,"RF")
TriallCF <- EvalLoopflex(nloop,k,tr,trainnum,formpropodds,nodesizenew,maxnodesnew,mtrynew,ntree,serious,"CF")
#

boxplotmatrix <-data.frame(TriallH$RPSH, TriallRF$RPSH, TriallCF$RPSH)
boxplot(boxplotmatrix)
names3 <- c("OrdRF", "RF", "CRF")  ##ens 2 meint zwei kombiniert
boxplot(boxplotmatrix,names=names3,cex.axis=1.5,cex.lab=1.5) 


#######Importance retino
datlearn <- retino
summary(retino)
formpropodds <-Cat ~ SM + DIAB + GH + BP
form <- resp ~ SM + DIAB + GH + BP
tr <- retino
k <-3
np <- 4

GiniImp <- matrix(0,nrow=np,ncol=k-1)
for(l in 2:k){
  
  
  dat <- datlearn
  dat <- subset(dat, Cat == l | Cat == l-1)
  xdim<-dim(dat)
  n <- xdim[1]
  
  dat$Cat <- as.numeric(dat$Cat)
  datpred$resp <- 0
  dat$resp <- 0
  
  for(i in 1:n){if(dat$Cat[i] >=l) {dat$resp[i] <- 1}}
  dat$resp <- as.factor(dat$resp)
  #summary(dat)
  
  output.forest2 <- randomForest(form, data = dat)#,mtry=mtry)
  GiniImp[,l-1] <- output.forest2$importance
}


#
label.size <-1.3
label.sizehead <- 1.5
#ylims <- range(c(0,dis))
x <- 1:np
matplot(x,GiniImp,pch=c(16,1), type ="b",lty = 1:np, lwd = 2,cex=2)


matplot(x,plottotal,pch=c(16,1), type ="b",lty = 1:np, lwd = 2,cex=2)

GiniSum <- rowSums(GiniImp)
GiniAverage <- GiniSum/(k-1)

matplot(x,GiniAverage,pch=c(16,1), type ="b",lty = 1:np, lwd = 2,cex=2)

matplot(x,cbind(GiniImp,GiniAverage),pch=c(16,1), type ="b",lty = 1:13, lwd = 2,cex=2)
#############################

#######Importance heart
library("ordinalForest")

data(hearth)
names(hearth)[names(hearth)=="Class"] <- "Cat"
dim(hearth)
summary(hearth)

form <-resp ~   chest_pain+oldpeak+age+trestbps+chol+thalach+exang+sex+fbs+restecg
tr <- hearth
k <-5

datlearn <-hearth


np <- 10 #number variables


###adj concept
GiniImp <- matrix(0,nrow=np,ncol=k-1)
GiniImpcf <- matrix(0,nrow=np,ncol=k-1)
for(l in 2:k){
  
  
  dat <- datlearn
  dat <- subset(dat, Cat == l | Cat == l-1)
  xdim<-dim(dat)
  n <- xdim[1]
  
  dat$Cat <- as.numeric(dat$Cat)
  datpred$resp <- 0
  dat$resp <- 0
  
  for(i in 1:n){if(dat$Cat[i] >=l) {dat$resp[i] <- 1}}
  dat$resp <- as.factor(dat$resp)
  #summary(dat)
  
  output.forest2 <- randomForest(form, data = dat)#,mtry=mtry)
  GiniImp[,l-1] <- output.forest2$importance

  output.forestcf <- cforest(form, data = dat)
  
  #v <- varimp(output.forestcf, nperm = 1L,  OOB = TRUE, conditional = TRUE, threshold = .2)
  
  v <- varimp(output.forestcf,   OOB = TRUE, conditional = TRUE, mincriterion = .95)
  GiniImpcf[,l-1] <- v
  #varimp(output.forestcf, nperm = 1L, OOB = TRUE, risk = c("loglik", "misclassification"),
  #       conditional = TRUE, threshold = .2, applyfun = NULL)
  
  
  }

#### split concept
GiniImp <- matrix(0,nrow=np,ncol=k-1)
GiniImpcf <- matrix(0,nrow=np,ncol=k-1)
for(l in 2:k){
  
  
  dat <- datlearn
  xdim<-dim(dat)
  n <- xdim[1]
  
  dat$Cat <- as.numeric(dat$Cat)
  datpred$resp <- 0
  dat$resp <- 0
  
  for(i in 1:n){if(dat$Cat[i] >=l) {dat$resp[i] <- 1}}
  dat$resp <- as.factor(dat$resp)
  #summary(dat)
  
  output.forest2 <- randomForest(form, data = dat)#,mtry=mtry)
  GiniImp[,l-1] <- output.forest2$importance

  output.forestcf <- cforest(form, data = dat)
  v <- varimp(output.forestcf,   OOB = TRUE, conditional = TRUE, mincriterion = .95)
  GiniImpcf[,l-1] <- v
  }



####einfach GiniImp <- GiniImpcf

GiniSum <- rowSums(GiniImp)
GiniAverage <- GiniSum/(k-1)


#plots
label.size <-1.3
label.sizehead <- 1.5
#ylims <- range(c(0,dis))
x <- 1:np
matplot(x,GiniImp,pch=c(16,1), type ="b",lty = 1:np, lwd = 2,cex=2,
        main = " ",cex.axis=1.5,cex.lab=1.6,cex.main=1.6, xlab="Variables", ylab="")

matplot(x,GiniImp, type ="b",lty = 1:np, lwd = 2,cex=2,
        main = "Heart data ",cex.axis=1.5,cex.lab=1.6,cex.main=1.6, xlab="Variables", ylab="")

GiniSum <- rowSums(GiniImp)
GiniAverage <- GiniSum/(k-1)

lines(x,GiniAverage,lwd = 3.5,cex=2)


####plot averages
### zwei läufe notwendig!
Giniverageadj <- GiniAverage
Giniveragesplit<- GiniAverage


matplot(x,cbind(Giniverageadj,Giniveragesplit),pch=c(16,1), type ="b",lty = 1:np, lwd = 2,cex=2,
        main = "Heart data ",cex.axis=1.5,cex.lab=1.6,cex.main=1.6, xlab="Variables", ylab="")#,ylim=c(0,30))




formCat <-Cat ~   chest_pain+oldpeak+age+trestbps+chol+thalach+exang+sex+fbs+restecg

output.forestfull <- randomForest(formCat, data = datlearn)#,mtry=mtry)
Ginifull <- output.forestfull$importance
lines(x,Ginifull,lwd = 2,cex=2, type ="b")
###

matplot(x,GiniAverage,pch=c(16,1), type ="b",lty = 1:np, lwd = 2,cex=2)

matplot(x,cbind(GiniImp,GiniAverage),pch=c(16,1), type ="b",lty = 1:13, lwd = 2,cex=2)


#############################parametric
summary(datlearn)
datlearn$Cat<- as.ordered(datlearn$Cat)
formCat <- Cat ~ chest_pain + oldpeak + age + trestbps + chol + thalach + exang + sex + fbs + restecg
pomcum <- vglm(formCat, family = cumulative(parallel=TRUE), data=datlearn)
summary(pomcum)


##########################




###loop mit train als trainingsmatrix aus daten tr

set.seed(101)
 nodesize <- 8 ##8 ok housing
 maxnodes <- 15  ### 15 housing
 mtry <- 3 #sqt[varnumber]4 gut
 ntree <- 1000
 nloop <- 10
 trainnum <- 150  #150 350gut  #bei extended gesis 500 gut
 
 tr$Cat <- as.factor(tr$Cat)
 summary(tr$Cat)
 
#EvalLoop <- function(nloop,tr,trainnum,formpropodds,nodesize,maxnodes,mtry,ntree)
  
Triall2 <- EvalLoop(nloop,k,tr,trainnum,formpropodds,nodesize,maxnodes,mtry,ntree)

boxplotmatrix <-data.frame(Triall2$RPSPom, Triall2$RPSSimp,Triall2$RPSRFadj,Triall2$RPSH)
boxplot(boxplotmatrix,cex.axis=2.5,cex.lab=2.5)
names <- c("RPSPom", "RPSSimp", "RPSRFadj",  "RPordinal" )
boxplot(boxplotmatrix,names=names,cex.axis=1.5,cex.lab=1.5)

boxplotmatrix <-data.frame(Triall2$Distpom, Triall2$DistSimpRF,Triall2$DistRF,Triall2$DistordRF)
 boxplot(boxplotmatrix)
 names <- c("Distpom", "DistSimpleRF", "DistRF",  "DistordRF" )
 boxplot(boxplotmatrix,names=names,cex.axis=1.5,cex.lab=1.5) 

 
#####Optimization 
 cv <- 5
 m1 <-3
 m2 <-3
 m3 <-4
 #array <- array(0, dim=c(m1,m2,m3)) 
 m<- m1*m2*m3
 arrayvalues <- matrix(0, m,3)
 arrayloop <- matrix(0, m,3)
 values <- matrix(0,m,1)
 
 start<-0
 for(i1 in 1:m1){ 
   nodesize <- 6+3*(i1-1)
   for(i2 in 1:m2){
     maxnodes <- 5+3*(i2-1)
     for(i3 in 1:m3){
       mtry <- 2+(i3-1)
       Triall2 <-EvalCV(cv,k,tr,formpropodds,nodesize,maxnodes,mtry,ntree)
       ##vorher ok
       #Triall2 <- EvalLoop(nloop,k,tr,trainnum,formpropodds,nodesize,maxnodes,mtry,ntree)
   #array[i1,i2,i3]<- mean(Triall2$RPSRFadj)
      start <- start+1
   values[start,1] <- mean(Triall2$RPSRFadj)
   arrayvalues[start,1]<-nodesize
   arrayvalues[start,2]<-maxnodes
   arrayvalues[start,3]<-mtry 
   arrayloop[start,1]<-i1
   arrayloop[start,2]<-i2
   arrayloop[start,3]<-i3 
   
   }}}
 
 #array
 values
 arrayloop
 arrayvalues

 min <- min(array)
 min2 <- min(values)
 
 ###search minimum
 start<-0
 for(i1 in 1:m1){ 
   nodesize <- 6+3*(i1-1)
   for(i2 in 1:m2){
     maxnodes <- 5+3*(i2-1)
     for(i3 in 1:m3){
       mtry <- 2+(i3-1)
       start <- start+1       
       if(values[start,1] <= min2){nodesizenew <-nodesize
       maxnodesnew <- maxnodes
       mtrynew <- mtry
       }
          }}}
 values
 nodesizenew 
 maxnodesnew
 mtrynew  

######end minimum search 
 

  
    
##############################
 ###mit Loop2 identische Ergebnisse
set.seed(101)
Triall2 <- EvalLoop2(loop,tr,trainnum,formpropodds,nodesize,maxnodes,mtry)
boxplotmatrix <-(Triall2$RPS)
names <- c("RPSpropodds","RPSsimpleRF","RPSRF")
boxplot(boxplotmatrix,names=names)  


set.seed(101)
nodesize <- 5
maxnodes <- 8
mtry <-3

 

Triall3 <- EvalLoop2Distsq(loop,tr,trainnum,formpropodds,nodesize,maxnodes,mtry)
boxplotmatrix <-(Triall3)
names <- c("Distpropodds","DistsimpleRF","DistRF")
boxplot(boxplotmatrix,names=names)  

Triall4 <- EvalLoop4(loop,tr,trainnum,formpropodds,nodesize,maxnodes,mtry)


###test
x <- gsub(" ","",formpropodds[3])
xname <- unlist(strsplit(x,"\\+"))
y <- gsub(" ","",formpropodds[2])


train$chas <- as.numeric(train$chas)
train$Cat <- as.numeric(train$Cat)
yl <- train$Cat
xl <-  train[xname]

dim(xl)
dim(train) 

summary(yl)
k    <- length(unique(train$Cat))

mns <- 150              # minimal node size       
link <- "cumulative"    # or "acat"
mtry <- 4
mod <- OrdinalTree(train$chas, xl, mns, link, mtry)

summary(mod$model)


set.seed(140720)
forest1 <- OrdinalForest(yl,
                         Xl,
                         mns=50,                # minimal node size
                         link="cumulative",     # or "acat"
                         ntrees=10)   




########loop  inzwischen ausgelagert als function
set.seed(201)
for(l in 1:nloop){

trainrow=sample(1:nrow(tr),trainnum,replace = FALSE)

###train are learn data, preddat validation data

train <- tr[trainrow,]
#preddat <- tr[trainrow,]
preddat <- tr[-trainrow,]



train$Cat<- as.ordered(train$Cat)
pom <- vglm(formpropodds, family = cumulative (parallel=TRUE), data=train)

###########computation performance

summary(tr)
summary(pom)
summary(train)
summary(pred)

di<-dim(preddat)[1]
dumtrue<-matrix(0,di,k)


#trueclass<-as.factor(preddat$Cat)
for(i in 1:di){
  for(j in 1:k){if(preddat$Cat[i] ==j) dumtrue[i,j]=1}
}

cumtrue <- matrix(0,di,k)
cumtrue[,1]<-dumtrue[ ,1]

for(i in 1:di){
  for(j in 2:k){cumtrue[i,j]<-cumtrue[i,j-1]+dumtrue[i,j]}
} 

#####pred pom
pred <- predict(pom, preddat, type="response")


cumpred <- matrix(0,di,k)
cumpred[,1]<-pred[ ,1]

for(i in 1:di){
   for(j in 2:k){cumpred[i,j]<-cumpred[i,j-1]+pred[i,j]}
}



#######pred RF mit adjacent
predRF<-Ordpredf(formRF,train,preddat,k,mtry)

####**#*******einfacher randomForest 

output.forest <- randomForest(formpropodds, data = train,mtry=mtry)
# output.forest <- randomForest(formpropodds, data = train,mtry=mtry,nodesize=nodesize,maxnodes=maxnodes)
####pred Rf direct
predRFSimp <- predict(output.forest, preddat, type="prob")

cumpredRF <- matrix(0,di,k)
cumpredRF[,1]<-predRF[ ,1]

for(i in 1:di){
  for(j in 2:k){cumpredRF[i,j]<-cumpredRF[i,j-1]+predRF[i,j]}
}

cumpredRFSimp <- matrix(0,di,k)
cumpredRFSimp[,1]<-predRFSimp[ ,1]

for(i in 1:di){
  for(j in 2:k){cumpredRFSimp[i,j]<-cumpredRFSimp[i,j-1]+predRFSimp[i,j]}
}

RPScore <- rowSums((cumtrue-cumpred)^2)
RPScoreRFSimp <- rowSums((cumtrue-cumpredRFSimp)^2)
RPScoreRF <- rowSums((cumtrue-cumpredRF)^2)

##absolutbetrag
#RPScore <- rowSums(abs(cumtrue-cumpred))
#RPScoreRFSimp <- rowSums(abs(cumtrue-cumpredRFSimp))
#RPScoreRF <- rowSums(abs(cumtrue-cumpredRF))



RPScoreLoop[l]<- sum(RPScore)/di
RPScoreRFLoop[l] <- sum(RPScoreRF)/di
RPScoreRFSimpLoop[l] <- sum(RPScoreRFSimp)/di

}


#####end loop


boxplotmatrix <-data.frame(RPScoreLoop, RPScoreRFSimpLoop,RPScoreRFLoop)
boxplot(boxplotmatrix)

names <- c("RPSpropodds","RPSsimpleRF","RPSRF")

#boxplot(RPScore, RPScoreRFSimp,RPScoreRF)
boxplot(boxplotmatrix,names=names)


#plot(output.forest)


### 







####################modepred







###propodds
modeprop <- matrix(1,di,1)
modprel <- pred[,1]
for(i in 1:di){
  for(j in 2:k){if(pred[i,j] >= modprel[i]) {modeprop[i]=j 
  modprel[i]<-pred[i,j]}}
}
trueclass<-preddat$Cat
trueclass<-as.numeric(trueclass)
erratePM <- as.numeric(abs(modeprop- trueclass)>0)
errateDist <- as.numeric(abs(modeprop- trueclass))

 

####RF
modeRF <- matrix(1,di,1)
modprelRF <- predRF[,1]
for(i in 1:di){
  for(j in 2:k){if(predRF[i,j] >= modprelRF[i]) {modeRF[i]=j 
  modprelRF[i]<-predRF[i,j]}}
}
#trueclass<-preddat$Cat
#trueclass<-as.numeric(trueclass)

erratemodeRF <- as.numeric(abs(modeRF- trueclass)>0)
erratemodeRFDist <- as.numeric(abs(modeRF- trueclass))

####RFSimp
modeRFSimp <- matrix(1,di,1)
modprelRFSimp <- predRFSimp[,1]
for(i in 1:di){
  for(j in 2:k){if(predRFSimp[i,j] >= modprelRFSimp[i]) {modeRFSimp[i]=j 
  modprelRFSimp[i]<-predRFSimp[i,j]}}
}
#trueclass<-preddat$Cat
#trueclass<-as.numeric(trueclass)

erratemodeRFSimp <- as.numeric(abs(modeRF- trueclass)>0)
erratemodeRFDistSimp <- as.numeric(abs(modeRF- trueclass))

#boxplot(errate, erratemodeRF)
errsum <- sum(erratePM)/di
errsumRF <- sum(erratemodeRF)/di
errateDist <- sum(errateDist)
erratemodeRFDist <- sum(erratemodeRFDist)
errsum
errsumRF
errateDist
erratemodeRFDist
################################################### 

####Alt: Versuche mit trees adjacent etc

###########Retinopathy
library(catdata)
data(retinopathy)
attach(retinopathy)

summary(retinopathy)

library(VGAM)
RET <- as.ordered(RET)

SM <- as.factor(SM)
pom <- vglm(RET ~ SM + DIAB + GH + BP, family = cumulative (parallel=TRUE))

summary(pom)

#####
library(rpart)
treeret <- rpart(formula = RET ~ SM + DIAB + GH + BP, data = retinopathy, method = "class")

summary(treeret)
plot(treeret)
text(treeret, use.n = TRUE)


###########splits
retinopathy$RET <- as.factor(RET)
ret12 <- retinopathy
xdim<-dim(ret12)
n <- xdim[1]

ret12$resp <- 0
for(i in 1:n){if(ret12$RET[i] >=1) {ret12$resp[i] <- 1}}
ret12$resp <- as.factor(ret12$resp)
summary(ret12)

treeret <- rpart(formula = resp ~ SM + DIAB + GH + BP, data = ret12, method = "class",
                 control = rpart.control(cp = 0.05))
summary(treeret)
plot(treeret)
text(treeret, use.n = TRUE)


ret23 <- retinopathy
xdim<-dim(ret23)
n <- xdim[1]

ret23$resp <- 0
for(i in 1:n){if(ret23$RET[i] >=2) {ret23$resp[i] <- 1}}
ret23$resp <- as.factor(ret23$resp)
summary(ret23)

treeret <- rpart(formula = resp ~ SM + DIAB + GH + BP, data = ret23, method = "class",
                 control = rpart.control(cp = 0.05))
summary(treeret)
plot(treeret)
text(treeret, use.n = TRUE)


###########adjacent
retinopathy$RET <- as.factor(RET)
ret12 <- retinopathy
ret12 <- subset(ret12, RET <= 1 )

xdim<-dim(ret12)
n <- xdim[1]

ret12$resp <- 0
for(i in 1:n){if(ret12$RET[i] >=1) {ret12$resp[i] <- 1}}
ret12$resp <- as.factor(ret12$resp)
summary(ret12)

treeret <- rpart(formula = resp ~ SM + DIAB + GH + BP, data = ret12, method = "class",
                 control = rpart.control(cp = 0.05))
summary(treeret)
plot(treeret)
text(treeret, use.n = TRUE)


ret23 <- retinopathy
ret23 <- subset(ret23, RET >= 1 )

xdim<-dim(ret23)
n <- xdim[1]

ret23$resp <- 0
for(i in 1:n){if(ret23$RET[i] >=2) {ret23$resp[i] <- 1}}
ret23$resp <- as.factor(ret23$resp)
summary(ret23)

treeret <- rpart(formula = resp ~ SM + DIAB + GH + BP, data = ret23, method = "class",
                 control = rpart.control(cp = 0.05))
summary(treeret)
plot(treeret)
text(treeret, use.n = TRUE)


#############birthweight
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
birthwt$smoke <- as.factor(birthwt$smoke )
birthwt$ht <- as.factor(birthwt$ht )
birthwt$ui <- as.factor(birthwt$ui )
summary(birthwt)


treeret <- rpart(formula = Cat ~ age+smoke, data = birthwt, method = "class",
                 control = rpart.control(cp = 0.03))
summary(treeret)
plot(treeret)
text(treeret, use.n = TRUE)


##########plots splits Cat is response variable
########### nur kategorien l, l-1

dat <- birthwt

k <- 4

#for(l in 2:k)
#ret12 <- subset(ret12, RET <= 1 )
l <-4
  
    dat <- birthwt
    dat <- subset(dat, Cat == l | Cat == l-1)
    summary(dat)
    xdim<-dim(dat)
    n <- xdim[1]
    
    dat$Cat <- as.numeric(dat$Cat)
    dat$resp <- 0

  for(i in 1:n){if(dat$Cat[i] >=l) {dat$resp[i] <- 1}}
  dat$resp <- as.factor(dat$resp)
 summary(dat)

treeret <- rpart(formula = resp ~ age+smoke+lwt+ht+ui+ftv, data = dat, method = "class",
                 control = rpart.control(cp = 0.03))

summary(treeret)
plot(treeret)
text(treeret, use.n = TRUE)


#####mit ctree keine signfikanten Effekte
dat$resp <- as.ordered(dat$resp)
mtree <- ctree(resp  ~ age+smoke+lwt+ht+ui+ftv, data = dat, control = ctree_control(testtype = "Bonferroni",
                                                                               mincriterion = 0.2))

mtree <- ctree(resp  ~ age+smoke+lwt+ht+ui+ftv, data = dat, control = ctree_control(testtype = "Univariate",
                                                                                    mincriterion = 0.9))
mtree <- ctree(resp  ~ lwt+ui, data = dat, control = ctree_control(testtype = "Bonferroni",
                                                                                    mincriterion = 0.5))
plot(mtree)

##random forest
require(randomForest)
dat$resp <- as.factor(dat$resp)
bw.rf=randomForest(resp ~ age+smoke+lwt+ht+ui+ftv , data = dat )
plot(bw.rf)
bw.rf 
################Versuche mit random forests: ICE

require(randomForest)
require(MASS)#Package which contains the Boston housing dataset
attach(Boston)
set.seed(101)

summary(Boston)

train=sample(1:nrow(Boston),300)
Boston.rf=randomForest(medv ~ . , data = Boston , subset = train)
Boston.rf

plot(Boston.rf)

##############
datconf <-  data
summary(datconf) 
dim(datconf)
datconf
conf.rf <- randomForest(income ~ . , data = datconf )
plot(conf.rf)

ag <- c(18:80)


n<-20
plotv <-matrix(0,nrow=63,ncol=n)

for(i in 1:n){ 
  #row<-50
  new <- datconf[i,]
  
  new <- new[rep(1:nrow(new), times = 63), ]
  new$age <- ag
  
  conf.pred <- predict(conf.rf, new, type="response")
  
  lo <- loess(conf.pred~ag)
  predict(lo)
  plotv[,i] <-  predict(lo)
  
  #plotv[,i] <-  conf.pred
}

label.size <-1.3
label.sizehead <- 1.5
ylims <- range(c(0,dis))

#plot(ag, conf.pred, type="l",  main = " ", xlab=" ",ylab="",  lwd=3,cex.lab=label.size,cex.axis=label.size)

plotv<- scale(plotv,center = TRUE, scale = FALSE) 

matplot(ag, plotv, type = "l", lty = 1:1, lwd = 3, lend = par("lend"),
        pch = NULL,
        col = 1:6, cex = NULL, bg = NA,
        xlab = NULL, ylab = NULL, xlim = NULL, ylim = NULL,
        log = "",  add = FALSE)


#####function OrdPred
##############prediction adj cat datalearn: lerndaten, datpred: prädiktionsdaten, k Anzahl Kategorien)
################Cat ist response  aber auf birthweight data zugeschnitten: resp ~ age+smoke+lwt+ht+ftv
source("./Ordpredf.R")


################
Ordpredf <- function(form, datlearn,datpred,k){
  preddim <-dim(datpred)
  np <- preddim[1]
  
  trcomp <- matrix(0,nrow=np,ncol=k)
  
  
  for(l in 2:k){
    
    
    dat <- datlearn
    dat <- subset(dat, Cat == l | Cat == l-1)
    xdim<-dim(dat)
    n <- xdim[1]
    
    dat$Cat <- as.numeric(dat$Cat)
    datpred$resp <- 0
    dat$resp <- 0
    
    for(i in 1:n){if(dat$Cat[i] >=l) {dat$resp[i] <- 1}}
    dat$resp <- as.factor(dat$resp)
    #summary(dat)
    
    output.forest <- randomForest(form, data = dat)
    #print(output.forest) 
    #plot(output.forest)
    conf.pred <- predict(output.forest, datpred, type="prob")
    #Korrektur
    minn<-0.005
    conf.pred[,2]<- (1-2*minn)*conf.pred[,2]+minn
    #
    tr <- log(conf.pred[,2]/(1-conf.pred[,2]))
    trcomp[,l]<- tr
  }
  
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

########################
############endfunction

#############################################proportional odds, Ordpredf and plots

########birthweight
datlearn <-birthwt
datpred <- birthwt[1:10,]

form <-resp ~ age+smoke+lwt+ht+ui+ftv
mmtry <- 2
d<-Ordpredf(form,datlearn,datpred,k,mmtry)


birthwt$Cat<- as.ordered(birthwt$Cat)
pom <- vglm(Cat ~ age + lwt + race + smoke +
              ptl + ht + ui + ftv, family = cumulative (parallel=TRUE), data=birthwt)

summary(pom)
pred <- predict(pom, datpred, type="response")

#######################plot  ????

xa <- c(1:k)
ya <- pred[1,]
yarand <- d[1,]
label.size <-1.3
label.sizehead <- 1.5
ylims <- range(c(0,yarand))

plot(xa, ya, type="h", ylim=ylims,
     main = "", xlab="categories",ylab="",  lwd=3,cex.lab=label.size,cex.axis=label.size,pch=2,col="gray")
xamod <- xa+0.04
lines(xamod, yarand, lwd=3, col="red",type="h")

plot(xa, ya, type="l", ylim=ylims,
     main = "", xlab="categories",ylab="",  lwd=3,cex.lab=label.size,cex.axis=label.size,pch=19,col="gray")
xamod <- xa+0.04
lines(xamod, yarand, lwd=3, col="red",type="l")

###weitere Beobachtungen
ya <- pred[2,]
yarand <- d[2,]
ylims <- range(c(0,yarand))



