library(ordinal)
library(DBI)
library(RMySQL)
library(plyr)
library(ggplot2)
library(missForest)
library(stargazer)


sysName = Sys.info()["sysname"]
if (sysName == "linux"){
  setwd("//stash/bikeapp/ORcycle_Analysis_Tool_Suite")
}else{
  setwd("/Users/bblanc/OneDrive/_BikeAppProject/ORcycle_Analysis_Tool_Suite")
}

userModelTab = readRDS("working_data/userModelData/preparedUserModelTab_2015-06-26.csv")
crashTab = readRDS("working_data/reportModelData/preparedCrashModelTab_2015-07-03.csv")
issueTab = readRDS("working_data/reportModelData/preparedIssueModelTab_2015-07-03.csv")
#write.csv(issueTab,"working_data/reportModelData/preparedIssueModelTab_2015-07-12.csv")
#write.csv(crashTab,"working_data/reportModelData/preparedCrashModelTab_2015-07-12.csv")

dbInfo = readLines("db_info.txt")
user_name = substr(dbInfo[9],12,nchar(dbInfo[9]))
password = substr(dbInfo[10],11,nchar(dbInfo[10]))
db_name = substr(dbInfo[11],11,nchar(dbInfo[11]))
host = substr(dbInfo[12],7,nchar(dbInfo[7]))

####Connect to ORcycle db
####Make sure you VPN into cecs network 
con <- dbConnect(dbDriver("MySQL"), host=host, port= 3306, user=user_name, password = password, dbname=db_name)
users = dbReadTable(con,"user")
notes = dbReadTable(con,"note")
crashTab$user_id = NA
for (i in 1:nrow(crashTab)){
  nid = crashTab$note_id[i]
  crashTab$user_id[i] = notes$user_id[notes$id == nid]
}
issueTab$user_id = NA
for (i in 1:nrow(issueTab)){
  nid = issueTab$note_id[i]
  issueTab$user_id[i] = notes$user_id[notes$id == nid]
}

crashModelTab = join(crashTab,userModelTab,by="user_id")
issueModelTab = join(issueTab,userModelTab,by="user_id")



#crashModelTab = subset(crashModelTab,crashModelTab$insideNetwork==TRUE)
#issueModelTab = subset(issueModelTab,issueModelTab$insideNetwork==TRUE)

#####Text categories 
intTypeLabels_1 = c("Uncontrolled Intersection",
                  "Stop Controlled Intersection from 2 Directions",
                  "Stop Controlled Intersection from >=3 Directions",
                  "Traffic Signal Controlled Intersection from >=3 Directions, No Bike Signals",
                  "Traffic Signal Controlled Intersection from >=3 Directions, with Bike Signals",
                  "Intersection with traffic circle",
                  "No Intersection")

intTypeLabels_2 = c("Uncontrolled Intersection",
                    "Stop Controlled Intersection from 2 Directions",
                    "Stop Controlled Intersection from >=3 Directions",
                    "Traffic Signal Controlled Intersection from 2 Directions, No Bike Signals",
                    "Traffic Signal Controlled Intersection from >=3 Directions, No Bike Signals",
                    "No Intersection")



source("functions/wrap_sentance.R")

intTypeLabs_1 = wrap_sentence(intTypeLabels_1,20)
intTypeLabs_2 = wrap_sentence(intTypeLabels_2,20)
###Exploration

theme_set(theme_grey(base_size = 20))

exploreDiscrete = function(iv,reportType,reportTable,ivLabs,ivAxis){
  if(reportType ==1){
    data = ddply(reportTable,c(iv),summarise,lts = mean(LTS,na.rm = TRUE), urgency = mean(as.numeric(urgency),na.rm = TRUE))
    data$numObs = paste0("N=",count(reportTable,iv)$freq)
    data$pos_lts = data$lts/2
    data$pos_urgency=data$urgency/2
    data[,iv]=factor(data[,iv],exclude = NULL)
    png(paste0("/Users/bblanc/OneDrive/_BikeAppProject/ORcycle_Analysis_Tool_Suite/results/miguelExplore/issue_",iv,"_vs_lts.png"),width = 1200,height = 800)
    print(ggplot(data,aes_string(x=iv,y="lts"))+geom_bar(stat="identity",fill= "#00759A")+scale_x_discrete(labels=ivLabs)+coord_flip()+xlab(ivAxis)+ylab("Level of Traffic Stress")+geom_text(aes(label = numObs, y = pos_lts),size = 10))
    dev.off()
    png(paste0("/Users/bblanc/OneDrive/_BikeAppProject/ORcycle_Analysis_Tool_Suite/results/miguelExplore/issue_",iv,"_vs_urgency.png"),width = 1200,height = 800)
    print(ggplot(data,aes_string(x=iv,y="urgency"))+geom_bar(stat="identity",fill= "#00759A")+scale_x_discrete(labels=ivLabs)+coord_flip()+xlab(ivAxis)+ylab("Urgency")+geom_text(aes(label = numObs, y = pos_urgency),size = 10))
    dev.off()
  }else if(reportType==2){
    data = ddply(reportTable,c(iv),summarise,lts = mean(LTS,na.rm = TRUE), severity = mean(as.numeric(severity),na.rm = TRUE))
    data$numObs = paste0("N=",count(reportTable,iv)$freq)
    data$pos_lts = data$lts/2
    data$pos_severity=data$severity/2
    data[,iv]=factor(data[,iv],exclude = NULL)
    png(paste0("/Users/bblanc/OneDrive/_BikeAppProject/ORcycle_Analysis_Tool_Suite/results/miguelExplore/crash_",iv,"_vs_lts.png"),width = 1200,height = 800)
    print(ggplot(data,aes_string(x=iv,y="lts"))+geom_bar(stat="identity",fill= "#00759A")+scale_x_discrete(labels=ivLabs)+coord_flip()+xlab(ivAxis)+ylab("Level of Traffic Stress")+geom_text(aes(label = numObs, y = pos_lts),size = 10))
    dev.off()
    png(paste0("/Users/bblanc/OneDrive/_BikeAppProject/ORcycle_Analysis_Tool_Suite/results/miguelExplore/crash_",iv,"_vs_severity.png"),width = 1200,height = 800)
    print(ggplot(data,aes_string(x=iv,y="severity"))+geom_bar(stat="identity",fill= "#00759A")+scale_x_discrete(labels=ivLabs)+coord_flip()+xlab(ivAxis)+ylab("severity")+geom_text(aes(label = numObs, y = pos_severity),size = 10))
    dev.off()
  }
  
}

#ggplot(data,aes(x=factor(intType),y=urgency))+geom_bar(stat="identity")+scale_x_discrete(labels=ivLabs)+coord_flip()+xlab("Intersection Type")+ylab("Urgency")

exploreDiscrete("intType",reportType = 1,issueModelTab,ivLabs = intTypeLabs_1, ivAxis = "Intersection Type")
exploreDiscrete("nearIntersect",reportType = 1,issueModelTab, ivLabs = c("False","True","NA"),ivAxis = "Near Intersection?" )
exploreDiscrete("adjParking",reportType = 1,issueModelTab, ivLabs = c("False","True","NA"),ivAxis = "Adjacent to Parking?" )
exploreDiscrete("twoWay",reportType = 1,issueModelTab, ivLabs = c("False","True","NA"),ivAxis = "Two Way Street?" )
exploreDiscrete("bikeBox",reportType = 1,issueModelTab, ivLabs = c("False","True","NA"),ivAxis = "Bicycle Box?" )
exploreDiscrete("centerLine",reportType = 1,issueModelTab, ivLabs = c("False","True","NA"),ivAxis = "Marked Center Line?" )
exploreDiscrete("bikeTypeLong",reportType = 1,issueModelTab, ivLabs = levels(factor(issueModelTab$bikeTypeLong)),ivAxis = "Bicycle Facility Type" )
exploreDiscrete("linkType",reportType = 1,issueModelTab, ivLabs = levels(factor(issueModelTab$linkType)),ivAxis = "Street/Road Type" )


exploreDiscrete("intType",reportType = 2,crashModelTab,ivLabs = intTypeLabs_2, ivAxis = "Intersection Type")

exploreDiscrete("issueType",reportType=1,issueModelTab,ivLabs=levels(factor(issue)))


cm = subset(crashModelTab,crashModelTab$insideNetwork==TRUE)
im = subset(issueModelTab,issueModelTab$insideNetwork==TRUE)

source("functions/imputeResponses.R")
cm$LTS=factor(cm$LTS,ordered = TRUE)
im$LTS=factor(im$LTS,ordered = TRUE)
cm$numLegs=factor(cm$numLegs)
im$numLegs=factor(im$numLegs)
cm$intType=factor(cm$intType)
im$intType=factor(im$intType)
cm$numLanesAuto=factor(cm$numLanesAuto,ordered = TRUE)
im$numLanesAuto=factor(im$numLanesAuto,ordered = TRUE)
imImp = imputeResponses(data=im,impCols = c(3:17,23:32,35:43))
cmImp = imputeResponses(data=cm,impCols = c(3:36,42:51,54:62))

ggConvert = function(df,x,y,reportType){
  xCol = df[,x]
  yCol=df[,y]
  xCats = levels(xCol)
  yCats = levels(yCol)
  pf = data.frame(matrix(nrow = length(xCats)*length(yCats),ncol=4))
  colnames(pf)=c(x,y,"numObs","numCat")
  xpCats = vector()
  vi=1
  for (i in 1:length(xCats)){
    xpCats[vi:(vi+length(yCats)-1)]=xCats[i]
    vi = vi+length(yCats)
  }
  pf[,x]=xpCats
  pf[,y]=yCats
  for(i in 1:nrow(pf)){
    xCat = pf[i,x]
    yCat=pf[i,y]
    pf$numObs[i]=nrow(df[as.character(df[,x])==xCat & as.character(df[,y])==yCat,])
  }
  pf[,x]=factor(pf[,x],ordered = TRUE,levels = xCats)
  pf[,y]=factor(pf[,y],ordered=TRUE,levels=yCats)
  pf$numCat="0"
  pf$numCat[pf$numObs>=1 &pf$numObs <=5]="1-5"
  pf$numCat[pf$numObs>=6 &pf$numObs <=10]="6-10"
  pf$numCat[pf$numObs>=11 &pf$numObs <=15]="11-15"
  pf$numCat[pf$numObs>=16 &pf$numObs <=20]="16-20"
  pf$numCat[pf$numObs>=21 ]=">21"
  pf$numCat = factor(pf$numCat,ordered=TRUE,levels = c("0","1-5","6-10","11-15","16-20",">21"))
  yLabs = wrap_sentence(yCats,25)
  xLabs=wrap_sentence(xCats,25)
  plt = ggplot(pf,aes_string(x=x,y=y))+geom_point(aes(size=numCat))+scale_size_discrete(range = c(0,5,10,15,20,25)*3)+ guides(size=guide_legend(title="Number of Observations"))+scale_x_discrete(labels=xLabs)+scale_y_discrete(labels=yLabs)+theme(axis.text.x = element_text(angle = 45, hjust = 1))
  if(length(xCats)>length(yCats)){
    plt = plt+coord_flip()
    png(filename = paste0(outputDir,reportType,"_",x,"_",y,".png"),width=(500+length(yCats)*70),height=(100+length(xCats)*70))
    print(plt)
    dev.off()
  }else{
    png(filename = paste0(outputDir,reportType,"_",x,"_",y,".png"),width=(500+length(xCats)*70),height=(00+length(yCats)*70))
    print(plt)
    dev.off()
  }
}

ggConvertMulti=function(df,xCols,y,xName,xLabs,reportType){
  xFrame = df[,xCols]
  xCats = colnames(xFrame)
  yCol = df[,y]
  yCats = levels(yCol)
  pf = data.frame(matrix(ncol=4,nrow = length(yCats)*ncol(xFrame)))
  colnames(pf)=c(xName,y,"numObs","numCat")
  xpCats = vector()
  vi=1
  for (i in 1:length(xCats)){
    xpCats[vi:(vi+length(yCats)-1)]=xCats[i]
    vi = vi+length(yCats)
  }
  pf[,xName]=xpCats
  pf[,y]=yCats
  for(i in 1:nrow(pf)){
    xCat = pf[i,xName]
    yCat=pf[i,y]
    pf$numObs[i]=nrow(df[df[,xCat] & as.character(df[,y])==yCat,])
  }
  pf[,xName]=factor(pf[,xName],ordered = TRUE,levels = xCats)
  pf[,y]=factor(pf[,y],ordered=TRUE,levels=yCats)
  pf$numCat="0"
  pf$numCat[pf$numObs>=1 &pf$numObs <=5]="1-5"
  pf$numCat[pf$numObs>=6 &pf$numObs <=10]="6-10"
  pf$numCat[pf$numObs>=11 &pf$numObs <=15]="11-15"
  pf$numCat[pf$numObs>=16 &pf$numObs <=20]="16-20"
  pf$numCat[pf$numObs>=21 ]=">21"
  pf$numCat = factor(pf$numCat,ordered=TRUE,levels = c("0","1-5","6-10","11-15","16-20",">21"))
  yLabs = wrap_sentence(yCats,25)
  plt = ggplot(pf,aes_string(x=xName,y=y))+geom_point(aes(size=numCat))+scale_size_discrete(range = c(0,5,10,15,20,25)*3)+ guides(size=guide_legend(title="Number of Observations"))+scale_x_discrete(labels=xLabs)+scale_y_discrete(labels=yLabs)+theme(axis.text.x = element_text(angle = 45, hjust = 1))
  if(length(xCats)>length(yCats)){
    plt = plt+coord_flip()
    png(filename = paste0(outputDir,reportType,"_",xName,"_",y,".png"),width=(500+length(yCats)*70),height=(100+length(xCats)*70))
    print(plt)
    dev.off()
  }else{
    png(filename = paste0(outputDir,reportType,"_",xName,"_",y,".png"),width=(500+length(xCats)*70),height=(00+length(yCats)*70))
    print(plt)
    dev.off()
  }
  
}
source("functions/wrap_sentance.R")

outputDir = "/Users/bblanc/OneDrive/_BikeAppProject/_TRB_Papers/Reports/"
reportCats=read.csv(paste0(outputDir,"reportCats.csv"))


ggConvert(imImp,"LTS","urgency",reportType = "issue")
ggConvert(imImp,"LTS","riderAbility",reportType = "issue")
ggConvert(imImp,"urgency","riderAbility",reportType = "issue")
ggConvert(cmImp,"LTS","severity",reportType = "crash")
ggConvert(cmImp,"LTS","riderAbility",reportType = "crash")
ggConvert(cmImp,"severity","riderAbility",reportType="crash")

ggConvertMulti(df =imImp,xCols=27:41,y="urgency",xName="issueType",
               xLabs = wrap_sentence(c("None",as.character(reportCats$issueType)),25),
               reportType="issue")
ggConvertMulti(df =cmImp,xCols=27:37,y="severity",xName="conflictWith",
               xLabs = wrap_sentence(c("None",as.character(reportCats$conflictWith)),25),
               reportType="crash")
ggConvertMulti(df =imImp,xCols=27:41,y="LTS",xName="issueType",
               xLabs = wrap_sentence(c("None",as.character(reportCats$issueType)),25),
               reportType="issue")
ggConvertMulti(df =cmImp,xCols=27:37,y="LTS",xName="conflictWith",
               xLabs = wrap_sentence(c("None",as.character(reportCats$conflictWith)),25),
               reportType="crash")
ggConvertMulti(df =imImp,xCols=27:41,y="riderAbility",xName="issueType",
               xLabs = wrap_sentence(c("None",as.character(reportCats$issueType)),25),
               reportType="issue")
ggConvertMulti(df =cmImp,xCols=27:37,y="riderAbility",xName="conflictWith",
               xLabs = wrap_sentence(c("None",as.character(reportCats$conflictWith)),25),
               reportType="crash")


#ggplot(cmImp,aes(x=dummy))+geom_bar(stat="bin")+facet_grid(LTS~severity,scales="free_x")+xlab("LTS")

mod = clm(data = cmImp,formula ="severity~numLanesAuto+centerLine")
mod = clm(data = imImp,formula ="urgency~numLanesAuto+centerLine")

removeUniform = function(df){
  cols = colnames(df)
  colsRemove=vector()
  for(i in 1:length(cols)){
    test = unique(df[,cols[i]])
    if(length(test)<=1){
      colsRemove = c(colsRemove,cols[i])
    }
  }
  return(df[,colnames(df)[!(colnames(df) %in% colsRemove)]])
}

classes = sapply(cmImp,class)
num.ord = names(classes[unlist(lapply(classes,function(x) x[1]=="numeric"))])
cmNums = cmImp[,num.ord[num.ord != "user_id" & num.ord !="note_id"]]
cmNums = removeUniform(cmNums)
corMat = corr.test(cmNums)
png(paste0(reportModelDir,"/corrplot.png"),width=1200,height=1200)
corrplot(corMat$r,method="ellipse",order="AOE")
#corrplot.mixed(corMat$r,p.mat=corMat$p,lower="number",upper="ellipse",order="AOE", sig.level = 0.05)
dev.off()

ggConvertVis()

reportModelDir = "/Users/bblanc/OneDrive/_BikeAppProject/_TRB_Papers/Reports/regressionModels/"

testModels = function(df,dVar,iVars,outFolder){
  for (i in 1:length(iVars)){
    formula = as.formula(paste0(dVar,"~",iVars[i]))
    model = clm(formula=formula,data = df,link="logit")
    stargazer(model,"html",out =paste0(outFolder,"/",dVar,"_",iVars[i],".html"),single.row = TRUE)
  }
}

testModels(df=imImp,dVar="urgency",
           iVars=c(colnames(imImp)[c(14:26,28:41,46:60)]),
           outFolder = paste0(reportModelDir,"/issueModels"))

testModels(df=cmImp,dVar="severity",
           iVars=c(colnames(cmImp)[c(14:26,28:37,39:48,50:60,65:79)]),
           outFolder = paste0(reportModelDir,"/crashModels"))

####Urgency Model
issueModel = clm(urgency~trafficCat,data = imImp,link="logit")

###Crash Models
crashModel = clm(severity~LTS+riderAbility,data = crashModelTab,link="logit")



###Issue Models

imImp$genderMale = FALSE
imImp$genderMale[as.character(imImp$gender)=="Male"] = TRUE
imImp$traffic=imImp$traffic/1000

issMod = clm(urgency~traffic+issueType_2+issueType_3+issueType_4+issueType_5+issueType_8+issueType_10+issueType_12+issueType_14+genderMale+income+LTS,data=imImp,link="logit")
stargazer(issMod,type="latex",single.row = TRUE,out=paste0(reportModelDir,"/issueModels/issueModel1.html"))
stepper = step(issMod)

final = clm(urgency~issueType_4+issueType_8+issueType_12+genderMale+income,data=imImp,link="logit")
stargazer(final,type="latex",single.row = TRUE,out=paste0(reportModelDir,"/issueModels/issueModel.html"))

ints = subset(imImp,imImp$nearIntersect==TRUE)
intMod = clm(urgency~trafficCat+speedCat+issueType_3+issueType_4+issueType_5+issueType_8+issueType_12+issueType_14+numLanesAuto+adjParking,data=ints,link="logit")

##Traffic Speed
issueModel$speedCat = relevel(issueModelTab$speedCat,ref = "s_less.equal_20")
speed = clm(urgency~speedCat,data = issueModelTab,link="logit")

##Traffic Volume
issueModel$trafficCat = relevel(issueModelTab$trafficCat,ref = "t_less5k")
traffic = clm(urgency~trafficCat,data = issueModelTab,link="logit")

##Near Intersection
nearInt = clm(urgency~nearIntersect,data = issueModelTab,link="logit")

#Intersection Type
issueModelTab$intType=factor(issueModelTab$intType)
intType = clm(urgency~intType,data = issueModelTab,link="logit")

#Number of lanes
numLanes = clm(urgency~numLanesAuto,data = issueModelTab,link="logit")


####Sample Description
ggplot(cmImp,aes())

###More attempts at multivariate models
imImp$traffic=imImp$traffic/1000
issMod = clm(urgency~traffic+speed+issueType_3+issueType_4+issueType_5+issueType_8+issueType_12+issueType_14+centerLine+adjParking,data=imImp,link="logit")
#stargazer(issMod,type="latex",single.row = TRUE,out=paste0(reportModelDir,"/issueModels/issueModel1.html"))
stepper = step(issMod)
stargazer(stepper,type="latex",single.row = TRUE,out=paste0(reportModelDir,"/issueModels/issueModel2_step.html"))

#cmImp$traffic=cmImp$traffic/1000
crashMod = clm(severity~traffic+conflict_1+conflict_2+speed+centerLine+adjParking+numLanesAuto,data=cmImp,link="logit")
stepper = step(crashMod)
stargazer(stepper,type="latex",single.row = TRUE,out=paste0(reportModelDir,"/crashModels/crashModel1_step.html"))
