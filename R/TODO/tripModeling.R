library(ordinal)
library(DBI)
library(RMySQL)
library(plyr)
library(stargazer)


sysName = Sys.info()["sysname"]
if (sysName == "linux"){
  setwd("//stash/bikeapp/ORcycle_Analysis_Tool_Suite")
}else{
  setwd("/Users/bblanc/OneDrive/_BikeAppProject/ORcycle_Analysis_Tool_Suite")
}

#TODO: Load saved out model tabs for users and trips
userModelTab = readRDS("working_data/userModelData/preparedUserModelTab_2015-06-07.csv")
tripTab = readRDS("working_data/tripModelData/preparedTripModelTab_2015-07-18.rds")

dbInfo = readLines("db_info.txt")
user_name = substr(dbInfo[9],12,nchar(dbInfo[9]))
password = substr(dbInfo[10],11,nchar(dbInfo[10]))
db_name = substr(dbInfo[11],11,nchar(dbInfo[11]))
host = substr(dbInfo[12],7,nchar(dbInfo[7]))

####Connect to ORcycle db
####Make sure you VPN into cecs network 
con <- dbConnect(dbDriver("MySQL"), host=host, port= 3306, user=user_name, password = password, dbname=db_name)
users = dbReadTable(con,"user")
trips = dbReadTable(con,"trip")
tripTab$user_id = NA
for (i in 1:nrow(tripTab)){
  tid = tripTab$trip_id[i]
  tripTab$user_id[i] = trips$user_id[trips$id == tid]
}

tripModelTab = join(tripTab,userModelTab,by="user_id")

tripModelTab = subset(tripModelTab,tripModelTab$matchedDistance >=0.25)

trips$date = substr(trips$stop,1,10)
trips = subset(trips,!is.na(trips$date))
endDate = as.POSIXct(strptime("2015-05-31",format = "%Y-%m-%d"))
trips$date = as.POSIXct(strptime(trips$date,format = "%Y-%m-%d"))
for (i in 1:nrow(tripModelTab)){
  tid = tripModelTab$trip_id[i]
  tripModelTab$date[i] = trips$date[trips$id == tid]
}
tripModelTab = subset(tripModelTab,tripModelTab$date<=endDate)

questions = dbReadTable(con, "questions")
answers = dbReadTable(con,"answers")
qaMap = dbReadTable(con,"question_answer_relate")

###Model Output Directory
modelOutDir = "/Users/bblanc/OneDrive/_BikeAppProject/_TRB_Papers/Trips/regressionModels/"

##Load forest plot function
source("functions/forestplot.R")
theme_set(theme_bw(base_size = 25))

#####Seperate Variable Group Models

###1.) Trip Statistics Model
tripStats = clm(routeComfort~avgLinkSpeed,data =tripModelTab,link="logit", weights = tripModelTab$similarWeight)
varNames = "Average Speed (mph)"
forestplot(df =tripStats,title = "Trip Statistics",outputDir = modelOutDir,rows = varNames)
stargazer(tripStats,type = "html",out = paste0(modelOutDir,"tripStats.html"),single.row = TRUE,dep.var.labels = "Route Comfort Rating",
          covariate.labels = varNames)

###2.) Temporal Characteristics Model
tripTime = clm(routeComfort~dowCat+startCat,data =tripModelTab,link="logit", weights = tripModelTab$similarWeight)
varNames = c("Weekend","Start Time = AM Peak","Start Time = Off-Peak Night","Start Time = PM Peak")
forestplot(df =tripTime,title = "Temporal Characteristics",outputDir = modelOutDir,rows = varNames)
stargazer(tripTime,type = "html",out = paste0(modelOutDir,"temporal.html"),single.row = TRUE,dep.var.labels = "Route Comfort Rating",
          covariate.labels = varNames)

###3.) Trip Response Model
formula = as.formula(paste("routeComfort~routeStress_1+routeStress_2+routeStress_3+routeStress_4+routeStress_5+routeStress_6+routeStress_7+routeStress_8",
                           "+routePref_1+routePref_2+routePref_3+routePref_4+routePref_5+routePref_6+routePref_7+routePref_8+routePref_9+routePref_10+routePref_11+routePref_12",
                           "+purpose+routeFreq",sep =""))
tripResponseAll=clm(formula = formula,data = tripModelTab,link = "logit", weights = tripModelTab$similarWeight)
stepper = step(tripResponseAll)

formula = as.formula(paste("routeComfort~routeStress_1+routeStress_3",
                           "+routePref_1+routePref_2+routePref_3+routePref_5+routePref_7",
                           "+purpose",sep =""))

routeStressLabels = answers$text[answers$id %in% qaMap$answer_id[qaMap$question_id==27]]
routePrefLabels = answers$text[answers$id %in% qaMap$answer_id[qaMap$question_id==21]]
purposeLabels = levels(tripModelTab$purpose)[2:8]

varNames = c(routeStressLabels[1],routeStressLabels[3],routePrefLabels[2],routePrefLabels[3],routePrefLabels[4],routePrefLabels[6],routePrefLabels[8],purposeLabels)

tripResponse=clm(formula = formula,data = tripModelTab,link = "logit", weights = tripModelTab$similarWeight)

forestplot(df =tripResponse,title = "Trip Responses",outputDir = modelOutDir,rows = varNames)
stargazer(tripResponse,type = "html",out = paste0(modelOutDir,"tripResponses.html"),single.row = TRUE,dep.var.labels = "Route Comfort Rating",
          covariate.labels = varNames)
###4.) User Response Model
formula = as.formula("routeComfort~age+gender+ethnicity+occupation+income+numBikes+cyclingFreq+cyclingWeather+riderAbility+riderType+veh2wrkRatio")
userModel = clm(formula = formula, link = "logit",data = tripModelTab, weights = tripModelTab$similarWeight)

stepper = step(userModel)

tripModelTab$ethnicity_white = FALSE
tripModelTab$ethnicity_white[as.character(tripModelTab$ethnicity)=="White American"]=TRUE
tripModelTab$gender_male = FALSE
tripModelTab$gender_male[as.character(tripModelTab$gender)=="Male"] = TRUE
tripModelTab$occ_employed=FALSE
tripModelTab$occ_employed[as.character(tripModelTab$occupation)=="Employed"]=TRUE

formula = as.formula("routeComfort~occ_employed")
userModel = clm(formula = formula, link = "logit",data = tripModelTab, weights = tripModelTab$similarWeight)

###5.) Bike Facility and Street Type Model
bikeStreet = clm(routeComfort~sp_all+bb_all+nb_primArt+nb_minArt+nb_res+bl_primArt+bl_minArt,data =tripModelTab,link="logit", weights = tripModelTab$similarWeight)
stepper = step(bikeStreet)
formula = as.formula("routeComfort~bb_all+bl_minArt+bl_primArt+nb_primArt+sp_all")
varNames = c("Bicycle Boulevard","Bike Lane, Minor Arterial","Bike Lane, Primary Arterial","No Bike Facility, Primary Arterial","Separated Path")
bikeStreet = clm(formula,data =tripModelTab,link="logit", weights = tripModelTab$similarWeight)
forestplot(df =bikeStreet,title = "Bicycle Facility and Street Types",outputDir = modelOutDir,rows = varNames)
stargazer(bikeStreet,type = "html",out = paste0(modelOutDir,"bikeStreet.html"),single.row = TRUE,dep.var.labels = "Route Comfort Rating",
          covariate.labels = varNames)

###6.) Traffic Volume Model
traffic = clm(routeComfort~t_less5k+t_5to10k+t_10to20k+t_20to30k+t_greater30k,data =tripModelTab,link="logit", weights = tripModelTab$similarWeight)
stepper = step(traffic)
formula = as.formula("routeComfort~t_less5k+t_5to10k+t_10to20k+t_20to30k+t_greater30k")
traffic = clm(formula,data =tripModelTab,link="logit", weights = tripModelTab$similarWeight)
varNames = c("Less than 5k veh/day","Between 5k and 10k veh/day","Between 10k and 20k veh/day","Between 20k and 30k veh/day","Greater than 30k veh/day")
forestplot(df =traffic,title = "Traffic Volumes",outputDir = modelOutDir,rows = varNames)
stargazer(traffic,type = "html",out = paste0(modelOutDir,"traffic.html"),single.row = TRUE,dep.var.labels = "Route Comfort Rating",
          covariate.labels = varNames)

###7.) Traffic Speed Model
speed = clm(routeComfort~s_less.equal_20+s_20to35+s_greater35,data =tripModelTab,link="logit", weights = tripModelTab$similarWeight)
stepper = step(speed)
formula = as.formula("routeComfort~s_less.equal_20+s_greater35")
speed = clm(formula,data =tripModelTab,link="logit", weights = tripModelTab$similarWeight)
varNames = c("<= 20 MPH","> 35 MPH")
forestplot(df =speed,title = "Posted Speed",outputDir = modelOutDir,rows = varNames)
stargazer(speed,type = "html",out = paste0(modelOutDir,"speed.html"),single.row = TRUE,dep.var.labels = "Route Comfort Rating",
          covariate.labels = varNames)

###8.) Topography Model
topo = clm(routeComfort~slp1+slp2+slp3+slp4+slp5,data =tripModelTab,link="logit", weights = tripModelTab$similarWeight)
stepper = step(topo)
formula = as.formula("routeComfort~slp5")
topo = clm(formula,data =tripModelTab,link="logit", weights = tripModelTab$similarWeight)
varNames = c("Grade >+6%")
forestplot(df =topo,title = "Topographical Grade",outputDir = modelOutDir,rows = varNames)
stargazer(topo,type = "html",out = paste0(modelOutDir,"grade.html"),single.row = TRUE,dep.var.labels = "Route Comfort Rating",
          covariate.labels = varNames)

###9.) Weather Characteristics Model
weather = clm(routeComfort~precip+temp+windSpeed+windGust+condCat,data =tripModelTab,link="logit", weights = tripModelTab$similarWeight)

###10.) Intersection Control Model
intersect = clm(routeComfort~stopsPerMile+signalsPerMile+turnsPerMile+bb_all,data =tripModelTab,link="logit", weights = tripModelTab$similarWeight)
varNames = c("# Stop Signs per Mile","# Traffic Signals per Mile","# Turns per Mile")
forestplot(df =intersect,title = "Intersection Characteristics",outputDir = modelOutDir,rows = varNames)
stargazer(intersect,type = "html",out = paste0(modelOutDir,"intersect.html"),single.row = TRUE,dep.var.labels = "Route Comfort Rating",
          covariate.labels = varNames)

###11.) Network Characteristics Model
#tripModelTab$turnsPerMile=tripModelTab$numTurns/tripModelTab$matchedDistance
#network = clm(routeComfort~turnsPerMile,data =tripModelTab,link="logit", weights = tripModelTab$similarWeight)

###12.) Traffic Calming Model
#tripModelTab$humpsPerMile = tripModelTab$numHumps/tripModelTab$matchedDistance
#calm = clm(routeComfort~numHumps+bb_all,data =tripModelTab,link="logit", weights = tripModelTab$similarWeight)

#########Pooled Model


###Curated pooled Model
pooledFormula = as.formula(
  paste0("routeComfort ~",
         "avgLinkSpeed",
         "+dowCat+startCat",
         "+routeStress_1+routeStress_3+routePref_1+routePref_2+routePref_3+routePref_5+routePref_7+purpose",
         "+occ_employed",
         "+bb_all+bl_minArt+bl_primArt+nb_primArt+sp_all",
         "+t_less5k+t_5to10k+t_10to20k+t_20to30k+t_greater30k",
         "+s_less.equal_20+s_greater35",
         "+slp5",
         "+stopsPerMile+signalsPerMile+turnsPerMile")
)

pooled = clm(pooledFormula,data =tripModelTab,link="logit", weights = tripModelTab$similarWeight)

stepper = step(pooled)


###Full pooled Model

pooledFormula = as.formula(
  paste0("routeComfort ~",
         "avgLinkSpeed+matchedDistance+matchedDuration",
         "+dowCat+startCat",
         "+routeStress_1+routeStress_2+routeStress_3+routeStress_4+routeStress_5+routeStress_6+routeStress_7+routeStress_8+routePref_1+routePref_2+routePref_3+routePref_4+routePref_5+routePref_6+routePref_7+routePref_8+routePref_9+routePref_10+routePref_11+routePref_12+routeFreq+purpose",
         "+occ_employed+gender_male+ethnicity_white+age+income+numBikes+cyclingFreq+cyclingWeather+riderAbility+riderType+veh2wrkRatio",
         "+sp_all+bb_all+nb_primArt+nb_minArt+nb_res+bl_primArt+bl_minArt",
         "+t_less5k+t_5to10k+t_10to20k+t_20to30k+t_greater30k",
         "+s_less.equal_20+s_20to35+s_greater35",
         "+slp1+slp2+slp3+slp4+slp5",
         "+stopsPerMile+signalsPerMile+turnsPerMile")
)

pooled = clm(pooledFormula,data =tripModelTab,link="logit", weights = tripModelTab$similarWeight)

stepper = step(pooled)

stargazer(stepper,type = "html",out = paste0(modelOutDir,"backwardsStepwise.html"),single.row = TRUE,dep.var.labels = "Route Comfort Rating")

####Bicycle Facility Specific Modeling

#Absolute
shortTrips = subset(tripModelTab,tripModelTab$matchedDistance<=4.5)
shortModel = clm(routeComfort~sp_all*matchedDistance+bb_all*matchedDistance+nb_primArt*matchedDistance+nb_minArt*matchedDistance+nb_res*matchedDistance+bl_primArt*matchedDistance+bl_minArt*matchedDistance+matchedDistance+matchedDistance^2,data =shortTrips,link="logit", weights = shortTrips$similarWeight)
stepper = step(shortModel)
varNames = c("Trip Distance (mi)","No Bike Facility, Primary Arterial (mi)","No Bike Facility, Residential", "Bike Lane, Primary Arterial (mi)","Bike Lane, Minor Arterial (mi)", "No Bike Facility, Residential x Trip Distance (mi)","Bike Lane, Primary Arterial x Trip Distance (mi)")
stargazer(stepper,type = "html",out = paste0(modelOutDir,"shortDist.html"),single.row = TRUE,dep.var.labels = "Route Comfort Rating",
          covariate.labels = varNames)

longTrips = subset(tripModelTab,tripModelTab$matchedDistance>4.5)
longModel = clm(routeComfort~sp_all*matchedDistance+bb_all*matchedDistance+nb_primArt*matchedDistance+nb_minArt*matchedDistance+nb_res*matchedDistance+bl_primArt*matchedDistance+bl_minArt*matchedDistance+matchedDistance+matchedDistance^2,data =longTrips,link="logit", weights = longTrips$similarWeight)
stepper = step(longModel)
varNames = c("Separated Path (mi)","Trip Distance (mi)","Bicycle Boulevard (mi)","No Bike Facility, Primary Arterial (mi)","No Bike Facility, Minor Arterial (mi)","Bike Lane, Primary Arterial (mi)","Bike Lane, Minor Arterial (mi)","Bicycle Boulevard x Trip Distance (mi)","No Bike Facility, Primarity Arterial x Trip Distance (mi)","No Bike Facility, Minor Arterial x Trip Distance (mi)", "Bike Lane, Minor Arterial x Trip Distance (mi)")
stargazer(stepper,type = "html",out = paste0(modelOutDir,"longDist.html"),single.row = TRUE,dep.var.labels = "Route Comfort Rating",
          covariate.labels = varNames)

#Proportions
propModel = tripModelTab
propModel[,49:72]=tripModelTab[,49:72]/tripModelTab$matchedDistance
shortTrips = subset(propModel,propModel$matchedDistance<=4.5)
shortModel = clm(routeComfort~sp_all*matchedDistance+bb_all*matchedDistance+nb_primArt*matchedDistance+nb_minArt*matchedDistance+nb_res*matchedDistance+bl_primArt*matchedDistance+bl_minArt*matchedDistance+matchedDistance+matchedDistance^2,data =shortTrips,link="logit", weights = shortTrips$similarWeight)
stepper = step(shortModel)
varNames = c("Seperated Path (%)","Trip Distance (mi)","Bicycle Boulevard (%)","No Bike Facility, Primary Arterial (%)","No Bike Facility, Residential (%)","Bike Lane, Primary Arterial (%)","Bike Lane, Minor Arterial (%)","Separated Path (%) x Trip Distance (mi)","Bicycle Boulevard (%) x Trip Distance (mi)","No Bike Facility, Primary Arterial (%) x Trip Distance (mi)","No Bike Facility, Residential (%) x Trip Distance (mi)","Bike Lane, Primary Arterial (%) x Trip Distance (mi)","Bike Lane, Minor Arterial (%) x Trip Distance (mi)")
stargazer(stepper,type = "html",out = paste0(modelOutDir,"shortProp.html"),single.row = TRUE,dep.var.labels = "Route Comfort Rating",
          covariate.labels = varNames)

longTrips = subset(propModel,propModel$matchedDistance>4.5)
longModel = clm(routeComfort~sp_all*matchedDistance+bb_all*matchedDistance+nb_primArt*matchedDistance+nb_minArt*matchedDistance+nb_res*matchedDistance+bl_primArt*matchedDistance+bl_minArt*matchedDistance+matchedDistance+matchedDistance^2,data =longTrips,link="logit", weights = longTrips$similarWeight)
stepper = step(longModel)
varNames = c("Separated Path (%)","Trip Distance (mi)","Bicycle Boulevard (%)","No Bike Facility, Primary Arterial (%)","No Bike Facility, Minor Arterial (%)","Bike Lane, Primary Arterial (%)","Bike Lane, Minor Arterial (%)","Bicycle Boulevard (%) x Trip Distance (mi)", "No Bike Facility, Minor Arterial (%) x Trip Distance (mi)","Bike Lane, Minor Arterial (%) x Trip Distance (mi)")
stargazer(stepper,type = "html",out = paste0(modelOutDir,"longProp.html"),single.row = TRUE,dep.var.labels = "Route Comfort Rating",
          covariate.labels = varNames)

###Include Trip Purpose

count(tripModelTab$purpose)
tripModelTab$purposeCat = NA
tripModelTab$purposeCat[as.character(tripModelTab$purpose)=="Commute" | as.character(tripModelTab$purpose)=="School" | as.character(tripModelTab$purpose)=="Work-related" ]="Cat1"
tripModelTab$purposeCat[as.character(tripModelTab$purpose)=="Exercise"] = "Cat2"
tripModelTab$purposeCat[as.character(tripModelTab$purpose)=="Social/entertainment" | as.character(tripModelTab$purpose)=="Shopping/errands"]="Cat3"
tripModelTab$purposeCat[as.character(tripModelTab$purpose)=="Transportation Access" | as.character(tripModelTab$purpose)=="Other "]="Cat4"
count(tripModelTab$purposeCat)

tripModelTab$pCommute = FALSE
tripModelTab$pCommute[as.character(tripModelTab$purpose)=="Commute"]=TRUE
tripModelTab$pSchool= FALSE
tripModelTab$pSchool[as.character(tripModelTab$purpose)=="School"]=TRUE
tripModelTab$pWorkRel = FALSE
tripModelTab$pWorkRel[as.character(tripModelTab$purpose)=="Work-related"]=TRUE
tripModelTab$pExercise = FALSE
tripModelTab$pExercise[as.character(tripModelTab$purpose)=="Exercise"]=TRUE
tripModelTab$pSocial = FALSE
tripModelTab$pSocial[as.character(tripModelTab$purpose)=="Social/entertainment"]=TRUE
tripModelTab$pShop = FALSE
tripModelTab$pShop[as.character(tripModelTab$purpose)=="Shopping/errands"]=TRUE
tripModelTab$pTrans=FALSE
tripModelTab$pTrans[as.character(tripModelTab$purpose)=="Transportation Access"]=TRUE
tripModelTab$pOther=FALSE
tripModelTab$pOther[as.character(tripModelTab$purpose)=="Other"]=TRUE

count(tripModelTab$routeFreq)
tripModelTab$rfCat = NA
tripModelTab$rfCat[(as.character(tripModelTab$routeFreq)=="First time ever" | as.character(tripModelTab$routeFreq)=="Once per year or less")]="Low Frequency"
tripModelTab$rfCat[!(as.character(tripModelTab$routeFreq)=="First time ever" | as.character(tripModelTab$routeFreq)=="Once per year or less")]=as.character(tripModelTab$routeFreq[!(as.character(tripModelTab$routeFreq)=="First time ever" |as.character(tripModelTab$routeFreq)=="Once per year or less")])
tripModelTab$rfCat= factor(tripModelTab$rfCat)
tripModelTab$rfCat=relevel(tripModelTab$rfCat,ref="Low Frequency")

#Absolute
shortTrips = subset(tripModelTab,tripModelTab$matchedDistance<=4.5)
shortModel = clm(routeComfort~sp_all+bb_all+nb_primArt+nb_minArt+nb_res+bl_primArt+bl_minArt+matchedDistance+matchedDistance^2+purposeCat,data =shortTrips,link="logit", weights = shortTrips$similarWeight)
stepper = step(shortModel)
varNames = c("No Bike Facility, Primary Arterial (mi)","Bike Lane, Minor Arterial (mi)", "Trip Purpose: Exercise","Trip Purpose: Social/Entertainment or Shopping/Errands","Trip Purpose: Transportation Access or Other")
stargazer(stepper,type = "html",out = paste0(modelOutDir,"shortDist.html"),single.row = TRUE,dep.var.labels = "Route Comfort Rating",
          covariate.labels = varNames)

longTrips = subset(tripModelTab,tripModelTab$matchedDistance>4.5)
longModel = clm(routeComfort~sp_all+bb_all+nb_primArt+nb_minArt+nb_res+bl_primArt+bl_minArt+matchedDistance+matchedDistance^2+purposeCat,data =longTrips,link="logit", weights = longTrips$similarWeight)
stepper = step(longModel)
varNames = c("Separated Path (mi)","Bicycle Boulevard (mi)","No Bike Facility, Primary Arterial (mi)","No Bike Facility, Residential (mi)","Bike Lane, Primary Arterial (mi)","Trip Purpose: Exercise","Trip Purpose: Social/Entertainment or Shopping/Errands","Trip Purpose: Transportation Access or Other")
stargazer(stepper,type = "html",out = paste0(modelOutDir,"longDist.html"),single.row = TRUE,dep.var.labels = "Route Comfort Rating",
          covariate.labels = varNames)

#Proportions
propModel = tripModelTab
propModel[,49:72]=tripModelTab[,49:72]/tripModelTab$matchedDistance
shortTrips = subset(propModel,propModel$matchedDistance<=4.5)
shortModel = clm(routeComfort~sp_all+bb_all+nb_primArt+nb_minArt+nb_res+bl_primArt+bl_minArt+matchedDistance+matchedDistance^2+purposeCat,data =shortTrips,link="logit", weights = shortTrips$similarWeight)
stepper = step(shortModel)
varNames = c("Seperated Path (%)","Bicycle Boulevard (%)","No Bike Facility, Primary Arterial (%)","No Bike Facility, Residential (%)","Trip Purpose: Exercise","Trip Purpose: Social/Entertainment or Shopping/Errands","Trip Purpose: Transportation Access or Other")
stargazer(stepper,type = "html",out = paste0(modelOutDir,"shortProp.html"),single.row = TRUE,dep.var.labels = "Route Comfort Rating",
          covariate.labels = varNames)

longTrips = subset(propModel,propModel$matchedDistance>4.5)
longModel = clm(routeComfort~sp_all+bb_all+nb_primArt+nb_minArt+nb_res+bl_primArt+bl_minArt+matchedDistance+matchedDistance^2+purposeCat,data =longTrips,link="logit", weights = longTrips$similarWeight)
stepper = step(longModel)
varNames = c("Separated Path (%)","Bicycle Boulevard (%)","No Bike Facility, Primary Arterial (%)","Bike Lane, Primary Arterial (%)","Trip Purpose: Exercise","Trip Purpose: Social/Entertainment or Shopping/Errands","Trip Purpose: Transportation Access or Other")
stargazer(stepper,type = "html",out = paste0(modelOutDir,"longProp.html"),single.row = TRUE,dep.var.labels = "Route Comfort Rating",
          covariate.labels = varNames)

#####Squared Terms
bikeComfort = ddply(tripModelTab,"routeComfort",function(x) colSums(x[50:60]))
bf = reshape(bikeComfort,varying=colnames(tripModelTab[50:60]),v.names="distance",idvar ="routeComfort",timevar="bikeCat",times = colnames(tripModelTab[50:60]),direction = "long")
bf$bikeCat=factor(bf$bikeCat,ordered=TRUE,levels = colnames(tripModelTab[50:60])[rev(order(bike_df$miles))])
bf$prop=0
for(i in 1:nrow(bf)){
  bikeType = bf$bikeCat[i]
  comfort = bf$routeComfort[i]
  sub = subset(bf,bf$bikeCat==bikeType)
  sub$prop = sub$distance/sum(sub$distance)
  bf$prop[i]=sub$prop[sub$routeComfort==comfort]
}
melted = melt(bf,c("routeComfort","bikeCat"))
dist = ggplot(bf,aes(x=bikeCat,y=distance,fill=routeComfort))+geom_bar(stat="identity",position="stack")+coord_flip()+ scale_fill_manual(name = "Route Comfort",values = brewer.pal(5, "RdYlGn"))+theme(legend.position="none")
prop = ggplot(bf,aes(x=bikeCat))+geom_bar(aes(weight=distance, fill = routeComfort),position="fill")+coord_flip()+ scale_fill_manual(name = "Route Comfort",values = brewer.pal(5, "RdYlGn"))+theme(legend.position="none")
ggplot(melted,aes(x=bikeCat))+geom_bar(data = subset(melted,melted$variable=="distance"),aes(y=value, fill = routeComfort),position="stack",stat="identity")+geom_bar(data = subset(melted,melted$variable=="prop"),aes(weight=value, fill = routeComfort),position="fill")+ 
  scale_fill_manual(name = "Route Comfort",values = brewer.pal(5, "RdYlGn"))+theme(legend.position="top")+facet_grid(variable~.,scales="free")+scale_x_discrete(labels=bikeTypes[rev(order(bike_df$miles))])+theme(axis.text.x = element_text(angle = 45, hjust = 1))
grid.arrange(dist,prop,ncol=2,nrow=1)

bikeSums = (colSums(tripModelTab[,c(50:52,54:60)]))
bike_df = data.frame(matrix(nrow=length(bikeSums),ncol=3))
colnames(bike_df)=c("bikeType","miles","bikeTypeLong")
bike_df$bikeType = names(bikeSums)
bike_df$miles=as.numeric(bikeSums)
bikeTypes = c("No Bike Facility, Primary Arterial",
  "No Bike Facility, Minor Arteial",
  "No Bike Facility, Residential",
  "Bike Lane, Primrary Arterial",
  "Bike Lane, Minor Arterial",
  "Bike Lane, Residential",
  "Bike Lane, Other",
  "Bicycle Boulevard",
  "Cycletrack",
  "Seperated Path")
bike_df$bikeTypeLong = bikeTypes
bike_df$bikeTypeLong= factor(bike_df$bikeTypeLong,ordered = TRUE,levels = bikeTypes[rev(order(bike_df$miles))])

theme_set(theme_bw(base_size = 30))
png(paste0(modelOutDir,"/bikeTypeDist.png"),width=1200,height=400)
ggplot(bike_df,aes(x=bikeTypeLong,y=miles))+geom_bar(stat="identity",fill = "#00759A")+coord_flip()+xlab("Bicycle Facility Type")+ylab("Miles")+ggtitle("Bicycle Facility Mileage Distribution among Model Sample")
dev.off()

barplot(bikeSums,horiz=TRUE,las=1)

tripModelTab$primArt = tripModelTab$nb_primArt+tripModelTab$bl_primArt
tripModelTab$minArt= tripModelTab$nb_minArt+tripModelTab$bl_minArt
tripModelTab$noBikeArt = tripModelTab$nb_minArt+tripModelTab$nb_primArt
tripModelTab$bl_Art=tripModelTab$bl_minArt+tripModelTab$bl_primArt
tripModelTab$allArt=tripModelTab$minArt+tripModelTab$primArt

propModelTab = tripModelTab
propModelTab[,c(50:73,111:115)]=(tripModelTab[,c(50:73,111:115)])/(tripModelTab$matchedDistance)

#Original
propModel = clm(routeComfort~sp_all+I(sp_all^2)+bb_all+I(bb_all^2)+nb_primArt+I(nb_primArt^2)+nb_minArt+I(nb_minArt^2)+nb_res+I(nb_res^2)+bl_primArt+I(bl_primArt^2)+bl_minArt+I(bl_minArt^2)+matchedDistance+I(matchedDistance^2)+purpose+routeStress_1+routeStress_2+routeStress_3+routeStress_4+routeStress_5+routeStress_6+routeStress_7+routeStress_8+rfCat,data =propModelTab,link="logit", weights = propModelTab$similarWeight)

#Pooled arterial types
propModel = clm(routeComfort~sp_all+I(sp_all^2)+bb_all+I(bb_all^2)+primArt+I(primArt^2)+minArt+I(minArt^2)+nb_res+I(nb_res^2)+matchedDistance+I(matchedDistance^2)+pCommute+pExercise+pShop+routeStress_1+routeStress_2+routeStress_3+routeStress_6,data =propModelTab,link="logit", weights = propModelTab$similarWeight)
stepper = step(propModel,direction="both")
final = clm(routeComfort~sp_all+matchedDistance+I(matchedDistance^2)+pExercise+pShop+routeStress_1+routeStress_2+routeStress_3+routeStress_6,data =propModelTab,link="logit", weights = propModelTab$similarWeight)
stargazer(final,type = "html",out = paste0(modelOutDir,"propModel_pooledArts.html"),single.row = TRUE,dep.var.labels = "Route Comfort Rating")

#Pooled Bike Lane and No Bike
propModel = clm(routeComfort~sp_all+I(sp_all^2)+bb_all+I(bb_all^2)+noBikeArt+I(noBikeArt^2)+bl_Art+I(bl_Art^2)+nb_res+I(nb_res^2)+matchedDistance+I(matchedDistance^2)+pCommute+pExercise+pShop+routeStress_1+routeStress_2+routeStress_3+routeStress_6,data =propModelTab,link="logit", weights = propModelTab$similarWeight)
stepper = step(propModel,direction="both")
final = clm(routeComfort~sp_all+bl_Art+matchedDistance+I(matchedDistance^2)+pExercise+pShop+routeStress_1+routeStress_2+routeStress_3+routeStress_6,data =propModelTab,link="logit", weights = propModelTab$similarWeight)
stargazer(final,type = "html",out = paste0(modelOutDir,"propModel_pooledBikes.html"),single.row = TRUE,dep.var.labels = "Route Comfort Rating")

##By Distance

#Pooled arterial types
distModel = clm(routeComfort~sp_all+I(sp_all^2)+bb_all+I(bb_all^2)+primArt+I(primArt^2)+minArt+I(minArt^2)+nb_res+I(nb_res^2)+matchedDistance+I(matchedDistance^2)+pCommute+pExercise+pShop+routeStress_1+routeStress_2+routeStress_3+routeStress_6,data =tripModelTab,link="logit", weights = tripModelTab$similarWeight)
stepper = step(distModel,direction="both")
final = clm(routeComfort~sp_all+bb_all+I(bb_all^2)+primArt+I(primArt^2)+minArt+I(minArt^2)+nb_res+pCommute+pExercise+pShop+routeStress_1+routeStress_2+routeStress_3+routeStress_6,data =tripModelTab,link="logit", weights = tripModelTab$similarWeight)
stargazer(final,type = "html",out = paste0(modelOutDir,"distModel_pooledArts.html"),single.row = TRUE,dep.var.labels = "Route Comfort Rating")

#Pooled Bike Lane and No Bike
distModel = clm(routeComfort~sp_all+I(sp_all^2)+bb_all+I(bb_all^2)+noBikeArt+I(noBikeArt^2)+bl_Art+I(bl_Art^2)+nb_res+I(nb_res^2)+matchedDistance+I(matchedDistance^2)+pCommute+pExercise+pShop+routeStress_1+routeStress_2+routeStress_3+routeStress_6,data =tripModelTab,link="logit", weights = tripModelTab$similarWeight)
stepper = step(propModel,direction="both")
final = clm(routeComfort~sp_all+bl_Art+matchedDistance+I(matchedDistance^2)+pExercise+pShop+routeStress_1+routeStress_2+routeStress_3+routeStress_6,data =tripModelTab,link="logit", weights = tripModelTab$similarWeight)
stargazer(final,type = "html",out = paste0(modelOutDir,"distModel_pooledBikes.html"),single.row = TRUE,dep.var.labels = "Route Comfort Rating")

###POOLED ARTERIAL

#Pooled arterial types
propModel = clm(routeComfort~sp_all+I(sp_all^2)+bb_all+I(bb_all^2)+allArt+nb_res+I(nb_res^2)+matchedDistance+I(matchedDistance^2)+pCommute+pExercise+pShop+routeStress_1+routeStress_2+routeStress_3+routeStress_6,data =propModelTab,link="logit", weights = propModelTab$similarWeight)
stepper = step(propModel,direction="both")
propFinal = clm(routeComfort~sp_all+allArt+matchedDistance+I(matchedDistance^2)+pExercise+pShop+routeStress_1+routeStress_2+routeStress_3+routeStress_6,data =propModelTab,link="logit", weights = propModelTab$similarWeight)
stargazer(final,type = "html",out = paste0(modelOutDir,"propModel_allArt.html"),single.row = TRUE,dep.var.labels = "Route Comfort Rating")

#Pooled arterial types
distModel = clm(routeComfort~sp_all+I(sp_all^2)+bb_all+I(bb_all^2)+allArt+nb_res+I(nb_res^2)+matchedDistance+I(matchedDistance^2)+pCommute+pExercise+pShop+routeStress_1+routeStress_2+routeStress_3+routeStress_6,data =tripModelTab,link="logit", weights = tripModelTab$similarWeight)
stepper = step(distModel,direction="both")
distFinal = clm(routeComfort~sp_all+bb_all+I(bb_all^2)+allArt+pExercise+pShop+routeStress_1+routeStress_2+routeStress_3+routeStress_6,data =tripModelTab,link="logit", weights = tripModelTab$similarWeight)
stargazer(final,type = "html",out = paste0(modelOutDir,"distModel_allArt.html"),single.row = TRUE,dep.var.labels = "Route Comfort Rating")

stargazer(propFinal,distFinal,type = "latex",out = paste0(modelOutDir,"finalModel.html"),single.row = TRUE,dep.var.labels = "Route Comfort Rating")

######
inputs = read.csv("/Users/bblanc/OneDrive/_BikeAppProject/_TRB_Papers/Trips/inputs.csv")
inputs$pShop=as.logical(inputs$pShop)
inputs$pExercise=as.logical(inputs$pExercise)
inputs$matchedDistance = inputs$sp_all+inputs$bb_all+inputs$allArt+inputs$Other
distEst = predict(distFinal,inputs)$fit
write.csv(distEst,"/Users/bblanc/OneDrive/_BikeAppProject/_TRB_Papers/Trips/distModelEstimates.csv")
propEst = predict(propFinal,inputs)$fit
write.csv(propEst,"/Users/bblanc/OneDrive/_BikeAppProject/_TRB_Papers/Trips/propModelEstimates.csv")

propEst=as.data.frame(propEst)
propEst$scenario = 1:nrow(propEst)
melted = melt(propEst,id.vars="scenario")
for(i in 1:nrow(melted)){
  comfort = melted$variable[i]
  scen = melted$scenario[i]
  if(scen==1){
    melted$change[i] = 0
  }else{
    melted$change[i]=melted$value[i]-melted$value[melted$scenario==1 & melted$variable==comfort]
  }
}
theme_set((theme_gray(base_size = 12)))
melted$sign = "minus"
melted$sign[melted$change>0]="plus"
ggplot(melted,aes(x=variable,fill=sign))+geom_bar(aes(y=change),stat="identity",position="dodge")+facet_grid(.~scenario)+coord_flip()+scale_fill_manual(values=(c("minus"="red","plus"="green")))


est = as.data.frame(est)
est$scenario = 1:nrow(est)
est$scenario=factor(est$scenario)
melted = melt(est,id.vars="scenario")
ggplot(melted,aes(x=variable,y=value))+geom_bar(stat="identity")+facet_grid(.~scenario)+coord_flip()+xlab("Route Comfort")+ylab("Probability")

propVarTab = propModelTab[,c("sp_all","bb_all","allArt","matchedDistance","pCommute","pExercise","pShop","routeStress_1","routeStress_2","routeStress_3","routeStress_6")]
propAllTab = propModelTab[,c(2:5,8,23:52,54:82,84:85,87:111,113)]
classes = sapply(propAllTab,class)
nums = classes[unlist(lapply(classes,function(x) x[1]=="numeric"))]
numeric = propAllTab[,names(nums)]
corMat = cor(numeric)
png(paste0(modelOutDir,"/prop_numeric_corr.png"),width=1200,height=1200)
corrplot(corMat,method="ellipse")
dev.off()

ordered = classes[unlist(lapply(classes,function(x) x[1]=="ordered"))]
ordered = propAllTab[,names(ordered)]
spearman.test(ordered)
corMat = hetcor(ordered)$correlations
png(paste0(modelOutDir,"/prop_ordered_corr.png"),width=1200,height=1200)
corrplot(corMat,method="ellipse")
dev.off()

factor = classes[unlist(lapply(classes,function(x) x[1]=="factor"))]
factor = propAllTab[,names(factor)]
#spearman.test(ordered)
corMat = hetcor(factor)$correlations
png(paste0(modelOutDir,"/prop_factor_corr.png"),width=1200,height=1200)
corrplot(corMat,method="ellipse")
dev.off()

logi = classes[unlist(lapply(classes,function(x) x[1]=="logical"))]
logi = propAllTab[,names(logi)]
#spearman.test(ordered)
corMat = corr.test(logi)
png(paste0(modelOutDir,"/prop_logical_corr.png"),width=1200,height=1200)
corrplot.mixed(corMat$r,p.mat=corMat$p,lower="number",upper="ellipse",order="AOE", sig.level = 0.05)
dev.off()

cor.mtest <- function(mat, conf.level = 0.95) {
  mat <- as.matrix(mat)
  n <- ncol(mat)
  p.mat <- lowCI.mat <- uppCI.mat <- matrix(NA, n, n)
  diag(p.mat) <- 0
  diag(lowCI.mat) <- diag(uppCI.mat) <- 1
  for (i in 1:(n - 1)) {
    for (j in (i + 1):n) {
      tmp <- cor.test(mat[, i], mat[, j], conf.level = conf.level)
      p.mat[i, j] <- p.mat[j, i] <- tmp$p.value
      lowCI.mat[i, j] <- lowCI.mat[j, i] <- tmp$conf.int[1]
      uppCI.mat[i, j] <- uppCI.mat[j, i] <- tmp$conf.int[2]
    }
  }
  return(list(p.mat, lowCI.mat, uppCI.mat))
}

distVarTab = tripModelTab[,c("sp_all","bb_all","allArt","pExercise","pShop","routeStress_1","routeStress_2","routeStress_3","routeStress_6")]
distCor = cor(distVarTab)
corrplot(distCor,method="ellipse")

finalDist = clm(routeComfort~sp_all+bb_all+bl_minArt+I(bl_minArt^2)+matchedDistance+I(matchedDistance^2)+pCommute+pExercise+pShop+
            +routeStress_1+routeStress_2+routeStress_3+routeStress_6,data =propModelTab,link="logit", weights = propModelTab$similarWeight)
final = clm(routeComfort~sp_all+bb_all+bl_minArt+matchedDistance+pCommute+pExercise+pShop+
              +routeStress_1+routeStress_2+routeStress_3+routeStress_6,data =propModelTab,link="logit", weights = propModelTab$similarWeight)
#varNames = c("Seperated Path (%)","Bicycle Boulevard (%)","No Bike Facility, Primary Arterial (%)","No Bike Facility, Residential (%)","Trip Purpose: Exercise","Trip Purpose: Social/Entertainment or Shopping/Errands","Trip Purpose: Transportation Access or Other")
stargazer(final,type = "html",out = paste0(modelOutDir,"propModel.html"),single.row = TRUE,dep.var.labels = "Route Comfort Rating")
stargazer(stepper,type = "html",out = paste0(modelOutDir,"propModel_no_blminArt.html"),single.row = TRUE,dep.var.labels = "Route Comfort Rating")

tripModel = clm(routeComfort~sp_all+I(sp_all^2)+bb_all+I(bb_all^2)+nb_primArt+I(nb_primArt^2)+nb_minArt+I(nb_minArt^2)+nb_res+I(nb_res^2)+matchedDistance+I(matchedDistance^2)+purpose+routeStress_1+routeStress_2+routeStress_3+routeStress_4+routeStress_5+routeStress_6+routeStress_7+routeStress_8,data =tripModelTab,link="logit", weights = tripModelTab$similarWeight)
stepper = step(tripModel,direction="both")
finalProp = clm(routeComfort~sp_all+bb_all+I(bb_all^2)+nb_primArt+I(nb_primArt^2)+bl_minArt+I(bl_minArt^2)+pExercise+pShop+
              routeStress_1+routeStress_2+routeStress_3+routeStress_6,data =tripModelTab,link="logit", weights = tripModelTab$similarWeight)
final = clm(routeComfort~sp_all+bb_all+nb_primArt+bl_minArt+pExercise+pShop+
              routeStress_1+routeStress_2+routeStress_3+routeStress_6,data =tripModelTab,link="logit", weights = tripModelTab$similarWeight)
#varNames = c("Seperated Path (%)","Bicycle Boulevard (%)","No Bike Facility, Primary Arterial (%)","No Bike Facility, Residential (%)","Trip Purpose: Exercise","Trip Purpose: Social/Entertainment or Shopping/Errands","Trip Purpose: Transportation Access or Other")
stargazer(final,type = "html",out = paste0(modelOutDir,"distModel.html"),single.row = TRUE,dep.var.labels = "Route Comfort Rating")

stargazer(finalDist,finalProp,type = "latex",out = paste0(modelOutDir,"dualModel.html"),single.row = TRUE,dep.var.labels = "Route Comfort Rating")
stargazer(stepper,type = "html",out = paste0(modelOutDir,"distModel_no_blminArt.html"),single.row = TRUE,dep.var.labels = "Route Comfort Rating")

#####No Squared Terms

#propModelTab = tripModelTab
#propModelTab[,49:72]=tripModelTab[,49:72]/tripModelTab$matchedDistance
propModel = clm(routeComfort~sp_all+bb_all+nb_primArt+nb_minArt+nb_res+bl_primArt+bl_minArt+bl_res+matchedDistance+purpose+
                  routeStress_1+routeStress_2+routeStress_3+routeStress_4+routeStress_5+routeStress_6+routeStress_7+routeStress_8,data =propModelTab,link="logit", weights = propModelTab$similarWeight)
stepper = step(propModel,direction="both")
final = clm(routeComfort~sp_all+nb_primArt+nb_res+bl_primArt+bl_minArt+bl_res+matchedDistance+pExercise+pShop+
              +routeStress_1+routeStress_2+routeStress_3+routeStress_6,data =propModelTab,link="logit", weights = propModelTab$similarWeight)
#varNames = c("Seperated Path (%)","Bicycle Boulevard (%)","No Bike Facility, Primary Arterial (%)","No Bike Facility, Residential (%)","Trip Purpose: Exercise","Trip Purpose: Social/Entertainment or Shopping/Errands","Trip Purpose: Transportation Access or Other")
stargazer(final,type = "html",out = paste0(modelOutDir,"propModel.html"),single.row = TRUE,dep.var.labels = "Route Comfort Rating")

tripModel = clm(routeComfort~sp_all+bb_all+nb_primArt+nb_minArt+nb_res+bl_primArt+bl_minArt+bl_res+matchedDistance+purpose+
                  routeStress_1+routeStress_2+routeStress_3+routeStress_4+routeStress_5+routeStress_6+routeStress_7+routeStress_8,data =tripModelTab,link="logit", weights = tripModelTab$similarWeight)
stepper = step(tripModel,direction="both")
final = clm(routeComfort~sp_all+nb_primArt+bl_minArt+pExercise+pShop+
              routeStress_1+routeStress_2+routeStress_3+routeStress_6,data =tripModelTab,link="logit", weights = tripModelTab$similarWeight)
#final = clm(routeComfort~sp_all+bb_all+nb_primArt+bl_minArt+pExercise+pShop+
#              routeStress_1+routeStress_2+routeStress_3+routeStress_6,data =tripModelTab,link="logit", weights = tripModelTab$similarWeight)
#varNames = c("Seperated Path (%)","Bicycle Boulevard (%)","No Bike Facility, Primary Arterial (%)","No Bike Facility, Residential (%)","Trip Purpose: Exercise","Trip Purpose: Social/Entertainment or Shopping/Errands","Trip Purpose: Transportation Access or Other")
stargazer(final,type = "html",out = paste0(modelOutDir,"distModel.html"),single.row = TRUE,dep.var.labels = "Route Comfort Rating")




###Summary tables
sumTable = function(df,cols){
  result = data.frame(matrix(nrow=length(cols),ncol=9))
  colnames(result)=c("bikeType","Min","10%","1st Q","Median","Mean","3rd Q","90%","Max")
  result$bikeType=colnames(df)[cols]
  for(i in 1:length(cols)){
    result[i,3]=as.numeric(quantile(df[,cols[i]],0.1))
    result[i,c(2,4:7,9)]=as.numeric(summary(df[,cols[i]]))
    result[i,8]=as.numeric(quantile(df[,cols[i]],0.9))
  }
  return(result)
}

propTab = propModelTab[,c(50:60)]
distTab = tripModelTab[,c(48,50:60)]

theme_set(theme_grey(base_size = 20))

propMelt = melt(propTab)
colnames(propMelt)=c("bikeType","prop")
png(paste0(modelOutDir,"/propBoxplot.png"),width=1200,height=400)
ggplot(propMelt,aes(x=bikeType,y=prop))+geom_boxplot()+scale_y_continuous(limits=c(0,1))+coord_flip()+scale_x_discrete(labels=c(bikeTypes))+xlab("Bicycle Facility Type")+ylab("Trip Distance Proportion")
dev.off()

distMelt = melt(distTab)
colnames(distMelt)=c("bikeType","dist")
png(paste0(modelOutDir,"/distBoxplot.png"),width=1200,height=400)
ggplot(distMelt,aes(x=bikeType,y=dist))+geom_boxplot()+coord_flip()+scale_x_discrete(labels=c("Trip Distance",bikeTypes))+xlab("Bicycle Facility Type")+ylab("Trip Distance")
dev.off()

props = sumTable(propModelTab,c(48,50:60))
write.csv(props,paste0(modelOutDir,"propBikeTable.csv"))
stargazer(props,type="html",out=paste0(modelOutDir,"propBikeTable.html"),summary=FALSE)
dists = sumTable(tripModelTab,c(48,50:60))
write.csv(dists,paste0(modelOutDir,"distBikeTable.csv"))
stargazer(dists,type="html",out=paste0(modelOutDir,"distBikeTable.html"),summary=FALSE)

#####Plots for paper
theme_set(theme_bw(base_size = 30))

png(paste0(modelOutDir,"/purposeDistBoxplot.png"),width=1200,height=400)
ggplot(tripModelTab,aes(x=purpose,y=matchedDistance))+geom_boxplot()+coord_flip()+xlab("Trip Purpose")+ylab("Trip Distance (miles)")
dev.off()





####Comparison of Selected Trips to Google Bike Trips-Shortest Time
gTripTab = readRDS("working_data/googleTripTable.rds")
gTripTab = subset(gTripTab,gTripTab$trip_id %in% tripModelTab$trip_id)

compare = gTripTab
vars = colnames(gTripTab)[3:ncol(gTripTab)]
for (i in 1:length(vars)){
  var = vars[i]
  for (j in 1:length(gTripTab$trip_id)){
    tid = gTripTab$trip_id[j]
    compare[j,var]=tripModelTab[tripModelTab$trip_id==tid,var]-gTripTab[gTripTab$trip_id==tid,var]
  }
}

subTripTab = subset(tripModelTab,tripModelTab$trip_id %in% gTripTab$trip_id)
tTests = data.frame(matrix(nrow = length(vars),ncol=4))
colnames(tTests)=c("variable","diff","tstat","sig")
tTests$variable = vars
for (i in 1:length(vars)){
  if(i %in% 2:25){
    var = vars[i]
    test = t.test(subTripTab[,var]/subTripTab[,"matchedDistance"],gTripTab[,var]/subTripTab[,"matchedDistance"])
    tTests$diff[i] = test$estimate[1]-test$estimate[2]
    tTests$tstat[i]=test$statistic
    tTests$sig[i]=test$p.value
  }else{
    var = vars[i]
    test = t.test(subTripTab[,var],gTripTab[,var])
    tTests$diff[i] = test$estimate[1]-test$estimate[2]
    tTests$tstat[i]=test$statistic
    tTests$sig[i]=test$p.value
  }
  
}

write.csv(tTests,"results/googleCompare.csv")


