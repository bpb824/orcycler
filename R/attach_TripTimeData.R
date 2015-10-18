#' Attach trip time data
#'
#' Attach temporal data to trip summary table
#'
#' @param tripTable Trip summary table
#' @param trips 'trip' table from SQL database
#'
#' @return Trip table with temporal data attached. 
#'
#' @export
#' 

attach_TripTimeData= function(tripTable,trips){
  
  trips$start=as.POSIXct(trips$start, format ="%Y-%m-%d %H:%M:%S")
  trips$stop=as.POSIXct(trips$stop, format ="%Y-%m-%d %H:%M:%S")
  trips$startTime = lubridate::hour(trips$start)+lubridate::minute(trips$start)/60+lubridate::second(trips$start)/3600
  trips$finishTime = lubridate::hour(trips$stop)+lubridate::minute(trips$stop)/60+lubridate::second(trips$stop)/3600
  for (i in 1:nrow(trips)){
    start = trips$startTime[i]
    finish = trips$finishTime[i]
    if(!is.na(start)&!is.na(finish)){
      if (start>=7 & start <=9){
        trips$startCat[i] = "AM_Peak"
      }else if (start>=16.5 & start<=18.5){
        trips$startCat[i] = "PM_Peak"
      }else if (start>9 & start<16.5){
        trips$startCat[i] = "OffPeak_Day"
      }else{
        trips$startCat[i] = "OffPeak_Night"
      }
      
      if (finish>=7 & finish <=9){
        trips$finishCat[i] = "AM_Peak"
      }else if (finish>=16.5 & finish<=18.5){
        trips$finishCat[i] = "PM_Peak"
      }else if (finish>9 & finish<16.5){
        trips$finishCat[i] = "OffPeak_Day"
      }else{
        trips$finishCat[i] = "OffPeak_Night"
      }
    }else{
      trips$startCat[i] = NA
      trips$finishCat[i] = NA
    }
    
  }
  
  trips$startCat = relevel(factor(trips$startCat),ref = "OffPeak_Day")
  trips$finishCat = relevel(factor(trips$finishCat),ref = "OffPeak_Day")
  
  
  ###Categorize Day of week
  trips$dow = weekdays(as.Date(trips$start))
  trips$dowCat[(trips$dow == "Saturday" | trips$dow == "Sunday")]="Weekend"
  trips$dowCat[!(trips$dow == "Saturday" | trips$dow == "Sunday")]="Weekday"
  trips$weekday= FALSE
  trips$weekday[(trips$dowCat == "Weekday")]=TRUE
  trips$holiday = FALSE
  holidays = c("2014-11-11","2014-11-27","2014-11-28","2014-12-24","2014-12-25","2014-12-31","2015-01-01")
  for (i in 1:nrow(trips)){
    day = as.character(strptime(trips$start[i],format = "%Y-%m-%d"))
    if (day %in% holidays){
      trips$holiday[i] = TRUE
    }
  }
  tripTimeData = cbind(trips$id,trips[,20:27])
  colnames(tripTimeData)[1]="trip_id"
  joined = plyr::join(tripTable,tripTimeData,by="trip_id")
  return(joined)
}