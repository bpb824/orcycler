#' Attach trip weather data
#'
#' Attach weather data to trip summary table
#'
#' @param tripTable Trip summary table
#' @param trips 'trip' table from SQL database
#' @param coords 'coord' table from SQL database
#'
#' @return Trip table with weather data attached. 
#'
#' @export
#' 
attach_TripWeatherData= function (tripTable,trips,coords){
  tripList = trips$id
  n = nrow(trips)
  trips$date = substr(trips$start,1,10)
  trips$date = as.POSIXct(strptime(trips$date,format = "%Y-%m-%d"))
  for (i in 1:nrow(trips)){
    id = trips$id[i]
    tripCoords = subset(coords,coords$trip_id == id)
    num = nrow(tripCoords)
    if (num >=20){
      start = tripCoords[1:10,]
      end = tripCoords[(num-9):num,]
    }else{
      start = tripCoords[1,]
      end = tripCoords[num,]
    }
    trips$firstLat[i] = mean(start$latitude)
    trips$firstLong[i] = mean(start$longitude)
    trips$lastLat[i] = mean(end$latitude)
    trips$lastLong[i] = mean(end$longitude)
    print(paste("calculated for trip number ",i,"of ",n))
  }
  
  trips$start = as.POSIXct(strptime(trips$start,format= "%Y-%m-%d %H:%M:%S"))
  trips$stop = as.POSIXct(strptime(trips$stop,format= "%Y-%m-%d %H:%M:%S"))
  
  require(weatherData)
  airStations = read.csv("data/weatherStations/weatherStations.csv")
  #airStations = read.csv("/Users/orcycle/OneDrive/_BikeAppProject/ORcycle\ Analysis\ Tool\ Suite/source_data/weatherStations/weatherStations.csv")
  tripWeather = data.frame(matrix(nrow=length(tripList),ncol=7))
  colnames(tripWeather) = c("trip_id","temp","windSpeed","windGust","precip","conditions","hasWeather")
  tripWeather$trip_id = tripList
  tripWeather$hasWeather = FALSE
  require(geosphere)
  n =length(tripList)
  for (i in 1:nrow(tripWeather)){
    tid = tripWeather$trip_id[i]
    xyTrip = c(trips$firstLong[i],trips$firstLat[i])
    stationDists = vector()
    for (j in 1:nrow(airStations)){
      stationDists[j] = distMeeus(xyTrip,c(airStations$Long[j],airStations$Lat[j]))
    }
    minCol = which.min(stationDists)
    closest = as.character(airStations$ID[minCol])
    tripDate = as.Date(trips$date[i])
    tripTime = trips$start[i]
    weather = getWeatherForDate(closest,tripDate,opt_detailed=TRUE,opt_all_columns = TRUE)
    if(is.null(weather)){
      tripWeather$temp[i] = NA
      tripWeather$windSpeed[i] = NA
      tripWeather$windGust[i]= NA
      tripWeather$precip[i]= NA
      tripWeather$conditions[i]= NA
    }else{
      tripWeather$hasWeather = TRUE
      weather$timediff = abs(difftime(weather$Time,tripTime))
      current = weather[which.min(weather$timediff),]
      tripWeather$temp[i] = current$TemperatureF
      tripWeather$windSpeed[i] = current$Wind_SpeedMPH
      tripWeather$windGust[i]=current$Gust_SpeedMPH
      tripWeather$precip[i]=current$PrecipitationIn
      tripWeather$conditions[i]=current$Conditions
    }
    
    print(paste("Grabbed weather for trip #",i," of ",n))
  }
  
  for (i in 1:nrow(tripWeather)){
    if (tripWeather$hasWeather[i]){
      ws = tripWeather$windSpeed[i]
      if(ws == "Calm"){
        tripWeather$windSpeed[i] = "0.0"
      }
      wg = tripWeather$windGust[i]
      if(wg == "-"){
        tripWeather$windGust[i] = "0.0"
      }
      pp = tripWeather$precip[i]
      if(pp == "N/A"){
        tripWeather$precip[i] = "0.0"
      }
    }
  }
  
  tripWeather$windSpeed = as.numeric(tripWeather$windSpeed)
  tripWeather$windGust = as.numeric(tripWeather$windGust)
  tripWeather$precip = as.numeric(tripWeather$precip)
  
  tripWeather$condCat[as.character(tripWeather$conditions) == "Overcast" | as.character(tripWeather$conditions) == "Mostly Cloudy"] = "Heavy Clouds"
  tripWeather$condCat[as.character(tripWeather$conditions) == "Partly Cloudy" | as.character(tripWeather$conditions) == "Scattered Clouds"] = "Light Clouds"
  tripWeather$condCat[as.character(tripWeather$conditions) == "Patches of Fog" | as.character(tripWeather$conditions) == "Shallow Fog" | as.character(tripWeather$conditions) == "Fog"] = "Fog"
  tripWeather$condCat[as.character(tripWeather$conditions) == "Light Rain" | as.character(tripWeather$conditions) == "Light Drizzle" ] = "Light Rain"
  tripWeather$condCat[as.character(tripWeather$conditions) == "Rain" | as.character(tripWeather$conditions) == "Heavy Rain"] = "Heavy Rain"
  tripWeather$condCat[as.character(tripWeather$conditions) == "Clear" ] = "Clear"
  tripWeather$condCat = factor(tripWeather$condCat)
  tripWeather$condCat = relevel(tripWeather$condCat, ref = "Clear")
  
  return(join(tripTable,tripWeather,by = "trip_id"))
}