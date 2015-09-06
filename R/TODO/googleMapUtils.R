library(maptools)
library(maps)
library(mapdata)
library(rjson)
library(stringr)
library(RCurl)
library(ggplot2)


GetDirections <- function(from, to) {
  Sys.sleep(1) #let's be nice internet citizens and wait 1s
  baseurl <- "https://maps.googleapis.com/maps/api/directions/json?origin="
  url = paste0(baseurl,from,"&destination=",to,"&mode=bicycling&key=AIzaSyA7nv6LHZ5KtPBrNLy4qTRN-ejXGrKT3vc")
  fromJSON(paste(readLines(url), collapse=""))
}

#Google polyline decoder borrowed from:
#http://facstaff.unca.edu/mcmcclur/GoogleMaps/EncodePolyline/decode.js
DecodeLineR <- function(encoded) {
  len = str_length(encoded)
  encoded <- strsplit(encoded, NULL)[[1]]
  index = 1
  N <- 100000
  df.index <- 1
  array = matrix(nrow = N, ncol = 2)
  lat = 0
  dlat = 0
  lng = 0
  dlng = 0
  b = 0
  shift = 0
  result = 0
  
  while(index <= len) {
    shift = 0
    result = 0
    
    repeat {
      b = as.integer(charToRaw(encoded[index])) - 63
      index <- index + 1
      result = bitOr(result, bitShiftL(bitAnd(b, 0x1f), shift))
      shift = shift + 5
      if(b < 0x20) break
    }
    dlat = ifelse(bitAnd(result, 1),
                  -(result - (bitShiftR(result, 1))),
                  bitShiftR(result, 1))
    lat = lat + dlat;
    
    shift = 0
    result = 0
    b = 0
    repeat {
      b = as.integer(charToRaw(encoded[index])) - 63
      index <- index + 1
      result = bitOr(result, bitShiftL(bitAnd(b, 0x1f), shift))
      shift = shift + 5
      if(b < 0x20) break
    }
    dlng = ifelse(bitAnd(result, 1),
                  -(result - (bitShiftR(result, 1))),
                  bitShiftR(result, 1))
    lng = lng + dlng
    
    array[df.index,] <- c(lat = lat * 1e-05, lng = lng * 1e-5)
    df.index <- df.index + 1
  }
  
  ret <- data.frame(array[1:df.index - 1,])
  names(ret) <- c("lat", "lng")
  return(ret)
}

src <- '
std::string encoded = as<std::string>(a);

int index = 0;
int len = encoded.size();
int df_index = 0;
long double lat = 0;
long double lng = 0;
std::vector<long double> longitude(0);
std::vector<long double> latitude(0);

if(encoded.size() == 0)
return R_NilValue;

longitude.reserve(30000);
latitude.reserve(30000);

while(index < len) {
int b;
int shift = 0;
int result = 0;

do {
b = encoded[index++] - 63;
result |= (b & 0x1f) << shift;
shift += 5;
} while(b >= 0x20);
long double dlat = ((result & 1) ? ~(result >> 1) : (result >> 1));
lat += dlat;
latitude.push_back(lat * 1e-5);

shift = 0;
result = 0;
do {
b = encoded[index++] - 63;
result |= (b & 0x1f) << shift;
shift += 5;
} while(b >= 0x20);
long double dlng = ((result & 1) ? ~(result >> 1) : (result >> 1));
lng += dlng;
longitude.push_back(lng * 1e-5);
df_index++;
}

return DataFrame::create( _["lat"] = latitude,  _["lng"] = longitude );
'


if (require("Rcpp") & require("inline")){
  DecodeLine <- cxxfunction(signature(a = "character"),
                            src, plugin = "Rcpp")
} else {
  DecodeLine <- DecodeLineR
}

# #http://code.google.com/apis/maps/documentation/utilities/polylinealgorithm.html
# poly.official <- data.frame(lat = c(38.5, 40.7, 43.252),
#                             lng = c(-120.2, -120.95, -126.453))
# 
# all.equal(DecodeLine("_p~iF~ps|U_ulLnnqC_mqNvxq`@"),
#           poly.official)
# all.equal(DecodeLineR("_p~iF~ps|U_ulLnnqC_mqNvxq`@"),
#           poly.official)
# 
# identical(DecodeLine("_p~iF~ps|U_ulLnnqC_mqNvxq`@"),
#           DecodeLineR("_p~iF~ps|U_ulLnnqC_mqNvxq`@"))

route2Shape = function(directions){
  route = directions$routes[[1]]
  steps = route$legs[[1]]$steps
  poly = DecodeLine(steps[[1]]$polyline$points)
  polyLine = Lines(Line(cbind(poly$lng,poly$lat)),1)
  lineList = list()
  lineList[[1]]=polyLine
  if (length(steps)>1){
    for (i in 2:length(steps)){
      poly = DecodeLine(steps[[i]]$polyline$points)
      polyLine = Lines(Line(cbind(poly$lng,poly$lat)),i)
      lineList[[i]]=polyLine
    }
  }
  polyShape = SpatialLines(lineList,CRS("+init=epsg:4326"))
  return(polyShape)
}



# library(compiler)
# DecodeLineRC <- cmpfun(DecodeLineR)
# 
# library(rbenchmark)
# 
# benchmark(DecodeLine("_p~iF~ps|U_ulLnnqC_mqNvxq`@"),
#           DecodeLineR("_p~iF~ps|U_ulLnnqC_mqNvxq`@"),
#           DecodeLineRC("_p~iF~ps|U_ulLnnqC_mqNvxq`@"),
#           columns=c("test", "replications", "elapsed", "relative"),
#           order="relative", replications=1000)
# 
# 
# #from Acapulco to Reynosa
# dir <- GetDirections(from = "Acapulco, Mexico",
#                      to = c("Mexico city, Mexico",
#                             "Juarez, Chihuahua, Mexico"))
# dir$Directions$Duration$html
# dir$Directions$Distance$meters
# 
# #Converting the encoded polyline to a data.frame takes a really long time
# system.time(dir$Decodedline <- DecodeLine(dir$Directions$Polyline$points))
# #The R version is slow
# system.time(dir$Decodedline <- DecodeLineR(dir$Directions$Polyline$points))
# 
# #Draw a map with the optimal highway
# mexico <- data.frame(map("worldHires", "mexico", plot=FALSE)[c("x","y")])
# mexico.map <- qplot(x, y, data=mexico, geom="path") + coord_map()
# mexico.map + geom_path(data = dir$Decodedline, aes(lng, lat))