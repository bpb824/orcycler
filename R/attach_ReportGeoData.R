#' Attach geographic data to reports
#'
#' @param reportSummary
#' @param notes
#'
#' @return None
#' @export
attach_ReportGeoData = function(reportSummary,notes){

  notes= subset(notes, notes$id %in% reportSummary$note_id)
  points = data.frame(matrix(ncol=3,nrow = nrow(notes)))
  colnames(points) = c("id","X","Y")
  points$id = notes$id
  points$X = notes$longitude
  points$Y = notes$latitude

  #WGS 84 coordinate system
  coordSys=CRS("+init=epsg:4326")

#####Administrative Boundaries

  ###Extract city
  cities = readOGR(dsn = "source_data/gis_data/State\ Level/city_boundaries", layer = "citylim_2014")
  proj4string = cities@proj4string
  cities  = spTransform(cities,CRS('+proj=longlat +datum=NAD83'))
  coordinates(points)=~X+Y
  points@proj4string = CRS("+init=epsg:4326")
  reportSummary$city = "other"

  for (i in 1:nrow(points@data)){
    for (j in 1:nrow(cities@data)){
      point.x = points@coords[i,1]
      point.y = points@coords[i,2]
      poly.x = cities@polygons[[j]]@Polygons[[1]]@coords[,1]
      poly.y = cities@polygons[[j]]@Polygons[[1]]@coords[,2]
      if(point.in.polygon(point.x,point.y,poly.x,poly.y)){
        reportSummary$city[i] = as.character(cities@data$CITY_NAME[j])
        break
      }
    }
  }

  ##Extract County
  counties = readOGR(dsn = "source_data/gis_data/State\ Level/county_boundaries", layer = "orcntypoly")
  proj4string = counties@proj4string
  counties  = spTransform(counties,CRS('+proj=longlat +datum=NAD83'))
  reportSummary$county = "other"

  for (i in 1:nrow(points@data)){
    for (j in 1:nrow(counties@data)){
      point.x = points@coords[i,1]
      point.y = points@coords[i,2]
      poly.x = counties@polygons[[j]]@Polygons[[1]]@coords[,1]
      poly.y = counties@polygons[[j]]@Polygons[[1]]@coords[,2]
      if(point.in.polygon(point.x,point.y,poly.x,poly.y)){
        reportSummary$county[i] = as.character(counties@data$instName[j])
        break
      }
    }
  }

  ###Extract State
  oregon = readOGR(dsn = "source_data/gis_data/State\ Level/or_state_boundary", layer = "or_state_boundary")
  oregon  = spTransform(oregon,CRS('+proj=longlat +datum=NAD83'))
  reportSummary$state = "other"
  oregonBound = oregon@polygons[[28]]@Polygons[[1]]
  poly.x = oregonBound@coords[,1]
  poly.y = oregonBound@coords[,2]

  for (i in 1:nrow(points@data)){
    point.x = points@coords[i,1]
    point.y = points@coords[i,2]
    if(point.in.polygon(point.x,point.y,poly.x,poly.y)){
      reportSummary$state[i] = "Oregon"
    }
  }

#####Link information

  modelNetwork = readOGR("source_data/modelNetwork","wbnet2012")
  buffWidth = 100

  networkHull = gConvexHull(modelNetwork)
  points = spTransform(points, networkHull@proj4string)
  points@proj4string=networkHull@proj4string
  metroPointsBool = !is.na(as.logical(over(points,networkHull,returnList = TRUE)))

  reportSummary$insideNetwork = metroPointsBool

  for (i in 1:nrow(reportSummary)){
    if(reportSummary$insideNetwork[i]){
      nid = reportSummary$note_id[i]
      point = points[i,]
      point = spTransform(point,CRS("+init=epsg:2913"))
      pointBuff = gBuffer(point,width = buffWidth)
      pointBuff = spTransform(pointBuff,modelNetwork@proj4string)
      overTest = which(!is.na(over(as(modelNetwork,"SpatialLines"),pointBuff)))
      linksIn = as.numeric(overTest)
      if(length(linksIn)>1){
        subNetwork = modelNetwork[linksIn,]
        point = spTransform(point, subNetwork@proj4string)
        dists = vector()
        for (j in 1:length(linksIn)){
          dists[j]=gDistance(point,subNetwork[j,])
        }
        nearestLink = subNetwork$psuid[which(min(dists)==dists)[1]]
      }else if (length(linksIn)==1){
        nearestLink=netAtts$psuid[linksIn]
      }else{
        nearestLink=NA
      }

    }else{
      nearestLink=NA
    }
    reportSummary$psuid[i] = nearestLink
    print(paste0("Found nearest link for report # ",i," of ",nrow(reportSummary)))
  }

  netAtts = read.csv("source_data/modelNetwork/networkLink_Attributes.csv")

  reportSummary = join(reportSummary,netAtts,by="psuid")

#
#   #####Check if reports were on ODOT facilities
#   odotHwys = readOGR(dsn ="source_data/gis_data/State\ Level/odot_gis_data", layer = "hwynet2011")
#   proj4string = odotHwys@proj4string
#   odotHwysBuff = gBuffer(odotHwys, width=buffWidth)
#   odotHwysBuffWGS  = spTransform(odotHwysBuff,coordSys)
#   proj4string = proj4string(odotHwysBuffWGS)
#   reportSummary$onODOT = FALSE
#   onODOT <- !is.na(over(as(points,"SpatialPoints"), as(odotHwysBuffWGS, "SpatialPolygons")))
#   reportSummary$onODOT = onODOT
#
#   ######Check numbers of lanes of auto facility adjacent to report
#   numLanes = readOGR(dsn = "source_data/gis_data/State\ Level/odot_gis_data",layer = "number_of_lanes_2011")
#   proj4string = numLanes@proj4string
#   numLanesBuff = gBuffer(numLanes, width=buffWidth, byid = TRUE)
#   numLanesBuffWGS  = spTransform(numLanesBuff,CRS('+proj=longlat +datum=NAD83'))
#   proj4string = proj4string(numLanesBuffWGS)
#   reportSummary$numLanes = NA
#
#   numLanesData = numLanes@data
#
#   for (i in 1:nrow(reportSummary)){
#     point = data.frame(matrix(nrow = 1,ncol = 2))
#     colnames(point)=c("x","y")
#     point$x = points@coords[i,1]
#     point$y = points@coords[i,2]
#     point = SpatialPoints(point,proj4string = CRS('+proj=longlat +datum=NAD83'))
#     id = over(point,as(numLanesBuffWGS,"SpatialPolygons"))
#     reportID = reportSummary$note_id[i]
#     print (paste("Over result for crash #", reportID, " is ", id ))
#     if(!is.na(id)){
#       reportSummary$numLanes[i] = numLanesData$NO_LANES[id]
#     }
#   }
#

  return(reportSummary)
}
