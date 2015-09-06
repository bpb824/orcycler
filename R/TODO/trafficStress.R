#######
# Level of Traffic Stress calculation script
# Written by Bryan Blanc
#######

##The following logical and mathematical calculations were based upon the 
# "Low Stress Bicycling and Network Connectivity" report developed by Mineta Transportation Institute
# http://transweb.sjsu.edu/PDFs/research/1005-low-stress-bicycling-network-connectivity.pdf
# and the Analysis Procedures Manual developed by ODOT: http://www.oregon.gov/ODOT/TD/TP/APM/APMv2.pdf


trafficStress = function(reportSummary){
  
  ##Constants 
  mixedTraffic = c("NONE", "Bicycles Prohibited", "Bicycle Boulevard", "Other Connection")
  sepFacilities = c("Separated Path", "Cycletrack")
  
  ####Segments#######
  #segReports = subset(reportSummary,reportSummary$nearIntersect==TRUE)
  segReports = reportSummary
  
  for (i in 1:nrow(segReports)){
    if(!is.na(segReports$speed[i]) & !is.na(segReports$numLanesAuto[i])){
      ##Bike Lanes adjacent to parking####
      if(as.character(segReports$bikeTypeLong[i]) == "Bike Lane"  &
         segReports$adjParking[i]==TRUE){
        if(segReports$numLanesAuto[i]/2<2){
          if(segReports$speed[i]<=25){
            segReports$LTS[i] = 2
          }else if(segReports$speed[i]>25 & segReports$speed[i]<40){
            segReports$LTS[i] = 3
          }else if(segReports$speed[i]>=40){
            segReports$LTS[i] = 4
          }
        }else{
          if(segReports$speed[i]<=25){
            segReports$LTS[i] = 3
          }else if(segReports$speed[i]>25 & segReports$speed[i]<40){
            segReports$LTS[i] = 3
          }else if(segReports$speed[i]>=40){
            segReports$LTS[i] = 4
          }
        }
      }else if(as.character(segReports$bikeTypeLong[i]) == "Buffered Bike Lane"  &
               segReports$adjParking[i]==TRUE){
        if(segReports$numLanesAuto[i]/2<2){
          if(segReports$speed[i]<=25){
            segReports$LTS[i] = 1
          }else if(segReports$speed[i]>25 & segReports$speed[i]<40){
            segReports$LTS[i] = 2
          }else if(segReports$speed[i]>=40){
            segReports$LTS[i] = 2
          }
        }else{
          if(segReports$speed[i]<=25){
            segReports$LTS[i] = 2
          }else if(segReports$speed[i]>25 & segReports$speed[i]<40){
            segReports$LTS[i] = 2
          }else if(segReports$speed[i]>=40){
            segReports$LTS[i] = 3
          }
        }
      }
      
      ##Bike Lanes not adjacent to parking###
      else if(as.character(segReports$bikeTypeLong[i]) == "Bike Lane"  &
              segReports$adjParking[i]==FALSE){
        if(segReports$numLanesAuto[i]/2<2){
          if(segReports$speed[i]<=30){
            segReports$LTS[i] = 1
          }else if(segReports$speed[i]>30 & segReports$speed[i]<40){
            segReports$LTS[i] = 3
          }else if(segReports$speed[i]>=40){
            segReports$LTS[i] = 4
          }
        }else{
          if(segReports$speed[i]<=30){
            segReports$LTS[i] = 3
          }else if(segReports$speed[i]>30 & segReports$speed[i]<40){
            segReports$LTS[i] = 3
          }else if(segReports$speed[i]>=40){
            segReports$LTS[i] = 4
          }
        }
      }else if(as.character(segReports$bikeTypeLong[i]) == "Buffered Bike Lane"  &
               segReports$adjParking[i]==FALSE){
        if(segReports$numLanesAuto[i]/2<2){
          if(segReports$speed[i]<=30){
            segReports$LTS[i] = 1
          }else if(segReports$speed[i]>30 & segReports$speed[i]<40){
            segReports$LTS[i] = 2
          }else if(segReports$speed[i]>=40){
            segReports$LTS[i] = 3
          }
        }else{
          if(segReports$speed[i]<=25){
            segReports$LTS[i] = 1
          }else if(segReports$speed[i]>25 & segReports$speed[i]<40){
            segReports$LTS[i] = 2
          }else if(segReports$speed[i]>=40){
            segReports$LTS[i] = 3
          }
        }
      }
      
      ##Mixed Traffic Situation####
      else if(as.character(segReports$bikeTypeLong[i]) %in% mixedTraffic){
        if(segReports$centerLine[i]==FALSE){
          if(segReports$speed[i]<=25){
            segReports$LTS[i] = 1
          }else if(segReports$speed[i]>25 & segReports$speed[i]<35){
            segReports$LTS[i] = 2
          }else if(segReports$speed[i]>=35){
            segReports$LTS[i] = 3
          }
        }else{
          if(segReports$numLanesAuto[i]/2<2){
            if(segReports$speed[i]<=25){
              segReports$LTS[i] = 2
            }else if(segReports$speed[i]>25 & segReports$speed[i]<35){
              segReports$LTS[i] = 3
            }else if(segReports$speed[i]>=35){
              segReports$LTS[i] = 4
            }
          }else if (segReports$numLanesAuto[i]/2 >=2 & segReports$numLanesAuto[i]/2 <3){
            if(segReports$speed[i]<=25){
              segReports$LTS[i] = 3
            }else if(segReports$speed[i]>25 & segReports$speed[i]<35){
              segReports$LTS[i] = 4
            }else if(segReports$speed[i]>=35){
              segReports$LTS[i] = 4
            }
          }else if (segReports$numLanesAuto[i]/2 >=3){
            if(segReports$speed[i]<=25){
              segReports$LTS[i] = 4
            }else if(segReports$speed[i]>25 & segReports$speed[i]<35){
              segReports$LTS[i] = 4
            }else if(segReports$speed[i]>=35){
              segReports$LTS[i] = 4
            }
          }
        }
      }
      
      ##Other###
      else if(as.character(segReports$bikeTypeLong[i]) %in% sepFacilities){
        segReports$LTS[i] = 1
      }
    }else{
      segReports$LTS[i]= NA
    }
  }
  ####Intersections#####
    
  return(segReports)
}

