#' Trip response plotting
#'
#' Plots all trip response distributions to 'results' folder
#'
#' @param tripSummary Trip summary table outputted from \code{\link{tripPrep}}
#'
#' @return None
#'
#' @export
#' 

trip_barPlots= function(tripSummary){
  require(plyr)
  require(ggplot2)
  require(grid)
  
  #source("functions/wrap_sentance.R")
  numTrips = nrow(tripSummary)
  
  singleVars = c("routeFreq","purpose","routeComfort")
  
  for (i in 1:length(singleVars)){
    var = tripSummary[,singleVars[i]]
    df = count(var)
    df$x = as.character(df$x)
    for (j in 1:length(df$x)){
      if (is.na(df$x[j])){
        df$x[j]="No data"
      }
    }
    df$freq = df$freq/sum(df$freq)
    range = c(0,ceiling(range(df$freq)*10)[2]/10)
    nums = seq(range[1],range[2],0.1)
    labs = seq(range[1],range[2],0.1)*100
    for(j in 1:length(labs)){
      labs[j]= paste(as.character(labs[j]),"%",sep = "")
    }
    order = order(as.character(df$x))
    df$x = factor(df$x, as.character(df$x))
    numCats = length(df$x)
    cats = as.character(df$x)
    catsLabs = vector(length= length(cats))
    
    for (j in 1:length(cats)){
      catsLabs[j] = wrap_sentence(cats[j],25)
    }
    
    titles = c("Route Frequency","Trip Purpose","Route Comfort")
    
    plotDir = "results/plots_singleVar/trips/"
    #theme_set(theme_bw(base_size = 25))
    
    png(file = paste(plotDir,singleVars[i],".png",sep = "") ,width = 1280, height = 100 + numCats *50, units = "px", bg = "transparent")
    print(
      ggplot(df, aes(x =x, y = freq, width =0.9)) + geom_bar(position = "dodge",stat = "identity", fill= "#00759A")+ scale_x_discrete("", labels = catsLabs)+ylab("Response frequency among trips")+ggtitle(paste(titles[i]," (# trips = ",numTrips,")",sep = "")) +scale_y_continuous(breaks = nums, labels = labs) +coord_flip()+
        theme(axis.title.x=element_text(size = 20,vjust=-1)) + 
        theme(axis.title.y=element_text(size = 20, angle=90, vjust=2)) +
        theme(plot.title=element_text(size=20, vjust=2)) +
        theme(plot.margin = unit(c(0.2,0.2,0.2,0.2), "inches"))
    )
    dev.off()
  }
  
  qid = 21
  qaMap = read.csv("qaTables/qar1020.csv",stringsAsFactors = FALSE)
  answers =read.csv("qaTables/answers1020.csv",stringsAsFactors = FALSE)
  ansInds = qaMap[qaMap$question_id==qid,"answer_id"]
  ansText = answers[answers$id %in% ansInds,"text"]
  ansText = ansText[1:length(ansText)-1]
  ###Route Preferences
  df = as.data.frame(colSums(tripSummary[,17:29])/numTrips)
  colnames(df)="freq"
  df$x = ansText
  
  range = c(0,ceiling(range(df$freq)*10)[2]/10)
  nums = seq(range[1],range[2],0.1)
  labs = seq(range[1],range[2],0.1)*100
  for(j in 1:length(labs)){
    labs[j]= paste(as.character(labs[j]),"%",sep = "")
  }
  order = order(as.character(df$x))
  df$x = factor(df$x, as.character(df$x))
  numCats = length(df$x)
  cats = as.character(df$x)
  catsLabs = vector(length= length(cats))
  
  for (j in 1:length(cats)){
    catsLabs[j] = wrap_sentence(cats[j],25)
  }
  
  plotDir = "results/plots_singleVar/trips/"
  #theme_set(theme_bw(base_size = 25))
  
  png(file = paste(plotDir,"routePrefs",".png",sep = "") ,width = 1280, height = 100 + numCats *50, units = "px", bg = "transparent")
  print(
    ggplot(df, aes(x =x, y = freq, width =0.9)) + geom_bar(position = "dodge",stat = "identity", fill= "#00759A")+ scale_x_discrete("", labels = catsLabs)+ylab("Response frequency among trips")+ggtitle(paste("Route Preferences"," (# trips = ",numTrips,")",sep = "")) +scale_y_continuous(breaks = nums, labels = labs) +coord_flip()+
      theme(axis.title.x=element_text(size = 20,vjust=-1)) + 
      theme(axis.title.y=element_text(size = 20, angle=90, vjust=2)) +
      theme(plot.title=element_text(size=20, vjust=2)) +
      theme(plot.margin = unit(c(0.2,0.2,0.2,0.2), "inches"))
  )
  dev.off()
  
  qid = 27
  ansInds = qaMap[qaMap$question_id==qid,"answer_id"]
  ansText = answers[answers$id %in% ansInds,"text"]
  ###Route Stressors
  df = as.data.frame(colSums(tripSummary[,8:16])/numTrips)
  colnames(df)="freq"
  df$x = c("No Data",ansText)
  
  range = c(0,ceiling(range(df$freq)*10)[2]/10)
  nums = seq(range[1],range[2],0.1)
  labs = seq(range[1],range[2],0.1)*100
  for(j in 1:length(labs)){
    labs[j]= paste(as.character(labs[j]),"%",sep = "")
  }
  order = order(as.character(df$x))
  df$x = factor(df$x, as.character(df$x))
  numCats = length(df$x)
  cats = as.character(df$x)
  catsLabs = vector(length= length(cats))
  
  for (j in 1:length(cats)){
    catsLabs[j] = wrap_sentence(cats[j],25)
  }
  
  plotDir = "results/plots_singleVar/trips/"
  #theme_set(theme_bw(base_size = 25))
  
  png(file = paste(plotDir,"routeStressors",".png",sep = "") ,width = 1280, height = 100 + numCats *50, units = "px", bg = "transparent")
  print(
    ggplot(df, aes(x =x, y = freq, width =0.9)) + geom_bar(position = "dodge",stat = "identity", fill= "#00759A")+ scale_x_discrete("", labels = catsLabs)+ylab("Response frequency among trips")+ggtitle(paste("Route Stressors"," (# trips = ",numTrips,")",sep = "")) +scale_y_continuous(breaks = nums, labels = labs) +coord_flip()+
      theme(axis.title.x=element_text(size = 20,vjust=-1)) + 
      theme(axis.title.y=element_text(size = 20, angle=90, vjust=2)) +
      theme(plot.title=element_text(size=20, vjust=2)) +
      theme(plot.margin = unit(c(0.2,0.2,0.2,0.2), "inches"))
  )
  dev.off()
}