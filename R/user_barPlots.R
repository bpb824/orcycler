#' User response plotting
#'
#' Plots all user response distributions to 'results' folder
#'
#' @param userSummary User summary table outputted from \code{\link{userPrep}}
#' @param outDir Relative or absolute directory destination for resultant plots
#'
#' @return None
#'
#' @export
#' 
user_barPlots= function(userSummary, outDir){
  singleVars = c("age","gender","ethnicity","occupation","income","hhWorkers","hhVehicles",
                 "numBikes","cyclingFreq","cyclingWeather","riderAbility","riderType") 
  singleTitles= c("Age","Gender","Ethnicity","Occupation","Income","Household Workers",
                  "Household Vehicles","Number of Bicycles","Cycling Frequency",
                  "Cycling Weather","Rider Ability","Rider Type")
  
  plotDir = outDir
  numUsers = nrow(userSummary)
  
  for (i in 1:length(singleVars)){
    var = userSummary[,singleVars[i]]
    df = plyr::count(var)
    df$x = as.character(df$x)
    for (j in 1:length(df$x)){
      if (is.na(df$x[j])){
        df$x[j]="No data"
      }
    }
    df$freq = df$freq/sum(df$freq)
    df$x = factor(df$x,levels = as.character(df$x))
    numCats = length(df$x)
    cats = as.character(df$x)
    catsLabs = vector(length= length(cats))
    
    for (j in 1:length(cats)){
      catsLabs[j] = wrap_sentence(cats[j],25)
    }
    

    ggplot2::theme_set(ggplot2::theme_bw(base_size = 25))
    
    png(file = paste(plotDir,vars[i],".png",sep = "") ,width = 1280, height = 100 + numCats *50, units = "px", bg = "transparent")
    print(
      ggplot2::ggplot(df, ggplot2::aes(x =x, y = freq)) + 
        ggplot2::geom_bar(position = "dodge",stat = "identity", fill= "#00759A")+ 
        ggplot2::scale_x_discrete("", labels = catsLabs)+
        ggplot2::ylab("Response frequency among users")+
        ggplot2::ggtitle(paste(singleTitles[i]," (# Users = ",numUsers,")",sep = "")) +
        ggplot2::scale_y_continuous(labels = scales::percent) +ggplot2::coord_flip()+
        ggplot2::theme(axis.title.x=ggplot2::element_text(size = 20,vjust=-1)) + 
        ggplot2::theme(axis.title.y=ggplot2::element_text(size = 20, angle=90, vjust=2)) +
        ggplot2::theme(plot.title=ggplot2::element_text(size=20, vjust=2)) +
        ggplot2::theme(plot.margin = grid::unit(c(0.2,0.2,0.2,0.2), "inches"))
    )
    dev.off()
  }
  
  #Bicycle types
  qid = 10
  qaMap = read.csv("source_data/qaTables/qar1020.csv",stringsAsFactors = FALSE)
  answers =read.csv("source_data/qaTables/answers1020.csv",stringsAsFactors = FALSE)
  ansInds = qaMap[qaMap$question_id==qid,"answer_id"]
  ansText = answers[answers$id %in% ansInds,"text"]
  ansText = ansText[1:length(ansText)-1]
  
  countFrame = data.frame(matrix(nrow = length(ansText),ncol=2))
  colnames(countFrame)=c("answer","count")
  countFrame$answer = ansText
  countFrame$count = 0
  
  bikeResults = userSummary$bikeTypes
  splitResults = strsplit(bikeResults,", ")
  
  for (i in 1:length(splitResults)){
    responses = splitResults[[i]]
    if(sum(is.na(responses))>0){
      countFrame$count[countFrame$answer=="no data"]= countFrame$count[countFrame$answer=="no data"] +1
    }else{
      countFrame$count[countFrame$answer %in% responses]= countFrame$count[countFrame$answer %in% responses] +1
    }
  }
  
  countFrame$freq = countFrame$count/numUsers
  
  countFrame$answer = factor(countFrame$answer, as.character(countFrame$answer))
  numCats = length(countFrame$answer)
  cats = as.character(countFrame$answer)
  catsLabs = vector(length= length(cats))
  
  for (j in 1:length(cats)){
    catsLabs[j] = wrap_sentence(cats[j],25)
  }
  
  gg =ggplot2::ggplot(countFrame, ggplot2::aes(x =answer, y = freq)) + 
    ggplot2::geom_bar(position = "dodge",stat = "identity", fill= "#00759A")+ 
    ggplot2::scale_x_discrete("", labels = catsLabs)+
    ggplot2::ylab("Response frequency among users")+
    ggplot2::ggtitle(paste("Bicycle Types"," (# Users = ",numUsers,")",sep = "")) +
    ggplot2::scale_y_continuous(labels = scales::percent) +ggplot2::coord_flip()+
    ggplot2::theme(axis.title.x=ggplot2::element_text(size = 20,vjust=-1)) + 
    ggplot2::theme(axis.title.y=ggplot2::element_text(size = 20, angle=90, vjust=2)) +
    ggplot2::theme(plot.title=ggplot2::element_text(size=20, vjust=2)) +
    ggplot2::theme(plot.margin = grid::unit(c(0.2,0.2,0.2,0.2), "inches"))
  
  png(file = paste(plotDir,"bikeTypes.png",sep = "") ,width = 1280, height = 100 + numCats *50, units = "px", bg = "transparent")
  print(gg)
  dev.off()
  
  
  
}