#' Trip response plotting
#'
#' Plots all trip response distributions to 'results' folder
#'
#' @param tripSummary Trip summary table outputted from \code{\link{tripPrep}}
#' @param outDir Relative or absolute directory destination for resultant plots
#'
#' @return None
#'
#' @export
#'

trip_barPlots= function(tripSummary,outDir){
  numTrips = nrow(tripSummary)
  plotDir = outDir

  singleVars = c("routeFreq","purpose","routeComfort")

  for (i in 1:length(singleVars)){
    var = tripSummary[,singleVars[i]]
    df = plyr::count(var)
    df$x = as.character(df$x)
    for (j in 1:length(df$x)){
      if (is.na(df$x[j])){
        df$x[j]="No data"
      }
    }
    df$freq = df$freq/sum(df$freq)
    df$x = factor(df$x, as.character(df$x))
    numCats = length(df$x)
    cats = as.character(df$x)
    catsLabs = vector(length= length(cats))

    for (j in 1:length(cats)){
      catsLabs[j] = wrap_sentence(cats[j],25)
    }

    titles = c("Route Frequency","Trip Purpose","Route Comfort")

    png(file = paste(plotDir,singleVars[i],".png",sep = "") ,width = 1280, height = 100 + numCats *50, units = "px", bg = "transparent")
    print(
      ggplot2::ggplot(df, ggplot2::aes(x =x, y = freq, width =0.9)) +
        ggplot2::geom_bar(position = "dodge",stat = "identity", fill= "#00759A")+
        ggplot2::scale_x_discrete("", labels = catsLabs)+
        ggplot2::ylab("Response frequency among trips")+
        ggplot2::ggtitle(paste(titles[i]," (# trips = ",numTrips,")",sep = "")) +
        ggplot2::scale_y_continuous(labels = scales::percent) +ggplot2::coord_flip()+
        ggplot2::theme(axis.title.x=ggplot2::element_text(size = 20,vjust=-1)) +
        ggplot2::theme(axis.title.y=ggplot2::element_text(size = 20, angle=90, vjust=2)) +
        ggplot2::theme(plot.title=ggplot2::element_text(size=20, vjust=2)) +
        ggplot2::theme(plot.margin = grid::unit(c(0.2,0.2,0.2,0.2), "inches"))
    )
    dev.off()
  }

  qid = 21
  qaMap = read.csv("source_data/qaTables/qar1020.csv",stringsAsFactors = FALSE)
  answers =read.csv("source_data/qaTables/answers1020.csv",stringsAsFactors = FALSE)
  ansInds = qaMap[qaMap$question_id==qid,"answer_id"]
  ansText = answers[answers$id %in% ansInds,"text"]
  ansText = ansText[1:length(ansText)-1]

  ###Route Preferences
  df = as.data.frame(colSums(tripSummary[,17:29])/numTrips)
  colnames(df)="freq"
  df$x = ansText
  df$x = factor(df$x, as.character(df$x))
  numCats = length(df$x)
  cats = as.character(df$x)
  catsLabs = vector(length= length(cats))

  for (j in 1:length(cats)){
    catsLabs[j] = wrap_sentence(cats[j],25)
  }

  png(file = paste(plotDir,"routePrefs",".png",sep = "") ,width = 1280, height = 100 + numCats *50, units = "px", bg = "transparent")
  print(
    ggplot2::ggplot(df, ggplot2::aes(x =x, y = freq, width =0.9)) + ggplot2::geom_bar(position = "dodge",stat = "identity", fill= "#00759A")+
      ggplot2::scale_x_discrete("", labels = catsLabs)+ggplot2::ylab("Response frequency among trips")+
      ggplot2::ggtitle(paste("Route Preferences"," (# trips = ",numTrips,")",sep = "")) +
      ggplot2::scale_y_continuous(labels = scales::percent) +ggplot2::coord_flip()+
      ggplot2::theme(axis.title.x=ggplot2::element_text(size = 20,vjust=-1)) +
      ggplot2::theme(axis.title.y=ggplot2::element_text(size = 20, angle=90, vjust=2)) +
      ggplot2::theme(plot.title=ggplot2::element_text(size=20, vjust=2)) +
      ggplot2::theme(plot.margin = grid::unit(c(0.2,0.2,0.2,0.2), "inches"))
  )
  dev.off()

  qid = 27
  ansInds = qaMap[qaMap$question_id==qid,"answer_id"]
  ansText = answers[answers$id %in% ansInds,"text"]
  ###Route Stressors
  df = as.data.frame(colSums(tripSummary[,8:16])/numTrips)
  colnames(df)="freq"
  df$x = c("No Data",ansText)
  df$x = factor(df$x, as.character(df$x))
  numCats = length(df$x)
  cats = as.character(df$x)
  catsLabs = vector(length= length(cats))

  for (j in 1:length(cats)){
    catsLabs[j] = wrap_sentence(cats[j],25)
  }

  png(file = paste(plotDir,"routeStressors",".png",sep = "") ,width = 1280, height = 100 + numCats *50, units = "px", bg = "transparent")
  print(
    ggplot2::ggplot(df, ggplot2::aes(x =x, y = freq, width =0.9)) +
      ggplot2::geom_bar(position = "dodge",stat = "identity", fill= "#00759A")+
      ggplot2::scale_x_discrete("", labels = catsLabs)+ggplot2::ylab("Response frequency among trips")+
      ggplot2::ggtitle(paste("Route Stressors"," (# trips = ",numTrips,")",sep = "")) +
      ggplot2::scale_y_continuous(labels = scales::percent) +ggplot2::coord_flip()+
      ggplot2::theme(axis.title.x=ggplot2::element_text(size = 20,vjust=-1)) +
      ggplot2::theme(axis.title.y=ggplot2::element_text(size = 20, angle=90, vjust=2)) +
      ggplot2::theme(plot.title=ggplot2::element_text(size=20, vjust=2)) +
      ggplot2::theme(plot.margin = grid::unit(c(0.2,0.2,0.2,0.2), "inches"))
  )
  dev.off()
}
