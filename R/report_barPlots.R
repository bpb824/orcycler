#' Plot distribution plots for report variables
#'
#' @param reportSummary Prepared report data summary
#' @param reportType Type of report to produce plots for (options are 0 or 1)
#'
#' @return None
#' @export
report_barPlots= function(reportSummary,reportType,outDir){
  
  plotDir = outDir
  ggplot2::theme_set(ggplot2::theme_bw(base_size = 25))
  
  qaMap = read.csv("source_data/qaTables/qar1020.csv",stringsAsFactors = FALSE)
  answers =read.csv("source_data/qaTables/answers1020.csv",stringsAsFactors = FALSE)
  
  if(reportType==0){
    numReports = nrow(reportSummary)

    #Severity
    var = reportSummary[,"severity"]
    df = as.data.frame(table(var))
    colnames(df)=c("x","freq")
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

    png(file = paste(plotDir,"severity",".png",sep = "") ,width = 1280, height = 100 + numCats *50, units = "px", bg = "transparent")
    print(
      ggplot2::ggplot(df, ggplot2::aes(x =x, y = freq)) + 
        ggplot2::geom_bar(position = "dodge",stat = "identity", fill= "#00759A")+ 
        ggplot2::scale_x_discrete("", labels = catsLabs)+
        ggplot2::ylab("Response frequency among crash event reports")+
        ggplot2::ggtitle(paste("Crash Event Severity"," (# reports = ",numReports,")",sep = "")) +
        ggplot2::scale_y_continuous(labels = scales::percent) +
        ggplot2::coord_flip()+
        ggplot2::theme(axis.title.x=ggplot2::element_text(size = 20,vjust=-1)) +
        ggplot2::theme(axis.title.y=ggplot2::element_text(size = 20, angle=90, vjust=2)) +
        ggplot2::theme(plot.title=ggplot2::element_text(size=20, vjust=2)) +
        ggplot2::theme(plot.margin = grid::unit(c(0.2,0.2,0.2,0.2), "inches"))
    )
    dev.off()

    #Conflicts
    qid = 29
    ansInds = qaMap[qaMap$question_id==qid,"answer_id"]
    ansText = c("No Data",answers[answers$id %in% ansInds,"text"])
    df = as.data.frame(colSums(reportSummary[,3:13])/numReports)
    colnames(df)="freq"
    df$x = ansText
    df$x = factor(df$x, as.character(df$x))
    numCats = length(df$x)
    cats = as.character(df$x)
    catsLabs = vector(length= length(cats))

    for (j in 1:length(cats)){
      catsLabs[j] = wrap_sentence(cats[j],25)
    }

    png(file = paste(plotDir,"conflictWith",".png",sep = "") ,width = 1280, height = 100 + numCats *50, units = "px", bg = "transparent")
    print(
      ggplot2::ggplot(df, ggplot2::aes(x =x, y = freq)) + 
        ggplot2::geom_bar(position = "dodge",stat = "identity", fill= "#00759A")+ 
        ggplot2::scale_x_discrete("", labels = catsLabs)+
        ggplot2::ylab("Response frequency among crash event reports")+
        ggplot2::ggtitle(paste("Conflict Type"," (# reports = ",numReports,")",sep = "")) +
        ggplot2::scale_y_continuous(labels = scales::percent) +
        ggplot2::coord_flip()+
        ggplot2::theme(axis.title.x=ggplot2::element_text(size = 20,vjust=-1)) +
        ggplot2::theme(axis.title.y=ggplot2::element_text(size = 20, angle=90, vjust=2)) +
        ggplot2::theme(plot.title=ggplot2::element_text(size=20, vjust=2)) +
        ggplot2::theme(plot.margin = grid::unit(c(0.2,0.2,0.2,0.2), "inches"))
    )
    dev.off()

    #Actions
    qid = 32
    ansInds = qaMap[qaMap$question_id==qid,"answer_id"]
    ansText = c("No Data",answers[answers$id %in% ansInds,"text"])
    df = as.data.frame(colSums(reportSummary[,14:24])/numReports)
    colnames(df)="freq"
    df$x = ansText
    df$x = factor(df$x, as.character(df$x))
    numCats = length(df$x)
    cats = as.character(df$x)
    catsLabs = vector(length= length(cats))

    for (j in 1:length(cats)){
      catsLabs[j] = wrap_sentence(cats[j],25)
    }

    png(file = paste(plotDir,"crashActions",".png",sep = "") ,width = 1280, height = 100 + numCats *50, units = "px", bg = "transparent")
    print(
      ggplot2::ggplot(df, ggplot2::aes(x =x, y = freq)) + 
        ggplot2::geom_bar(position = "dodge",stat = "identity", fill= "#00759A")+ 
        ggplot2::scale_x_discrete("", labels = catsLabs)+
        ggplot2::ylab("Response frequency among crash event reports")+
        ggplot2::ggtitle(paste("Crash Event Action"," (# reports = ",numReports,")",sep = "")) +
        ggplot2::scale_y_continuous(labels = scales::percent) +
        ggplot2::coord_flip()+
        ggplot2::theme(axis.title.x=ggplot2::element_text(size = 20,vjust=-1)) +
        ggplot2::theme(axis.title.y=ggplot2::element_text(size = 20, angle=90, vjust=2)) +
        ggplot2::theme(plot.title=ggplot2::element_text(size=20, vjust=2)) +
        ggplot2::theme(plot.margin = grid::unit(c(0.2,0.2,0.2,0.2), "inches"))
    )
    dev.off()

    #Reasons
    qid = 33
    ansInds = qaMap[qaMap$question_id==qid,"answer_id"]
    ansText = c("No Data",answers[answers$id %in% ansInds,"text"])
    df = as.data.frame(colSums(reportSummary[,25:36])/numReports)
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

    png(file = paste(plotDir,"crashReasons",".png",sep = "") ,width = 1280, height = 100 + numCats *50, units = "px", bg = "transparent")
    print(
      ggplot2::ggplot(df, ggplot2::aes(x =x, y = freq)) + 
        ggplot2::geom_bar(position = "dodge",stat = "identity", fill= "#00759A")+ 
        ggplot2::scale_x_discrete("", labels = catsLabs)+
        ggplot2::ylab("Response frequency among crash event reports")+
        ggplot2::ggtitle(paste("Crash Event Reason"," (# reports = ",numReports,")",sep = "")) +
        ggplot2::scale_y_continuous(labels = scales::percent) +
        ggplot2::coord_flip()+
        ggplot2::theme(axis.title.x=ggplot2::element_text(size = 20,vjust=-1)) +
        ggplot2::theme(axis.title.y=ggplot2::element_text(size = 20, angle=90, vjust=2)) +
        ggplot2::theme(plot.title=ggplot2::element_text(size=20, vjust=2)) +
        ggplot2::theme(plot.margin = grid::unit(c(0.2,0.2,0.2,0.2), "inches"))
    )
    dev.off()

  }else if(reportType ==1){
    numReports = nrow(reportSummary)

    #Urgency
    var = reportSummary[,"urgency"]
    df = as.data.frame(table(var))
    colnames(df)=c("x","freq")
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

    png(file = paste(plotDir,"urgency",".png",sep = "") ,width = 1280, height = 100 + numCats *50, units = "px", bg = "transparent")
    print(
      ggplot2::ggplot(df, ggplot2::aes(x =x, y = freq)) + 
        ggplot2::geom_bar(position = "dodge",stat = "identity", fill= "#00759A")+ 
        ggplot2::scale_x_discrete("", labels = catsLabs)+
        ggplot2::ylab("Response frequency among safety issue reports")+
        ggplot2::ggtitle(paste("Safety Issue Urgency"," (# reports = ",numReports,")",sep = "")) +
        ggplot2::scale_y_continuous(labels = scales::percent) +
        ggplot2::coord_flip()+
        ggplot2::theme(axis.title.x=ggplot2::element_text(size = 20,vjust=-1)) +
        ggplot2::theme(axis.title.y=ggplot2::element_text(size = 20, angle=90, vjust=2)) +
        ggplot2::theme(plot.title=ggplot2::element_text(size=20, vjust=2)) +
        ggplot2::theme(plot.margin = grid::unit(c(0.2,0.2,0.2,0.2), "inches"))
    )
    dev.off()

    #Conflicts
    qid = 30
    ansInds = qaMap[qaMap$question_id==qid,"answer_id"]
    ansText = c("No Data",answers[answers$id %in% ansInds,"text"])
    df = as.data.frame(colSums(reportSummary[,3:17])/numReports)
    colnames(df)="freq"
    df$x = ansText
    df$x = factor(df$x, as.character(df$x))
    numCats = length(df$x)
    cats = as.character(df$x)
    catsLabs = vector(length= length(cats))

    for (j in 1:length(cats)){
      catsLabs[j] = wrap_sentence(cats[j],25)
    }
    
    png(file = paste(plotDir,"issueType",".png",sep = "") ,width = 1280, height = 100 + numCats *50, units = "px", bg = "transparent")
    print(
      ggplot2::ggplot(df, ggplot2::aes(x =x, y = freq)) + 
        ggplot2::geom_bar(position = "dodge",stat = "identity", fill= "#00759A")+ 
        ggplot2::scale_x_discrete("", labels = catsLabs)+
        ggplot2::ylab("Response frequency among safety issue reports")+
        ggplot2::ggtitle(paste("Issue Type"," (# reports = ",numReports,")",sep = "")) +
        ggplot2::scale_y_continuous(labels = scales::percent) +
        ggplot2::coord_flip()+
        ggplot2::theme(axis.title.x=ggplot2::element_text(size = 20,vjust=-1)) +
        ggplot2::theme(axis.title.y=ggplot2::element_text(size = 20, angle=90, vjust=2)) +
        ggplot2::theme(plot.title=ggplot2::element_text(size=20, vjust=2)) +
        ggplot2::theme(plot.margin = grid::unit(c(0.2,0.2,0.2,0.2), "inches"))
    )
    dev.off()
  }
}
