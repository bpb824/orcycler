###Trip distribution bar plots
library(plyr)
library(dplyr)
library(ggplot2)
library(grid)

report_barPlots= function(reportSummary,reportType){
  source("functions/wrap_sentance.R")
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
    
    plotDir = "results/plots_singleVar/reports/"
    theme_set(theme_bw(base_size = 25))
    
    png(file = paste(plotDir,"severity",".png",sep = "") ,width = 1280, height = 100 + numCats *50, units = "px", bg = "transparent")
    print(
      ggplot(df, aes(x =x, y = freq, width =0.9)) + geom_bar(position = "dodge",stat = "identity", fill= "#00759A")+ scale_x_discrete("", labels = catsLabs)+ylab("Response frequency among crash event reports")+ggtitle(paste("Crash Event Severity"," (# reports = ",numReports,")",sep = "")) +scale_y_continuous(breaks = nums, labels = labs) +coord_flip()+
        theme(axis.title.x=element_text(size = 20,vjust=-1)) + 
        theme(axis.title.y=element_text(size = 20, angle=90, vjust=2)) +
        theme(plot.title=element_text(size=20, vjust=2)) +
        theme(plot.margin = unit(c(0.2,0.2,0.2,0.2), "inches"))
    )
    dev.off()
    
    qaMap = read.csv("qaTables/qar1020.csv",stringsAsFactors = FALSE)
    answers =read.csv("qaTables/answers1020.csv",stringsAsFactors = FALSE)
    
    #Conflicts
    qid = 29
    ansInds = qaMap[qaMap$question_id==qid,"answer_id"]
    ansText = c("No Data",answers[answers$id %in% ansInds,"text"])
    df = as.data.frame(colSums(reportSummary[,3:13])/numReports)
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
    
    plotDir = "results/plots_singleVar/reports/"
    theme_set(theme_bw(base_size = 25))
    
    png(file = paste(plotDir,"conflictWith",".png",sep = "") ,width = 1280, height = 100 + numCats *50, units = "px", bg = "transparent")
    print(
      ggplot(df, aes(x =x, y = freq, width =0.9)) + geom_bar(position = "dodge",stat = "identity", fill= "#00759A")+ scale_x_discrete("", labels = catsLabs)+ylab("Response frequency among crash event reports")+ggtitle(paste("Conflict Type"," (# reports = ",numReports,")",sep = "")) +scale_y_continuous(breaks = nums, labels = labs) +coord_flip()+
        theme(axis.title.x=element_text(size = 20,vjust=-1)) + 
        theme(axis.title.y=element_text(size = 20, angle=90, vjust=2)) +
        theme(plot.title=element_text(size=20, vjust=2)) +
        theme(plot.margin = unit(c(0.2,0.2,0.2,0.2), "inches"))
    )
    dev.off()
    
    #Actions
    qid = 32
    ansInds = qaMap[qaMap$question_id==qid,"answer_id"]
    ansText = c("No Data",answers[answers$id %in% ansInds,"text"])
    df = as.data.frame(colSums(reportSummary[,14:24])/numReports)
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
    
    plotDir = "results/plots_singleVar/reports/"
    theme_set(theme_bw(base_size = 25))
    
    png(file = paste(plotDir,"crashActions",".png",sep = "") ,width = 1280, height = 100 + numCats *50, units = "px", bg = "transparent")
    print(
      ggplot(df, aes(x =x, y = freq, width =0.9)) + geom_bar(position = "dodge",stat = "identity", fill= "#00759A")+ scale_x_discrete("", labels = catsLabs)+ylab("Response frequency among crash event reports")+ggtitle(paste("Crash Event Action"," (# reports = ",numReports,")",sep = "")) +scale_y_continuous(breaks = nums, labels = labs) +coord_flip()+
        theme(axis.title.x=element_text(size = 20,vjust=-1)) + 
        theme(axis.title.y=element_text(size = 20, angle=90, vjust=2)) +
        theme(plot.title=element_text(size=20, vjust=2)) +
        theme(plot.margin = unit(c(0.2,0.2,0.2,0.2), "inches"))
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
    
    plotDir = "results/plots_singleVar/reports/"
    theme_set(theme_bw(base_size = 25))
    
    png(file = paste(plotDir,"crashReasons",".png",sep = "") ,width = 1280, height = 100 + numCats *50, units = "px", bg = "transparent")
    print(
      ggplot(df, aes(x =x, y = freq, width =0.9)) + geom_bar(position = "dodge",stat = "identity", fill= "#00759A")+ scale_x_discrete("", labels = catsLabs)+ylab("Response frequency among crash event reports")+ggtitle(paste("Crash Event Reason"," (# reports = ",numReports,")",sep = "")) +scale_y_continuous(breaks = nums, labels = labs) +coord_flip()+
        theme(axis.title.x=element_text(size = 20,vjust=-1)) + 
        theme(axis.title.y=element_text(size = 20, angle=90, vjust=2)) +
        theme(plot.title=element_text(size=20, vjust=2)) +
        theme(plot.margin = unit(c(0.2,0.2,0.2,0.2), "inches"))
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
    
    plotDir = "results/plots_singleVar/reports/"
    theme_set(theme_bw(base_size = 25))
    
    png(file = paste(plotDir,"urgency",".png",sep = "") ,width = 1280, height = 100 + numCats *50, units = "px", bg = "transparent")
    print(
      ggplot(df, aes(x =x, y = freq, width =0.9)) + geom_bar(position = "dodge",stat = "identity", fill= "#00759A")+ scale_x_discrete("", labels = catsLabs)+ylab("Response frequency among safety issue reports")+ggtitle(paste("Safety Issue Urgency"," (# reports = ",numReports,")",sep = "")) +scale_y_continuous(breaks = nums, labels = labs) +coord_flip()+
        theme(axis.title.x=element_text(size = 20,vjust=-1)) + 
        theme(axis.title.y=element_text(size = 20, angle=90, vjust=2)) +
        theme(plot.title=element_text(size=20, vjust=2)) +
        theme(plot.margin = unit(c(0.2,0.2,0.2,0.2), "inches"))
    )
    dev.off()
    
    qaMap = read.csv("qaTables/qar1020.csv",stringsAsFactors = FALSE)
    answers =read.csv("qaTables/answers1020.csv",stringsAsFactors = FALSE)
    
    #Conflicts
    qid = 30
    ansInds = qaMap[qaMap$question_id==qid,"answer_id"]
    ansText = c("No Data",answers[answers$id %in% ansInds,"text"])
    df = as.data.frame(colSums(reportSummary[,3:17])/numReports)
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
    
    plotDir = "results/plots_singleVar/reports/"
    theme_set(theme_bw(base_size = 25))
    
    png(file = paste(plotDir,"issueType",".png",sep = "") ,width = 1280, height = 100 + numCats *50, units = "px", bg = "transparent")
    print(
      ggplot(df, aes(x =x, y = freq, width =0.9)) + geom_bar(position = "dodge",stat = "identity", fill= "#00759A")+ scale_x_discrete("", labels = catsLabs)+ylab("Response frequency among safety issue reports")+ggtitle(paste("Issue Type"," (# reports = ",numReports,")",sep = "")) +scale_y_continuous(breaks = nums, labels = labs) +coord_flip()+
        theme(axis.title.x=element_text(size = 20,vjust=-1)) + 
        theme(axis.title.y=element_text(size = 20, angle=90, vjust=2)) +
        theme(plot.title=element_text(size=20, vjust=2)) +
        theme(plot.margin = unit(c(0.2,0.2,0.2,0.2), "inches"))
    )
    dev.off()
  }
}