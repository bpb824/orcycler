#' User response plotting
#'
#' Plots all user response distributions to 'results' folder
#'
#' @param userSummary User summary table outputted from \code{\link{userPrep}}
#'
#' @return None
#'
#' @export
#' 
user_barPlots= function(userSummary){
  require(plyr)
  require(ggplot2)
  require(grid)
  
  #source("functions/wrap_sentance.R")
  
  vars = colnames(userSummary[4:ncol(userSummary)])
  for (i in 1:length(vars)){
    var = userSummary[,vars[i]]
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
    
    titles = c("Age","Gender","Ethnicity","Occupation","Income","Household Workers","Household Vehicles","Number of Bicycles","Bicycle Types","Cycling Frequency","Cycling Weather","Rider Ability","Rider Type")
    
    plotDir = "results/plots_singleVar/users/"
    numUsers = nrow(userSummary)
    theme_set(theme_bw(base_size = 25))
    
    png(file = paste(plotDir,vars[i],".png",sep = "") ,width = 1280, height = 100 + numCats *50, units = "px", bg = "transparent")
    print(
      ggplot(df, aes(x =x, y = freq, width =0.9)) + geom_bar(position = "dodge",stat = "identity", fill= "#00759A")+ scale_x_discrete("", labels = catsLabs)+ylab("Response frequency among users")+ggtitle(paste(titles[i]," (# Users = ",numUsers,")",sep = "")) +scale_y_continuous(breaks = nums, labels = labs) +coord_flip()+
        theme(axis.title.x=element_text(size = 20,vjust=-1)) + 
        theme(axis.title.y=element_text(size = 20, angle=90, vjust=2)) +
        theme(plot.title=element_text(size=20, vjust=2)) +
        theme(plot.margin = unit(c(0.2,0.2,0.2,0.2), "inches"))
    )
    dev.off()
  }
}