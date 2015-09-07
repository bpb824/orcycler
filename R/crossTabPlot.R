#' Cross tablutation plot
#'
#' @param data Trip, user, or report table for plotting
#' @param xVar Variable divide into seperate bars
#' @param yVar Variable to fill sections of bars by
#' @param xlab Label for x variable
#' @param ylab Label of y variable
#' @param colorPal Color pallette to use for plotting
#' @param xCats X category names, default is levels of x variable
#' @param yCats Y category names, default is levels of y variable
#' @param px Width of plot in pixels, default is 1280 pixels
#' @param py Height of plot in pixels, default is 560 pixels
#'
#' @return None
#' @export
crossTabPlot= function(data,xVar,yVar,xlab,ylab,colorPal,xCats=levels(x),yCats=levels(y),px=1280,py=560){
  x = data[,xVar]
  y = data[,yVar]
  chi = chisq.test(x,y,simulate.p.value = TRUE)
  dfo = as.data.frame(table(x,y))
  df = ddply(dfo, .(x), transform, pos = (cumsum(Freq) - (0.5 * Freq))/sum(Freq))
  df$Freq[df$Freq==0]=NA
  breaks = c(0,0.25,0.5,0.75,1)
  labs = c("0%","25%","50%","75%","100%")
  numLevs = length(levels(y))
  plotDir = "results/plots_doubleVar/users/"

  source("functions/wrap_sentance.R")
  char25 = FALSE
  for (i in xCats){
    if (nchar(i)>25){
      char25 = TRUE
      break
    }
  }
  if(char25){
    xLabs = vector()
    for (i in 1:length(xCats)){
      if(nchar(xCats[i])>25){
        xLabs[i] = wrap_sentence(xCats[i],25)
      }else{
        xLabs[i]=xCats[i]
      }
    }
    xCats=xLabs
  }

  png(file = paste(plotDir,xlab,"_vs_",ylab,".png",sep = "") ,width = px, height = py, units = "px", bg = "transparent")
    g = ggplot(df, aes(x = x)) + geom_bar(aes(weight=Freq, fill = y), position = 'fill') + scale_y_continuous("",breaks = breaks,labels = labs) +scale_x_discrete(labels = xCats)+xlab(xlab)+ scale_fill_manual(name = ylab,values = colorPal,labels = yCats)+coord_flip()+geom_text(aes(label = Freq, y = pos), size = 6)
    if(length(chi)>1){
      stat = as.numeric(chi["statistic"])
      p = as.numeric(chi["p.value"])
      if(p<0.1&p>=0.05){
        pv="p<0.1"
      }else if(p<0.05&p>=0.01){
        pv="p<0.05"
      }else if(p<0.01){
        pv="p<0.01"
      }else{
        pv = "NS"
      }
      g= g+ggtitle(paste(xlab," vs. ",ylab," (Chi-square=",round(stat,2),", ",pv,")",sep =""))
    }
    print(g)
  dev.off()
}

