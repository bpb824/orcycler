#' forestPlot
#'
#' Create a forest plot to visualize cumulative logistic regression model coefficients
#'
#' @param model Cumulative logistic regression model object
#' @param xlab X-axis label, default = "Odds Ratio"
#' @param ylab Y-Axis label, default = "Independent Variable"
#' @param title Plot title
#' @param outputDir Where to plot the .png output
#' @param rownames Optional row names parameter
#' @param xRng X-axis range
#'
#' @return None
#' @export
forestplot <- function(model, xlab="Odds Ratio", ylab="Independent Variable",title,outputDir, rownames = NULL,xRng= NULL){

  coeffs = exp(stats::coef(model))
  pvals = stats::coef(summary(model))[,4]
  coeffs = coeffs[5:length(coeffs)]
  pvals=pvals[5:length(pvals)]
  odds=data.frame(cbind(coeffs, exp(stats::confint(model))))
  names(odds)<-c("OR", "lower", "upper")
  if (is.null(rownames)){
    odds$vars<-row.names(odds)
  }else{
    odds$vars = rownames
  }
  for (i in 1:nrow(odds)){
    pv = pvals[i]
    if (pv>=0.1){
      odds[i,]=NA
    }
  }
  for (i in 1:nrow(odds)){
    var = odds$vars[i]
    if (grepl(".Q",var,fixed=TRUE) | grepl(".C",var,fixed=TRUE)){
      odds[i,]=NA
    }
  }
  pvals = pvals[stats::complete.cases(odds)]
  odds = odds[stats::complete.cases(odds),]
  if(nrow(odds)>0){
    for (i in 1:nrow(odds)){
      pv = pvals[i]
      if (pv>=0.05){
        odds$vars[i]=paste(odds$vars[i],"*",sep="")
      }else if (pv<0.05 & pv>=0.01){
        odds$vars[i]=paste(odds$vars[i],"**",sep="")
      }else{
        odds$vars[i]=paste(odds$vars[i],"***",sep="")
      }
    }
    odds$vars = factor(odds$vars, levels=unique(odds$vars))
    if (is.null(xRng)){
      p <- ggplot2::ggplot(odds, aes(x=vars, y=OR, ymin=lower, ymax=upper)) +
        ggplot2::geom_point(size = 4)+
        ggplot2::geom_pointrange() +
        ggplot2::coord_flip() +
        ggplot2::geom_hline(yintercept=1, lty=2) +
        ggplot2::ylab(xlab) +
        ggplot2::ggtitle(title)+
        ggplot2::xlab(ylab) #switch because of the coord_flip() above
    }else{
      for (i in 1:nrow(odds)){
        upper = odds$upper[i]
        if (upper>xRng[2]){
          odds$upper[i]=xRng[2]
        }
      }
      p <- ggplot2::ggplot(odds, aes(x=vars, y=OR, ymin=lower, ymax=upper)) +
        ggplot2::geom_point(size = 4)+
        ggplot2::scale_y_continuous(limits = xRng)+
        ggplot2::geom_pointrange() +
        ggplot2::coord_flip() +
        ggplot2::geom_hline(yintercept=1, lty=2) +
        ggplot2::ylab(xlab) +
        ggplot2::ggtitle(title)+
        ggplot2::xlab(ylab) #switch because of the coord_flip() above
    }
    png(file = paste(outputDir,title,".png",sep = "") ,width = 1280, height = 200 + length(odds$vars)*50, units = "px", bg = "transparent")
    print(p)
    dev.off()
  }else{
    stop("No significant variables in model to plot.")
  }

}
