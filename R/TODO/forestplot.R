require(ggplot2)

forestplot <- function(df, xlab="Odds Ratio", ylab="Independent Variable",title,outputDir, rows = NULL,rng= NULL){
  cs = exp(coef(df))
  pvals = coef(summary(df))[,4]
  cs = cs[5:length(cs)]
  pvals=pvals[5:length(pvals)]
  tmp<-data.frame(cbind(cs, exp(confint(df))))
  odds<-tmp[,]
  names(odds)<-c("OR", "lower", "upper")
  if (is.null(rows)){
    odds$vars<-row.names(odds)
  }else{
    odds$vars = rows
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
  pvals = pvals[complete.cases(odds)]
  odds = odds[complete.cases(odds),]
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
  if (is.null(rng)){
    p <- ggplot(odds, aes(x=vars, y=OR, ymin=lower, ymax=upper)) + 
      geom_point(size = 4)+
      geom_pointrange() + 
      coord_flip() +
      geom_hline(yintercept=1, lty=2) +
      ylab(xlab) +
      ggtitle(title)+
      xlab(ylab) #switch because of the coord_flip() above
  }else{
    for (i in 1:nrow(odds)){
      upper = odds$upper[i]
      if (upper>rng[2]){
        odds$upper[i]=rng[2]
      }
    }
    p <- ggplot(odds, aes(x=vars, y=OR, ymin=lower, ymax=upper)) + 
      geom_point(size = 4)+
      scale_y_continuous(limits = rng)+
      geom_pointrange() + 
      coord_flip() +
      geom_hline(yintercept=1, lty=2) +
      ylab(xlab) +
      ggtitle(title)+
      xlab(ylab) #switch because of the coord_flip() above
  }
  png(file = paste(modelOutDir,title,".png",sep = "") ,width = 1280, height = 200 + length(odds$vars)*50, units = "px", bg = "transparent")
  print(p)
  dev.off()
}