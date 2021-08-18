### Toolbox functions #####
#--------------------------------------------------------------------------
# Variance Inflation factor
#--------------------------------------------------------------------------
VIF <- function(linearmodel){
  dat <- as.data.frame(linearmodel$model[-1])
  KK <- ncol(dat)
  if(KK==1){
    print("No collinearity possible, only one regressor.")
  }
  else{
    resultsVIF <- matrix(NA, nrow=KK, ncol=1)
    rownames(resultsVIF) <- colnames(dat)
    colnames(resultsVIF) <- "VIF"
    for(i in 1:KK){
      VIFformula <- paste(colnames(dat[i]),"~ .")
      regrVIF <- lm(VIFformula, data=dat)
      resultsVIF[i] <- 1/(1-summary(regrVIF)$r.squared)
    }
    return(resultsVIF)
  }
}
#--------------------------------------------------------------------------
#Splitting up dataset in test and training set
#--------------------------------------------------------------------------
testsplit <- function(data, split=0.5){
  NN <- nrow(data)
  data$splitvar <- runif(NN, min=0,max=1)
  data$set <- "training"
  for(i in 1:NN){
    if(data$splitvar[i]>split){data$set[i] <- "test"}else{}
  }
  return(data)
}
#--------------------------------------------------------------------------
#Classification Error 
#--------------------------------------------------------------------------
confusion.table <- function(true.data, predicted.data){
  t1 <- table(true.data, predicted.data)
  print(t1/sum(t1)*100)
  print(paste("fraction correctly predicted ",t1[1,1]+t1[2,2],"%"))
}
#--------------------------------------------------------------------------
#Jarque Bera Test
#--------------------------------------------------------------------------
#moment function
zm <- function(daten, m){
  ssize <- length(daten)
  return((1/ssize*sum((daten-mean(daten))^m)))
}
#test statistic
jb.test <- function(testdata){
  samplesize <- length(testdata)
  jb.skew <- zm(testdata,3)/(zm(testdata,2)^1.5)
  jb.kurtosis <- zm(testdata,4)/(zm(testdata,2)^2)
  jb.statistic <- samplesize/6*(jb.skew^2+(jb.kurtosis-3)^2/4)
  RVAL <- list(statistic = c(JB = jb.statistic), p.value = pchisq(jb.statistic, df=2), 
               method = "Jarque Bera normality test", data.name=deparse(substitute(testdata)))
  class(RVAL) <- "htest"
  return(RVAL)
}
#--------------------------------------------------------------------------
#Multiplot
#--------------------------------------------------------------------------
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}


#--------------------------------------------------------------------------
#Theil's T Index 
#--------------------------------------------------------------------------

theil_t <- function(data, subgroups=NA){
  
  theil_index <- (1/length(data))*sum(data/mean(data)*log(data/mean(data)))
  
  return(theil_index)
  
}

