############################################################
# populationOverTimePlot
#

populationOverTimePlot <- function(dataList, infoList, textList) {
    
  ########################################
  # Set up style
  ########################################
  
  oma = c(0,1,1,0)
  mar <- c(4,4,4,1) + 0.1 # default is c(5,4,4,2)+0.1
  par(mar=mar, oma=oma)
  
  lineWidth <- 1 #width of standard plot
  axisSize <- 1 # cex of numbers on axes
  axisLine <- 0.7 # distance of axes from plot
  labSize <- 1.2 # text size of axis labels
  labLine <- 3.3 # distance of labels from plot
  tick <- -0.02 # width of axis ticks
  gridWidth <- 1 # width of gridlines
  
  percentLimit <- list(high=8, low=-4)
  
  ########################################
  # Plot the data
  ########################################
  
  
  midYearPop <- dataList[['midYearPop']]
  estimateUpdateYears = dataList[['estimateUpdateYears']]
  
  ### hide projections past 2025
  yearMask <- midYearPop$year <= 2025
  years <- midYearPop$year[yearMask]
  
  ### If Namibia is included, add a period to the end of it's code so R doesn't read it as NA
  if ("NA" %in% infoList$countryCode) {
    infoList$countryCode = replace(infoList$countryCode, infoList$countryCode == "NA", "NA.")
  }
  
  ### if there are multiple country codes add their populations
  if (length(infoList$countryCode)>1) {
    projectionStart <- min(estimateUpdateYears[1,infoList$countryCode])
    population <- apply(midYearPop[,infoList$countryCode], 1, sum)[yearMask]
  } else {
    projectionStart <- estimateUpdateYears[1,infoList$countryCode]
    population <- midYearPop[,infoList$countryCode][yearMask]
  }
  
  ### max population
  pMax <- max(population)
  
  ### adjust labels and population if max population is over/under 1,000,000
  if (pMax < 1000000) {
    textList$units <- textList$thousands
    textList$unit <- textList$thousand
    population <- population / 1000
    pMax <- pMax / 1000
  } else if (pMax > 1000000) {
    textList$units <- textList$millions
    textList$unit <- textList$million
    population <- population / 1000000
    pMax <- pMax / 1000000  
  }
  
  ### make year labels
  yearLabs <- c(1950,1960,1970,1980,1990,2000,2010,2020,2030,2040)
  yearAt <- c(0,10,20,30,40,50,60,70,80,90)
  
  ### find growth for header
  oldPop <- population[match(2000, years)]
  currentPop <- population[match(2014, years)]
  percentGrowth <- round(100 * (currentPop - oldPop) / oldPop, digits=0)
  # Create the subhead using the template for either 'growth' or 'decline'
  stringName <- ifelse(percentGrowth >= 0,'growth','decline')
  if (str_detect(textList[[stringName]],'__REPLACE__')) {
    textList$subhead <- str_replace(textList[[stringName]],'__REPLACE__',abs(percentGrowth))
  } else {
    textList$subhead <- paste(percentGrowthString,textList[[stringName]])
  }
  
  #########################################
  ### population over time line plot
  #########################################
  
  par(fig=c(0,1,0.445,1), yaxs='i', xaxs='i', lty=0, mgp = c(0, 0.75, 0), xpd=NA)
  
  plot(years, population, type="n",
       axes=FALSE, xlab=NA, ylab=NA, main=NA, 
       ylim=range(0,pMax), xlim=c(min(years), max(years)))
  
  ### draw polygon under line
  y <- population
  x <- years
  y2 <- rep(y, each=2)
  y2 <- y2[-length(y2)]
  x2 <- rep(x, each=2)[-1]
  x3 <- c(min(x2), x2, max(x2))
  y3 <- c(0, y2, 0)
  polygon(x3, y3, border=NA, col=rgb(0.9,0.9,0.9))
  
  ### special annotations for declining populations
  if (percentGrowth < -0.5) {
    ### horizontal line showing current population
    segments(min(years), currentPop, 2014, currentPop, lty=1, col="white", lwd=2)
    ### label the pre-2000 year at this population
    nowIndex <- match(2014,years)
    y2kIndex <- match(2000,years)
    thenIndex <- max(which(population[1:y2kIndex] < currentPop))
    thenYear <- years[thenIndex]
    thenPop <- population[thenIndex]
    text(thenYear-2,thenPop,toString(thenYear),pos=3)
  }
  
  ### draw line
  lines(years, population, type='s', lwd=1, col="black", lty=1, xpd=NA)
  
  ### draw axes
  axis(side=1, at=c(min(years),max(years)), labels=FALSE, line=axisLine, lwd.ticks=0)
  axis(side=1, at=yearLabs, labels=yearLabs, cex.axis=axisSize, lwd=0, lwd.ticks=1, line=axisLine, tck=tick)
  axis(side=2, at=c(0,pMax), labels=FALSE, line=axisLine, lwd.ticks=0)
  axis(side=2, cex.axis=axisSize, line=axisLine, lwd=0, lwd.ticks=1, tck=tick, las=1)
  
  ### axis label text
  mtext(paste(textList$ylab1, " (", textList$units,")", sep=''), side=2, cex=labSize, line=labLine)
  
  ### draw main text and subtitle text
  mtext(paste(textList$countryName, ": ", round(currentPop, 1), " ", textList$unit, " (", textList$population, ")", sep=""), 
        side=3, cex=labSize*1.2, line=labLine, font=2)
  mtext(textList$subhead, side=3, cex=labSize*1.2, line=labLine-1.4, font=1)
  
  ### draw ablines up until the population line
  for (i in seq(1,101,by=10)) {
    segments(years[i], 0, years[i], population[i], lwd=gridWidth, lty=3)
  }
  
  ### projection block
  rect(projectionStart,0,2025,pMax, col=rgb(0.3,0.3,0.3,0.5), angle=45, density = 20, lty=1, border=FALSE)
  
  ### prjection text
  text(mean(c(projectionStart, 2025)), pMax + pMax/20, textList$projection, cex=1.0, col=rgb(0.3,0.3,0.3))
  
  
  #########################################
  ### change in population per year barplot
  #########################################
  
  par(fig=c(0,1,0,0.555), new=TRUE, yaxs='i', xaxs='i', lty=0)
  
  ### calculate the percentage change from year to year
  y <- diff(population)
  y <- c(0, y)
  percentChange <- 100 * y/population
  
  ###for different colored bars
  col1 <- '#91cebe'
  col2 <- '#d8735b'
  
  barColors <- c()
  for (i in 1:length(percentChange)) {
    if (percentChange[i] > 0) {
      barColors <- c(barColors, col1)
    } else {
      barColors <- c(barColors, col2)
    }
  }
  
  ### set range and labels for bar plot
  pAt <- c(-2,0,2,4,6)
  pRange <- c(-2,6)
  pLabels <- c("-2%","0%","2%","4%","6%")
  
  dMax <- max(percentChange)
  dMin <- min(percentChange)
  
  ### allow yrange of bar plot to fit to outliers
  if ((dMax > percentLimit$high) | dMin < percentLimit$low) {
    scalar <- round((dMax - dMin)/6)
    range <- round(c(dMin/scalar, dMax/scalar))
    
    pAt <- scalar*seq(range[1], range[2]) 
    pRange <- c(min(pAt),max(pAt))
    pLabels <- paste(pAt, "%", sep="")
  }
  
  ### make solid barplot of population change
  barplot(head(percentChange, -1), space=0, axes=FALSE, 
          xlab=NA, ylab=NA, border=FALSE, col=barColors,
          main=NA, ylim=pRange, xlim=range(0,length(years)-1), xpd=T)
  
  ### draw axes
  axis(side=3, at=c(0, length(years)), labels=FALSE, line=axisLine, lwd.ticks=0)
  axis(side=3, at=yearAt, labels=FALSE, line=axisLine, lwd=0, lwd.ticks=1, tck=tick)
  axis(side=2, at=pRange, line=axisLine, lwd.ticks=0, labels=FALSE)
  axis(side=2, las=1, lwd=0, at=pAt, labels=pLabels, lwd.ticks=1, cex.axis=axisSize, line=axisLine, tck=tick)
  
  ### draw axis label
  mtext(textList$ylab2, side=2, cex=labSize, line=labLine)
  
  ### white horizontal gridlines
  for (i in pAt) {
    segments(0, i, length(years)-1, i, col="white", lty=3, lwd=gridWidth, xpd=NA)
  }
  
  # draw ablines up until the population line
  for (i in seq(0,100,by=10)) {
    segments(i, max(0,percentChange[i+1]), i, max(pRange), lwd=gridWidth, lty=3)
  }
  
  lines(head(percentChange, -1), type='S', lty=1, xpd=T)
  
  rect(match(projectionStart,years)-1,min(pRange),75,max(pRange), col=rgb(0.3,0.3,0.3,0.5), angle=45, density = 20, lty=1, border=FALSE)
  
  # plot information at bottom
  mtext(textList$subtitle, side=1, line=2, cex=labSize)
    
}

