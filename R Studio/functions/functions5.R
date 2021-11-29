##########################################################################################

import_data<-function(dataset)
	{
	 data_<-read.csv(paste("data\\",dataset,".txt",sep=""),sep=",",dec=".",header=T)
	 data_$Date<-as.Date(as.character(data_$Date),"%Y%m%d")
	 data_<-data_[,c(2,6)]
	 names(data_)[2]<-dataset
	 return(data_)
	}

##########################################################################################

testdf<-function(variable, adf_order)
	{
	results_adf<-data.frame(order=-1,adf=0,p_adf="",bgodfrey=0,p_bg=0)
	variable<-variable[!is.na(variable)]
	
	for(order in 0:adf_order)
		{
		df.test_<-ur.df(variable,type = c("drift"),lags=order)
		df_<-df.test_@teststat[1]
		df_crit<-df.test_@cval[1,]
		df_crit<-(df_<df_crit)*1
		p_adf<-ifelse(sum(df_crit)==0,">10pct",paste("<",names(df_crit)[min(which(df_crit==1))],sep=""))
		resids_<-df.test_@testreg$residuals
		bgtest_<-bgtest(resids_~1,order=1)
		bgodfrey<-bgtest_$statistic
		names(bgodfrey)<-NULL
		p_bg<-bgtest_$p.value
		
		results_adf<-rbind(results_adf,data.frame(order=order,adf=df_,p_adf=p_adf,bgodfrey=bgodfrey,p_bg=p_bg))
		rm(df.test_,df_,df_crit,resids_,bgtest_,bgodfrey,p_bg)
		}
	
	results_adf<-results_adf[results_adf$order>=0,]
	
	plot(variable,type="l",col="blue",lwd=2,main="Plot of the examined variable")

	return(results_adf)
	}	
##########################################################################################

plot.varfevd  <- function (x, plot.type = c("multiple", "single"), names = NULL,
                           main = NULL, col = NULL, ylim = NULL, ylab = NULL, xlab = NULL,
                           legend = NULL, names.arg = NULL, nc, mar = par("mar"), oma = par("oma"),
                           addbars = 1, ...)
{
  K <- length(x)
  ynames <- names(x)
  plot.type <- match.arg(plot.type)
  if (is.null(names)) {
    names <- ynames
  }
  else {
    names <- as.character(names)
    if (!(all(names %in% ynames))) {
      warning("\nInvalid variable name(s) supplied, using first variable.\n")
      names <- ynames[1]
    }
  }
  nv <- length(names)
  #    op <- par(no.readonly = TRUE)
  ifelse(is.null(main), main <- paste("FEVD for", names), main <- rep(main,
                                                                      nv)[1:nv])
  ifelse(is.null(col), col <- gray.colors(K), col <- rep(col,
                                                         K)[1:K])
  ifelse(is.null(ylab), ylab <- rep("Percentage", nv), ylab <- rep(ylab,
                                                                   nv)[1:nv])
  ifelse(is.null(xlab), xlab <- rep("Horizon", nv), xlab <- rep(xlab,
                                                                nv)[1:nv])
  ifelse(is.null(ylim), ylim <- c(0, 1), ylim <- ylim)
  ifelse(is.null(legend), legend <- ynames, legend <- legend)
  if (is.null(names.arg))
    names.arg <- c(paste(1:nrow(x[[1]])), rep(NA, addbars))
  plotfevd <- function(x, main, col, ylab, xlab, names.arg,
                       ylim, ...) {
    addbars <- as.integer(addbars)
    if (addbars > 0) {
      hmat <- matrix(0, nrow = K, ncol = addbars)
      xvalue <- cbind(t(x), hmat)
      barplot(xvalue, main = main, col = col, ylab = ylab,
              xlab = xlab, names.arg = names.arg, ylim = ylim,
              legend.text = legend, ...)
      abline(h = 0)
    }
    else {
      xvalue <- t(x)
      barplot(xvalue, main = main, col = col, ylab = ylab,
              xlab = xlab, names.arg = names.arg, ylim = ylim,
              ...)
      abline(h = 0)
    }
  }
  if (plot.type == "single") {
    #        par(mar = mar, oma = oma)
    #        if (nv > 1)
    #            par(ask = TRUE)
    for (i in 1:nv) {
      plotfevd(x = x[[names[i]]], main = main[i], col = col,
               ylab = ylab[i], xlab = xlab[i], names.arg = names.arg,
               ylim = ylim, ...)
    }
  }
  else if (plot.type == "multiple") {
    if (missing(nc)) {
      nc <- ifelse(nv > 4, 2, 1)
    }
    nr <- ceiling(nv/nc)
    par(mfcol = c(nr, nc), mar = mar, oma = oma)
    for (i in 1:nv) {
      plotfevd(x = x[[names[i]]], main = main[i], col = col,
               ylab = ylab[i], xlab = xlab[i], names.arg = names.arg,
               ylim = ylim, ...)
    }
  }
  #    on.exit(par(op))
}

# Source: http://stackoverflow.com/questions/33632401/fix-layout-plot-options-of-plotfevd-function

#######################################################################################################

# Multiple plot function
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
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
# Source: http://www.cookbook-r.com/Graphs/Multiple_graphs_on_one_page_(ggplot2)/

grid_arrange_shared_legend <- function(..., ncol = length(list(...)), nrow = 1, position = c("bottom", "right")) {
  
  plots <- list(...)
  position <- match.arg(position)
  g <- ggplotGrob(plots[[1]] + theme(legend.position = position))$grobs
  legend <- g[[which(sapply(g, function(x) x$name) == "guide-box")]]
  lheight <- sum(legend$height)
  lwidth <- sum(legend$width)+10 
  gl <- lapply(plots, function(x) x + theme(legend.position="none"))
  gl <- c(gl, ncol = ncol, nrow = nrow)
  
  combined <- switch(position,
                     "bottom" = arrangeGrob(do.call(arrangeGrob, gl),
                                            legend,
                                            ncol = 1,
                                            heights = unit.c(unit(1, "npc") - lheight, lheight)),
                     "right" = arrangeGrob(do.call(arrangeGrob, gl),
                                           legend,
                                           ncol = 2,
                                           widths = unit.c(unit(1, "npc") - lwidth, lwidth)))
  grid.newpage()
  grid.draw(combined)
  
}

#Soiurce: https://github.com/hadley/ggplot2/wiki/Share-a-legend-between-two-ggplot2-graphs
