#############################################################################################
# Functions used to generate plots containing lines, possibly several distinct series.
# Note: very old source code from the Ganetto project.
# 
# 10/2015 Vincent Labatut
#############################################################################################
source("src/plot-tools/plot-common.R")




#############################################################################################
# Plots the specified graph as a line chart. Each series is represented as a separate line
# with its own color. The plot scale is processed automatically depending on the values and
# optional dispersion values. 
#
# plot.file: path and name of the generated files.
# series:	data to plot, as a named list of series. Each series is itself a list containing 
#			some fields named x, y and stdev, which are vectors. All of them must have the 
#			same length, for a given series. Field stdev is optional, but x and y are not.
# axes: info about the plot axes. It is an optional list containing two lists named x and y.
#		Each list contains two optional fields range and title. Field range corresponds to the 
#		forced range for the considered axis. It is a vector containing two values corresponding
#		to the min and max. An NA symbol means the min and/or max is not specified. If the min
#		or max is not specified, we use the min or max processed over all considered series. 
#		Field title is the name of the considered axis on the plot.
# plot.title: general title of the plot (NA for no title at all).
# with.lines:	whether to draw lines between points, or not.
# format: vector of formats of the generated files (PDF and/or PNG, NA for the screen).
#############################################################################################
plot.lines <- function(plot.file, series, axes=list(), plot.title, with.lines=TRUE, consider.stdev=TRUE, format=c("PDF","PNG", NA))
{	# complete axes lists (if needed)
	axes <- complete.axes(axes)
	# complete format vector (if needed)
	if(length(format)==0)
		format = NA
	
	# deal with symbolic x values
	serie <- series[[1]]
	values <- serie$x
	values <- values[!is.na(values)]
	numeric <- TRUE
	if(!is.numeric(values[1]))
	{	numeric <- FALSE
		tmp <- clean.symbolic.data(axes, series)
		series <- tmp$series
		axes <- tmp$axes
		x.old.values <- tmp$x.old.values
		x.new.values <- tmp$x.new.values
	}
		
	# clean numerical y values
	series <- clean.numerical.data(series)
		
	# init axes ranges
	axes <- init.x.range(axes, series)
	axes <- init.y.range(axes, series, consider.stdev)
	
	# init colors
	#colors <- c("red","green","black","purple","orange","blue")
	if(length(series)>1)
	{	colors <- rainbow(length(series),v=0.8)
		line.symbs <- 1:length(series)
		line.types <- 1:length(series)
	}
	else
	{	colors <- c("red")
		line.symbs <- c(1)
		line.types <- c(1)
	}
	
	# init plot options
	lines.width <- 1
	type <- "p" # points
	if(with.lines)
		type <- "o"
	legend.pos <- determine.legend.position(series,axes)
	
	# process each specified file format
	for(frmt in format)
	{	# set plot file
		plot.filename <- paste(plot.file,".",frmt,sep="")
		# create the file
		if(!is.na(frmt))
		{	if(frmt=="PNG")
			{	png(filename=plot.filename,width=800,height=800,units="px",pointsize=20,bg="white")
			}
			else if(frmt=="PDF")
			{	pdf(file=plot.filename,bg="white",compress=COMPRESS)
			}
		}
		
		# setup axes
		series.names <- names(series)
		i <- 1;
		serie <- series[[series.names[i]]]
		if(numeric)
			xaxis <- 's'
		else
			xaxis <- 'n'
		# create plot	
		plot(serie$x,serie$y,
				type=type, pch=line.symbs[i], col=colors[i], lwd=lines.width, lty=line.types[i],
				xlim=axes$x$range, ylim=axes$y$range,
				ann=FALSE, xaxt=xaxis)
		# add series
		for(i in 1:length(series))
		{	serie <- series[[series.names[i]]]
			lines(serie$x, serie$y,
				type=type, pch=line.symbs[i], col=colors[i], lwd=lines.width, lty=line.types[i],
				ann=FALSE, xaxt=xaxis)
			if(!is.null(serie$stdev) && length(serie$stdev[!is.na(serie$stdev)]>0))
			{	plotCI(serie$x, serie$y, uiw=serie$stdev, liw=serie$stdev,
					err="y", add=TRUE,
					pch=line.symbs[i], col=colors[i], lwd=lines.width)
			}
		}
		# fix x axis for symbolic values
		if(!numeric)
			axis(side=1, at=x.new.values, labels=x.old.values)
		
		# add legend
		if(length(series.names)>1)
			legend(x=legend.pos, legend=series.names, col=colors, pch=line.symbs,
				lwd=lines.width,lty=line.types,bty="n",cex=0.8)
		# add titles
		if(!is.na(plot.title))
			title(main=plot.title)
		title(xlab=axes$x$title, ylab=axes$y$title)
		
		# finalize plot
		if(!is.na(frmt))
			dev.off()
	}
}


#############################################################################################
# Performs the necessary transformations when dealing with an x axis containing symbolic values
# i.e. non-numerical values). It converts them into integers, while keeping the original symbols
# to allow later substitution.
#
# axes: same as in the plot.lines function.
# series: same as in the plot.lines function.
#
# returns: a list containing updated variables.
#############################################################################################
clean.symbolic.data <- function(axes, series)
{	# replace by numeric values in the data
	correspondance <- list()
	cmpt <- 1
	for(s in 1:length(series))
	{	serie <- series[[s]]
		values <- serie$x
		values <- values[!is.na(values)]
		new.values <- rep(NA,length(values))
		for(i in 1:length(values))
		{	old.val <- values[i]
			if(is.null(correspondance[[old.val]]))
			{	correspondance[[old.val]] <- cmpt
				cmpt <- cmpt + 1
			}
			new.val <- correspondance[[old.val]]
			new.values[i] <- new.val
		}
		series[[s]]$x <- new.values
	}
	# keep the old symbolic values to switch when plotting
	x.old.values <- names(correspondance)
	x.new.values <- c()
	for(v in x.old.values)
		x.new.values <-  c(x.new.values,correspondance[[v]])
	# reinit x range
	axes$x$range <- c(NA,NA)
		
	# build result list
	result <- list(
		series=series,
		axes=axes,
		x.old.values=x.old.values,
		x.new.values=x.new.values
	)
	return(result)
}


#############################################################################################
# Sets the range values for the x axis and the specified data.
#
# axes: same as in the plot.lines function.
# series: same as in the plot.lines function.
#
# returns: an updated axes list.
#############################################################################################
init.x.range <- function(axes, series)
{	fs <- list(min,max)
	for(f in 1:length(fs))
	{	temp.opt <- c()
		for(s in 1:length(series))
		{	serie <- series[[s]]
			values <- serie$x
			values <- values[!is.na(values)]
			vect.opt <- fs[[f]](values)
			temp.opt <- c(temp.opt,vect.opt)
		}
		axes$x$range[f] <- fs[[f]](temp.opt)
	}
	return(axes)
}


#############################################################################################
# Sets the range values for the y axis and the specified data.
#
# axes: same as in the plot.lines function.
# series: same as in the plot.lines function.
# consider.stdev: same as in the plot.lines function.
#
# returns: an updated axes list.
#############################################################################################
init.y.range <- function(axes, series, consider.stdev)
{	fs <- list(min,max)
	for(f in 1:length(fs))
	{	if(is.na(axes$y$range[f]))
		{	temp.opt <- c()
			for(s in 1:length(series))
			{	serie <- series[[s]]
				values <- serie$y
				if(consider.stdev && !is.null(serie$stdev) && length(serie$stdev[!is.na(serie$stdev)]>0))
					values <- values - serie$stdev
				values <- values[!is.na(values)]
				if(length(values)>0)
				{	vect.opt <- fs[[f]](values)
					temp.opt <- c(temp.opt,vect.opt)
				}
				else
					temp.opt <- 0
			}
			axes$y$range[f] <- fs[[f]](temp.opt)
		}
	}
	return(axes)
}


#############################################################################################
# Determines where the legend should go in a plot. More precisely: which quadrant of the plot.
# 
# series: same as in the plot.lines function.
#
# returns: position of the legend.
#############################################################################################
determine.legend.position <- function(series, axes)
{	result <- ""

	x <- c()
	y <- c()
	
	# add actual values
	for(i in 1:length(series))
	{	serie <- series[[i]]
		x <- c(x, serie$x)
		y <- c(y, serie$y)
	}
	
	# add axis-based extreme values
	x <- c(x, axes$x$range[1], axes$x$range[1], axes$x$range[2], axes$x$range[2])
	y <- c(y, axes$y$range[1], axes$y$range[2], axes$y$range[1], axes$y$range[2])
	
	
	print(x)
	print(y)
	# center of the empty space
	pos <- emptyspace(x,y)
	
	# determine the corresponding quadrant
	y.sep <- mean(range(y))
	if(pos[2]<y.sep)
		result <- "bottom"
	else
		result <- "top"
	x.sep <- mean(range(x))
	if(pos[1]<x.sep)
		result <- paste(result,"left",sep="")
	else
		result <- paste(result,"right",sep="")
	
	return(result)
}


#############################################################################################
## Tests
#############################################################################################
#plot.file <- "temp"
#plot.title <- "Graph Title"
#x1 <- 1:10
#x2 <- 1:20
#x3 <- 1:25
#series <- list(
#	"1st Series"=list(x=x1, y=(2*x1+1), stdev=c(runif(length(x1)))),
#	"2nd Series"=list(x=x2, y=(x2/2+10), stdev=c(runif(length(x2)))),
#	"3rd Series"=list(x=x3, y=(x3^2/4), stdev=c(runif(length(x3))))
#)
#axes <- list(
#	x=list(range=c(NA,25), title="x axis"),
#	y=list(range=c(NA,NA), title="y axis")
#)
#plot.lines(plot.file, series, axes, plot.title, with.lines=TRUE, consider.stdev=TRUE, format=c("PDF","PNG",NA))
