#############################################################################################
# Function used to generate a histogram.
# 
# 10/2015 Vincent Labatut
#############################################################################################
library("plotrix")
source("src/plot-tools/plot-lines.R")




#############################################################################################
# Plots the specified single series as a histogram. The y axis correspond to absolute values 
# (by opposition to proportions) describing the series to be plot, and each x bar corresponds 
# to one categorical or integer value.
#
# plot.file: path and name of the generated files.
# x.values: vector of symbols or integer values.
# y.values: vector of numerical values.
# axes: info about the plot axes. It is an optional list containing two lists named x and y.
#		The y list contains two optional fields range and title. Field range corresponds to the 
#		forced range for the y axis. It is a vector containing two values corresponding
#		to the min anx max. An NA symbol means the min and/or max is not specified. If the min
#		or max is not specified, we use the min or max processed over all considered series. 
#		Field title is the name of the y axis on the plot. The x list contains two fields title
#		and values. The formder is similar to that of the y list. The latter is a vector
#		containing all values of x to appear in the plot, in the specified order.
# plot.title: general title of the plot (NA for no title at all).
# format: vector of formats of the generated files (PDF and/or PNG).
#############################################################################################
plot.single.indiv.histo <- function(plot.file, x.values, y.values, axes=list(), plot.title, format=c("PDF","PNG"))
{	# complete axes lists (if needed)
	axes <- complete.axes(axes)
	# complete format vector (if needed)
	if(length(format)==0)
		format = NA
	
	# deal with symbolic x values
	tmp <- clean.symbolic.data.sec(axes$x, x.values) #TODO
	x.values <- tmp$values
	axes$x <- tmp$axis
	#TODO same thing for z
		
	# clean numerical y values
	y.values <- clean.numerical.data.sec(y.values)
	
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
			{	pdf(file=plot.filename,bg="white")
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
# Plots the specified multiple series as a histogram. The y axis correspond to absolute values 
# or proportions, depending on the corresponding parameter. Each bar corresponds to one categorical 
# or integer value. One bar breaks down to several colors, each one representing one series.
#
# plot.file: path and name of the generated files.
# x.values: vectors of symbols or integer values.
# ly.values: list of vector of numerical values.
# axes: info about the plot axes. It is an optional list containing two lists named x and y.
#		The y list contains two optional fields range and title. Field range corresponds to the 
#		forced range for the y axis. It is a vector containing two values corresponding
#		to the min anx max. An NA symbol means the min and/or max is not specified. If the min
#		or max is not specified, we use the min or max processed over all considered series. 
#		Field title is the name of the y axis on the plot. The x list contains two fields title
#		and values. The formder is similar to that of the y list. The latter is a vector
#		containing all values of x to appear in the plot, in the specified order.
# plot.title: general title of the plot (NA for no title at all).
# format: vector of formats of the generated files (PDF and/or PNG).
#############################################################################################
plot.multiple.indiv.histo <- function(plot.file, x.values, ly.values, axes=list(), plot.title, format=c("PDF","PNG"))
{	
	
	
}


#############################################################################################
# Plots the specified single series as a histogram. The y axis correspond to absolute values 
# (by opposition to proportions) describing the series to be plot, and each x bar corresponds 
# to one categorical or integer value. The bars are grouped according to a second categorical
# or integer variable, named z.
#
# plot.file: path and name of the generated files.
# x.values: vector of symbols or integer values.
# y.values: vector of numerical values.
# z.values: vector of symbols or integer values.
# axes: info about the plot axes. It is an optional list containing two lists named x, y and z.
#		The y list contains two optional fields range and title. Field range corresponds to the 
#		forced range for the y axis. It is a vector containing two values corresponding
#		to the min anx max. An NA symbol means the min and/or max is not specified. If the min
#		or max is not specified, we use the min or max processed over all considered series. 
#		Field title is the name of the y axis on the plot. The x list contains two fields title
#		and values. The formder is similar to that of the y list. The latter is a vector
#		containing all values of x to appear in the plot, in the specified order. The z list 
#  		contains only a values field, which is similar to that of the x list.
# plot.title: general title of the plot (NA for no title at all).
# format: vector of formats of the generated files (PDF and/or PNG).
#############################################################################################
plot.single.group.histo <- function(plot.file, x.values, y.values, axes=list(), plot.title, format=c("PDF","PNG"))
{	
	
	
}


#############################################################################################
# Plots the specified multiple series as a histogram. The y axis correspond to absolute values 
# or proportions, depending on the corresponding parameter. Each bar corresponds to one categorical 
# or integer value. One bar breaks down to several colors, each one representing one series.
# The bars are grouped according to a second categorical or integer variable, named z.
#
# plot.file: path and name of the generated files.
# x.values: vectors of symbols or integer values.
# ly.values: list of vector of numerical values.
# z.values: vector of symbols or integer values.
# axes: info about the plot axes. It is an optional list containing two lists named x, y and z.
#		The y list contains two optional fields range and title. Field range corresponds to the 
#		forced range for the y axis. It is a vector containing two values corresponding
#		to the min anx max. An NA symbol means the min and/or max is not specified. If the min
#		or max is not specified, we use the min or max processed over all considered series. 
#		Field title is the name of the y axis on the plot. The x list contains two fields title
#		and values. The formder is similar to that of the y list. The latter is a vector
#		containing all values of x to appear in the plot, in the specified order. The z list 
#  		contains only a values field, which is similar to that of the x list.
# plot.title: general title of the plot (NA for no title at all).
# format: vector of formats of the generated files (PDF and/or PNG).
#############################################################################################
plot.multiple.group.histo <- function(plot.file, x.values, ly.values, axes=list(), plot.title, format=c("PDF","PNG"))
{	
	
	
}








#############################################################################################
# Plots the specified graph as a line chart. Each series is represented as a separate line
# with its own color. The plot scale is processed automatically depending on the values and
# optional dispersion values. 
#
# plot.file: path and name of the generated files.
# series:	data to plot, as a named list of series. Each series is itself a list containing 
#			some fields named x, y and stdev, which are vectors. All of them must have the 
#			same length, for a given series. Field stdev is optional, but x and y are not.
# axes: info about the plot axes. It is an optional list containing two lists named y and x.
#		Each list contains two optional fields range and title. Field range corresponds to the 
#		forced range for the considered axis. It is a vector containing two values corresponding
#		to the min and max. An NA symbol means the min and/or max is not specified. If the min
#		or max is not specified, we use the min or max processed over all considered series. 
#		Field title is the name of the considered axis on the plot.
# plot.title: general title of the plot (NA for no title at all).
# with.lines:	whether to draw lines between points, or not.
# format: vector of formats of the generated files (PDF and/or PNG).
#############################################################################################
plot.lines <- function(plot.file, series, axes=list(), plot.title, with.lines=TRUE, consider.stdev=TRUE, format=c("PDF","PNG"))
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
			{	pdf(file=plot.filename,bg="white")
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
## Tests
#############################################################################################
plot.file <- "temp"
plot.title <- "Graph Title"
x1 <- 1:10
x2 <- 1:20
x3 <- 1:25
series <- list(
		"1st Series"=list(x=x1, y=(2*x1+1), stdev=c(runif(length(x1)))),
		"2nd Series"=list(x=x2, y=(x2/2+10), stdev=c(runif(length(x2)))),
		"3rd Series"=list(x=x3, y=(x3^2/4), stdev=c(runif(length(x3))))
)
axes <- list(
		x=list(range=c(NA,25), title="x axis"),
		y=list(range=c(NA,NA), title="y axis")
)
plot.lines(plot.file, series, axes, plot.title, with.lines=TRUE, consider.stdev=TRUE, format=c("PDF","PNG",NA))
