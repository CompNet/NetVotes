#############################################################################################
# Function used to generate histograms, i.e. plot a frequency in function of a discretized
# continuous variable.
# 
# 10/2015 Vincent Labatut
#############################################################################################
source("src/plot-tools/plot-common.R")



#############################################################################################
# Plots the specified series as a histogram. The y axis corresponds to absolute values 
# or proportions depending on the selected option. Each x bar corresponds to one discrete value
# or discretized bin.
#
# plot.file: path and name of the generated files.
# values: vector of real values.
# x.label: general title of the bars. 
# proportions: whether to plot actual counts or proportions.
# x.lim: limits for the x axis (optional).
# y.max: limit for the y axis (optional).
# break.nbr: number of bins in the histogram.
# plot.title: general title of the plot (NA for no title at all).
# format: vector of formats of the generated files (PDF and/or PNG).
#############################################################################################
plot.histo <- function(plot.file, values, x.label, proportions=TRUE, x.lim=c(NA,NA), y.max=NA, break.nbr=NA, plot.title, format=c("PDF","PNG",NA))
{	# possibly complete the x axis ranges
	if(is.na(x.lim[1]) & !is.na(x.lim[2]))
		x.lim[1] <- min(values)
	else if(!is.na(x.lim[1]) & is.na(x.lim[2]))
		x.lim[2] <- max(values)
	
	# set up the number of bars
	if(is.na(break.nbr))
		break.nbr <- 30
	
	# format the data
	temp <- data.frame(vals=values)
	#print(temp)

	# process each specified format
	for(frmt in format)
	{	# set plot file name
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
		
		# create the plot
		p <- ggplot(data=temp, aes(x=vals))
		# add the histogram
		if(proportions)
		{	p <- p + geom_histogram(
				aes(y = ..count.. / sum(..count..)),
				binwidth=1/break.nbr,
				col="black",
				fill="red"
			)
			#p <- p + geom_density(alpha=.2)
			if(is.na(y.max))
				p <- p + scale_y_continuous(labels=percent)
			else
				p <- p + scale_y_continuous(labels=percent, limits=c(0,y.max))
		}
		else
		{	p <- p + geom_histogram(
				aes(y = ..count..),
				binwidth=1/break.nbr,
				col="black",
				fill="red"
			)
			if(!is.na(y.max))
				p <- p + ylim(c(0,y.max))
		}
		# set up the plot limits
		if(!any(is.na(x.lim)))
			p <- p + xlim(x.lim)
		# possiby add title and labels
		if(!is.na(plot.title))
			p <- p + ggtitle(plot.title)
		if(!is.na(x.label))
			p <- p + xlab(x.label)
		if(proportions)
			p <- p + ylab("Percents")
		else
			p <- p + ylab("Counts")
		print(p)
		
		# finalize plot file
		if(!is.na(frmt))
			dev.off()
	}
	
	result <- ggplot_build(p)$data[[1]]
	#print(result)
	return(result)
}


#############################################################################################
## Tests
#############################################################################################
#plot.file <- "temp"
#format <- c("PDF","PNG", NA)
#plot.title <- "Graph Title"
#proportions <- TRUE
#values <- runif(1000)
#x.label <- "Bins"
#x.lim <- c(0,1)
#y.max <- 1
#break.nbr <- 10
#plot.histo(plot.file, values, x.label, proportions, x.lim, y.max, break.nbr, plot.title, format)
