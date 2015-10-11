#############################################################################################
# Functions used to generate bar plots, possibly stacked and possibly grouped (i.e. up to
# three categorical variables).
# 
# 10/2015 Vincent Labatut
#############################################################################################
source("src/plot-tools/plot-common.R")




#############################################################################################
# Plots the specified single series as a bar plot. Each bar corresponds to one categorical 
# or integer value specified in the parameter bar.names, and its height depends on the count 
# processed on the values vector.
#
# plot.file: path and name of the generated files.
# bar.names: vector of symbols or integer values used to name the individual bars.
# values: vector of raw values, which need to be counted to determine the bar heights.
# x.label: general title of the bars. 
# plot.title: general title of the plot (NA for no title at all).
# format: vector of formats of the generated files (PDF and/or PNG, NA for the screen).
#############################################################################################
plot.unif.indiv.raw.bars <- function(plot.file, bar.names, values, x.label, plot.title, format=c("PDF","PNG",NA))
{	counts <- c()
	dispersion <- NA
	
	# if values is a list, we average the counts
	if(is.list(values))
	{	mcounts <- t(sapply(values, function(v) as.vector(table(factor(v, levels=bar.names), useNA="no"))))
		counts <- apply(mcounts, 2, mean)
		dispersion <- apply(mcounts, 2, sd)
	}
	# otherwise, we directly count the values
	else
	{	counts <- as.vector(table(factor(values, levels=bar.names), useNA="no"))
		dispersion <- NA
	}
	
	# use the other function to perfom the actual plot
	plot.unif.indiv.count.bars(plot.file, bar.names, counts, dispersion, x.label, y.label="Count", plot.title, format)
}


#############################################################################################
# Plots the specified single series as a bar plot. Each bar corresponds to one categorical 
# or integer value specified in the parameter bar.names, and its height depends on the specified
# count. An optional dispersion parameter allows adding dispersion bars.
#
# plot.file: path and name of the generated files.
# bar.names: vector of symbols or integer values used to name the individual bars.
# counts: vector of counts used to to determine the bar heights. If this parameter is a list of
# 		  vectors instead of a single vector, then the counts are processed for each vector and
#		  then averaged.
# dispersion: optional vector of dispersion values (one for each count value).
# x.label: general title of the bars. 
# y.label: general title of the bar heights. 
# plot.title: general title of the plot (NA for no title at all).
# format: vector of formats of the generated files (PDF and/or PNG, NA for the screen).
#############################################################################################
plot.unif.indiv.count.bars <- function(plot.file, bar.names, counts, dispersion=NA, x.label, y.label, plot.title, format=c("PDF","PNG",NA))
{	# process each specified format
	for(frmt in format)
	{	# set plot file name
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
		
		# do the actual plotting
		# (old version)
			#barplot(count, main=plot.title, xlab=bars.label)
		# create the plot
		temp <- data.frame(cats=bar.names, counts=counts)
		p <- ggplot(data=temp, aes(x=factor(cats),y=counts))
		# change the theme
		p <- p + theme_bw() + theme(
			#axis.text.x=element_text(angle = 90, hjust = 1), 
			panel.grid.major=element_line(colour = "grey90"),
			panel.grid.minor=element_line(colour = "grey96"),
			panel.background=element_blank(),
			axis.ticks=element_blank(), 
			legend.position="none"
		)
		# add the bars
		p <- p + geom_bar(stat="identity", color="black", fill="red")
		p <- p + geom_text(aes(y=counts, label=counts), vjust=1.6, color="white", size=3.5)
		# possiby add title and labels
		if(!is.na(plot.title))
			p <- p + ggtitle(plot.title)
		if(!is.na(x.label))
			p <- p + xlab(x.label)
		if(!is.na(y.label))
			p <- p + ylab(y.label)
		# possibly add dispersion bars
		if(length(dispersion)>0 && !is.na(dispersion))
		{	temp[["upper"]] <- counts + dispersion
			p <- p + geom_errorbar(data=temp, mapping=aes(ymin=counts, ymax=upper), width=.2)
		}
		print(p)
		
		# finalize plot file
		if(!is.na(frmt))
			dev.off()
	}
}


#############################################################################################
# Plots the specified multiple series as a stacked bar plot. Each bar corresponds to one categorical 
# or integer value specified in the parameter bar.names, and its height depends on the specified
# count. Each color corresponds to an other categorical or integer value specified in the color.names
# parameter. The value vectors are supposed to contain the symbols from color.names (not bar.names).
#
# plot.file: path and name of the generated files.
# bar.names: vector of symbols or integer values used to name the individual bars.
# color.names: vector of symbols or integer values used to name the stacked color forming the bars.
# values: list of vectors of raw values, which need to be counted to determine the bar heights.
#      	  Each vector corresponds to one stacked bar. If this parameter is a list of lists of 
# 		  vectors instead of a single list of vectors, then the counts are processed for each vector 
#		  in a sublist, and then averaged to get a single count vector.
# x.label: general title of the bars. 
# colors.label: title of the color legend.
# plot.title: general title of the plot (NA for no title at all).
# format: vector of formats of the generated files (PDF and/or PNG, NA for the screen).
#############################################################################################
plot.stacked.indiv.raw.bars <- function(plot.file, bar.names, color.names, values, x.label, plot.title, format=c("PDF","PNG",NA))
{	counts <- list()
	dispersion <- NA
	
	# if values is a list of lists, we average the counts
	if(is.list(values[[1]]))
	{	dispersion <- list()
		for(l in 1:length(values))
		{	mcounts <- t(sapply(values[[l]], function(v) as.vector(table(factor(v, levels=color.names), useNA="no"))))
			#print(mcounts)
			counts[[l]] <- apply(mcounts, 2, mean)
			dispersion[[l]] <- apply(mcounts, 2, sd)
		}
	}
	# otherwise, we directly count the values
	else
	{	counts <- lapply(values, function(v) as.vector(table(factor(v, levels=color.names), useNA="no")))
		dispersion <- NA
	}
	
	# use the other function to perfom the actual plot
	#print(counts)
	plot.stacked.indiv.count.bars(plot.file, bar.names, color.names, counts, dispersion, x.label, y.label="Count", colors.label, plot.title, format)
}


#############################################################################################
# Plots the specified multiple series as a stacked bar plot. Each bar corresponds to one categorical 
# or integer value specified in the parameter bar.names, and its height depends on the specified
# count. Each color corresponds to an other categorical or integer value specified in the color.names
# parameter. Each vector in the values list correspond to a bar. The values composing a vector correspond
# to the stacked colors. An optional dispersion parameter allows adding dispersion bars.
#
# plot.file: path and name of the generated files.
# bar.names: vector of symbols or integer values used to name the individual bars.
# color.names: vector of symbols or integer values used to name the stacked color forming the bars.
# counts: list of vectors of counts used to to determine the bar heights.
# dispersion: optional list of vectors of dispersion values (one for each count value).
# x.label: general title of the bars. 
# y.label: title of the bar heights. 
# colors.label: title of the color legend 
# plot.title: general title of the plot (NA for no title at all).
# format: vector of formats of the generated files (PDF and/or PNG, NA for the screen).
#############################################################################################
plot.stacked.indiv.count.bars <- function(plot.file, bar.names, color.names, counts, dispersion=NA, x.label, y.label, colors.label, plot.title, format=c("PDF","PNG",NA))
{	# process each specified format
	for(frmt in format)
	{	# set plot file name
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
		
		# create the data frame
		col.names <- c()
		col.colors <- c()
		col.counts <- c()
		col.dispersion <- c()
		col.cumul <- c()
		for(s in 1:length(counts))
		{	bar.name <- bar.names[s]
			col.names <- c(col.names,rep(bar.name,length(counts[[s]])))
			col.colors <- c(col.colors, color.names)
			col.counts <- c(col.counts, counts[[s]])
			cs <- cumsum(counts[[s]])
			col.cumul <- c(col.cumul, cs)
			if(length(dispersion)>0 && !is.na(dispersion))
				col.dispersion <- c(col.dispersion, cs + dispersion[[s]])
		}
		#print(col.names);print(col.counts);print(col.colors)
		col.label <- col.counts
		col.label[col.label==0] <- NA
		temp <- data.frame(cats=col.names, counts=col.counts, clrs=col.colors, cumul=col.cumul, lbl=col.label)
		if(length(dispersion)>0 && !is.na(dispersion))
			temp[["upper"]] <- col.dispersion
		#print(temp)
		# init the plot
		p <- ggplot(data=temp, aes(x=factor(cats),y=counts,fill=factor(clrs)))
		# change the theme
		p <- p + theme_bw() + theme(
				#axis.text.x=element_text(angle = 90, hjust = 1), 
				panel.grid.major=element_line(colour = "grey90"),
				panel.grid.minor=element_line(colour = "grey96"),
				panel.background=element_blank(),
				axis.ticks=element_blank()
				#legend.position="none"
		)
		# add the bars
		p <- p + geom_bar(stat="identity", color="black")
		p <- p + geom_text(aes(y=cumul, label=lbl), vjust=1.6, color="white", size=3.5)
		# possiby add title and labels
		if(!is.na(plot.title))
			p <- p + ggtitle(plot.title)
		if(!is.na(x.label))
			p <- p + xlab(x.label)
		if(!is.na(y.label))
			p <- p + ylab(y.label)
		if(!is.na(colors.label))
			p <- p + scale_fill_discrete(name=colors.label)   
		# possibly add dispersion bars
		if(length(dispersion)>0 && !is.na(dispersion))
			p <- p + geom_errorbar(mapping=aes(ymin=cumul, ymax=upper), width=.2)
		print(p)
		
		# finalize plot file
		if(!is.na(frmt))
			dev.off()
	}
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
# format: vector of formats of the generated files (PDF and/or PNG, NA for the screen).
#############################################################################################
plot.unif.grouped.bars <- function(plot.file, x.values, y.values, axes=list(), plot.title, format=c("PDF","PNG",NA))
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
# format: vector of formats of the generated files (PDF and/or PNG, NA for the screen).
#############################################################################################
plot.stacked.grouped.bars <- function(plot.file, x.values, ly.values, axes=list(), plot.title, format=c("PDF","PNG",NA))
{	
	
	
}










#############################################################################################
## Tests
#############################################################################################
plot.file <- "temp"
format <- c("PDF","PNG", NA)
plot.title <- "Graph Title"
#############################################################################################
### test uniform individual plot from count
#bar.names <- c("A", "B", "C", "D")
#counts <- c(10, 20, 0, 4)
##dispersion <- NA
#dispersion <- c(1,0,0.5,4)
#x.label <- "Categories"
#y.label <- "Values"
##plot.unif.indiv.count.bars(plot.file, bar.names, counts, dispersion, x.label, y.label, plot.title, format)
### test uniform individual plot from raw data
##values <- c("A", "D", "D", "C", "C", "D", "D", "D", "B", "B", NA, "D", "D", "D", "D")
##plot.unif.indiv.raw.bars(plot.file, bar.names, values, x.label, plot.title, format)
### test uniform individual plot from multiple raw data
#values <- list()
#values[[1]] <- c("A", "D", "D", "C", "C", "D", "D", "D", "B", "B", NA, "D", "D", "D", "D")
#values[[2]] <- c("A", "A", "A", "C", "C", "A", "A", "D", "B", "B", "B", "A", "A", "D", "D")
#plot.unif.indiv.raw.bars(plot.file, bar.names, values, x.label, plot.title, format)
#############################################################################################
## test stacked individual plot from count
bar.names <- c("A", "B", "C", "D")
color.names <- c("x", "y", "z")
counts <- list(
	c(10, 0, 4),
	c(1, 10, 3),
	c(1, 0, 4),
	c(6, 2, 4)
)
#dispersion <- NA
dispersion <- list(
	c(0.8,0,2),
	c(0.5,0.5,3),
	c(0.2,0,0.75),
	c(0.5,1,2)
)
x.label <- "Categories"
y.label <- "Values"
colors.label <- "Colors"
#plot.stacked.indiv.count.bars(plot.file, bar.names, color.names, counts, dispersion, x.label, y.label, colors.label, plot.title, format)
# test stacked individual plot from raw data
#values <- list()
#values[[1]] <- c("x", "x", "x", "x", "y", "y", "y", "z", "z", "z", NA, "x")
#values[[2]] <- c("y", "x", "z", "x", "z", "z", "z", "y", "x", "z", NA, "y", "x", "y", "x")
#values[[3]] <- c("y", "x", "x", "x", "y", "z", "y")
#values[[4]] <- c("x", "z", "z", "y", "z", "y", "z", "y", "z", "z", NA, "x", "x", "y", "y")
#plot.stacked.indiv.raw.bars(plot.file, bar.names, color.names, values, x.label, plot.title, format)
#c("x","y","z")[round(runif(10,min=1,max=3))]
## test stacked individual plot from multiple raw data
values <- list(); values[[1]] <- list(); values[[2]] <- list(); values[[3]] <- list(); values[[4]] <- list()
values[[1]][[1]] <- c("x", "x", "x", "x", "y", "y", "y", "z", "z", "z", NA, "x")
values[[1]][[2]] <- c("x", "y", "x", "x", "y", "y", "y", "z", "z", "z", "x", "x","x","x")
values[[2]][[1]] <- c("y", "x", "z", "x", "z", "z", "z", "y", "x", "z", NA, "y", "x", "y", "x")
values[[2]][[2]] <- c("y", "x", "z", "x", "z", "z", "z", "y", "x", "z", "z", "z", "z", "z", "x", "y", "y", "y")
values[[3]][[1]] <- c("y", "x", "x", "x", "y", "z", "y", "y", "y", "y", "y")
values[[3]][[2]] <- c("y", "x", "x", "x", "y", "z", "y")
values[[4]][[1]] <- c("x", "z", "z", "y", "z", "y", "z", "y", "z", "z", NA, "x", "x", "y", "y")
values[[4]][[2]] <- c("x", "z", "z", "y", "z", "y", "z", "y", "z", "z", NA, "x", "x", "y", "y", "x", "x", "x")
plot.stacked.indiv.raw.bars(plot.file, bar.names, color.names, values, x.label, plot.title, format)
