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
# values: vector of raw values, which need to be counted to determine the bar heights. If this 
#		  parameter is a list of vectors instead of a single vector, then the counts are processed 
#		  for each vector and then averaged.
# proportions: whether to plot actual counts or proportions.
# areas: whether to plot separate bars or continuous areas.
# y.lim: limits for the y axis (optional).
# x.label: general title of the bars. 
# plot.title: general title of the plot (NA for no title at all).
# x.rotate: whether the x axis labels should be rotated by 90°, or not. 
# format: vector of formats of the generated files (PDF and/or PNG, NA for the screen).
# returns: the counts processed in this function from the original values.
#############################################################################################
plot.unif.indiv.raw.bars <- function(plot.file, bar.names, values, proportions=TRUE, areas=FALSE, y.lim=c(NA,NA), x.label, plot.title, x.rotate=FALSE, format=c("PDF","PNG",NA))
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

	# set up label for y axis
	if(proportions)
		y.label <- "Percent"
	else
		y.label <- "Count"
	
	# use the other function to perfom the actual plot
	#print(counts)
	#print(dispersion)
	plot.unif.indiv.count.bars(plot.file, bar.names, counts, dispersion, proportions, areas, y.lim, x.label, y.label, plot.title, x.rotate, format)
	return(counts)
}



#############################################################################################
# Plots the specified single series as a bar plot. Each bar corresponds to one categorical 
# or integer value specified in the parameter bar.names, and its height depends on the specified
# count. An optional dispersion parameter allows adding dispersion bars.
#
# plot.file: path and name of the generated files.
# bar.names: vector of symbols or integer values used to name the individual bars.
# counts: vector of counts used to to determine the bar heights. 
# dispersion: optional vector of dispersion values (one for each count value).
# proportions: whether to plot actual counts or proportions.
# areas: whether to plot separate bars or continuous areas.
# y.lim: limits for the y axis (optional).
# x.label: general title of the bars. 
# y.label: general title of the bar heights. 
# plot.title: general title of the plot (NA for no title at all).
# x.rotate: whether the x axis labels should be rotated by 90°, or not. 
# format: vector of formats of the generated files (PDF and/or PNG, NA for the screen).
#############################################################################################
plot.unif.indiv.count.bars <- function(plot.file, bar.names, counts, dispersion=NA, proportions=TRUE, areas=FALSE, y.lim=c(NA,NA), x.label, y.label, plot.title, x.rotate=FALSE, format=c("PDF","PNG",NA))
{	#print(counts)
	# possibly complete the y axis ranges
	if(is.na(y.lim[1]) & !is.na(y.lim[2]))
	{	if(proportions)
			y.lim[1] <- min(counts) / sum(counts)
		else
			y.lim[1] <- min(counts)
	}
	else if(!is.na(y.lim[1]) & is.na(y.lim[2]))
	{	if(proportions)
			y.lim[2] <- max(counts) / sum(counts)
		else
			y.lim[2] <- max(counts)
	}
	
	# init the data frame
	col.names <- bar.names
	col.counts <- c()
	col.dispersion <- c()
	if(proportions)
	{	col.counts <- counts / sum(counts)
		col.counts[is.infinite(col.counts) | is.nan(col.counts)] <- 0
		if(length(dispersion)>0 && !is.na(dispersion))
		{	col.dispersion <- dispersion / sum(counts)
			col.dispersion[is.infinite(col.dispersion) | is.nan(col.dispersion)] <- 0
		}			
	}
	else
	{	col.counts <- counts
		if(length(dispersion)>0 && !is.na(dispersion))
			col.dispersion <- dispersion
	}
	col.label <- col.counts
	if(proportions)
		col.label <- round(col.label*100)
	col.label[col.label==0] <- NA
	#print(col.counts)	
	temp <- data.frame(cats=col.names, cnts=col.counts, lbl=col.label)
	if(length(dispersion)>0 && !is.na(dispersion))
		temp[["upper"]] <- col.counts + col.dispersion
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
			{	pdf(file=plot.filename,bg="white")
			}
		}
		
		# create the plot
		if(areas)
			#p <- ggplot(data=temp, aes(x=sort(unique((cats))),y=cnts))
			p <- ggplot(data=temp, aes(x=cats,y=cnts))
		else
			p <- ggplot(data=temp, aes(x=factor(cats),y=cnts)) + scale_x_discrete(limits=bar.names)
		# change the theme
		p <- p + theme_bw() + theme(
			panel.grid.major=element_line(colour = "grey90"),
			panel.grid.minor=element_line(colour = "grey96"),
			panel.background=element_blank(),
			axis.ticks=element_blank(), 
			legend.position="none"
		)
		# possibly rotate the x axis labels
		if(x.rotate)
			p <- p + theme(axis.text.x=element_text(angle=90,hjust=1))
		# add the bars
		if(areas)
		{	p <- p + geom_area(stat="identity", color="black", fill="red")
		}
		else
		{	p <- p + geom_bar(stat="identity", color="black", fill="red")
			p <- p + geom_text(aes(y=cnts, label=lbl), vjust=1.6, color="white", size=3.5)
		}
		# set up the y limits
		if(proportions)
		{	if(any(is.na(y.lim)))
				p <- p + scale_y_continuous(labels=percent)
			else
				p <- p + scale_y_continuous(labels=percent, limits=y.lim)
		}
		else
		{	if(!any(is.na(y.lim)))
				p <- p + ylim(y.lim)
		}
		# possiby add title and labels
		if(!is.na(plot.title))
			p <- p + ggtitle(plot.title)
		if(!is.na(x.label))
			p <- p + xlab(x.label)
		if(!is.na(y.label))
			p <- p + ylab(y.label)
		# possibly add dispersion bars
		if(length(dispersion)>0 && !is.na(dispersion))
			p <- p + geom_errorbar(data=temp, mapping=aes(ymin=cnts, ymax=upper), width=.2)
		print(p)
		
		# finalize plot file
		if(!is.na(frmt))
			dev.off()
	}
}



#############################################################################################
# Plots the specified multiple series as a stacked bar plot. Each bar corresponds to one categorical 
# or integer value specified in the parameter bar.names, and its height depends on the specified
# values. Each color corresponds to an other categorical or integer value specified in the color.names
# parameter. The value vectors are supposed to contain the symbols from color.names (not bar.names).
#
# plot.file: path and name of the generated files.
# bar.names: vector of symbols or integer values used to name the individual bars.
# color.names: vector of symbols or integer values used to name the stacked colors forming the bars.
# values: list of vectors of raw values, which need to be counted to determine the bar heights.
#      	  Each vector corresponds to one stacked bar. If this parameter is a list of lists of 
# 		  vectors instead of a single list of vectors, then the counts are processed for each vector 
#		  in a sublist, and then averaged to get a single count vector.
# proportions: whether to plot actual counts or proportions.
# areas: whether to plot separate bars or continuous areas.
# y.lim: limits for the y axis (optional).
# x.label: general title of the bars. 
# colors.label: title of the color legend.
# plot.title: general title of the plot (NA for no title at all).
# x.rotate: whether the x axis labels should be rotated by 90°, or not. 
# format: vector of formats of the generated files (PDF and/or PNG, NA for the screen).
# returns: the counts processed in this function from the original values.
#############################################################################################
plot.stacked.indiv.raw.bars <- function(plot.file, bar.names, color.names, values, proportions=TRUE, areas=FALSE, y.lim=c(NA,NA), x.label, colors.label, plot.title, x.rotate=FALSE, format=c("PDF","PNG",NA))
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
	
	# set up label for y axis
	if(proportions)
		y.label <- "Percent"
	else
		y.label <- "Count"
	
	# use the other function to perfom the actual plot
	#print(plot.title)
	#print(counts)
	plot.stacked.indiv.count.bars(plot.file, bar.names, color.names, counts, dispersion, proportions, areas, y.lim, x.label, y.label, colors.label, plot.title, x.rotate, format)
	return(counts)
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
# color.names: vector of symbols or integer values used to name the stacked colors forming the bars.
# counts: list of vectors of counts used to to determine the bar heights.
# dispersion: optional list of vectors of dispersion values (one for each count value).
# proportions: whether to plot actual counts or proportions.
# areas: whether to plot separate bars or continuous areas.
# y.lim: limits for the y axis (optional).
# x.label: general title of the bars. 
# y.label: title of the bar heights. 
# colors.label: title of the color legend 
# plot.title: general title of the plot (NA for no title at all).
# x.rotate: whether the x axis labels should be rotated by 90°, or not. 
# format: vector of formats of the generated files (PDF and/or PNG, NA for the screen).
#############################################################################################
plot.stacked.indiv.count.bars <- function(plot.file, bar.names, color.names, counts, dispersion=NA, proportions=TRUE, areas=FALSE, y.lim=c(NA,NA), x.label, y.label, colors.label, plot.title, x.rotate=FALSE, format=c("PDF","PNG",NA))
{	# possibly complete the y axis ranges
	if(is.na(y.lim[1]) & !is.na(y.lim[2]))
	{	if(proportions)
			y.lim[1] <- 0 #TODO not tested
		else
			y.lim[1] <- min(sapply(counts, sum)) 
	}
	else if(!is.na(y.lim[1]) & is.na(y.lim[2]))
	{	if(proportions)
			y.lim[2] <- 1 #TODO not tested
		else
			y.lim[2] <- max(sapply(counts, sum))
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
		if(proportions)
		{	tc <- counts[[s]] / sum(counts[[s]])
			tc[is.infinite(tc) | is.nan(tc)] <- 0
			if(length(dispersion)>0 && !is.na(dispersion))
			{	td <- dispersion[[s]] / sum(counts[[s]])
				td[is.infinite(td) | is.nan(td)] <- 0
			}				
		}
		else
		{	tc <- counts[[s]]
			if(length(dispersion)>0 && !is.na(dispersion))
				td <- dispersion[[s]]
		}
		col.counts <- c(col.counts, tc)
		cs <- cumsum(tc)
		col.cumul <- c(col.cumul, cs)
		if(length(dispersion)>0 && !is.na(dispersion))
			col.dispersion <- c(col.dispersion, cs + td)
	}
	#print(col.names);print(col.counts);print(col.colors)
	col.label <- col.counts
	if(proportions)
		col.label <- round(col.label*100)
	col.label[col.label==0] <- NA
	temp <- data.frame(cats=col.names, cnts=col.counts, clrs=col.colors, cumul=col.cumul, lbl=col.label)
	if(length(dispersion)>0 && !is.na(dispersion))
		temp[["upper"]] <- col.dispersion
	#print(counts)
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
			{	pdf(file=plot.filename,bg="white")
			}
		}
		
		# init the plot
		if(areas)
			p <- ggplot(data=temp, aes(x=cats,y=cnts,fill=clrs))
		else
			p <- ggplot(data=temp, aes(x=cats,y=cnts,fill=factor(clrs))) + scale_x_discrete(limits=bar.names)
		# change the theme
		p <- p + theme_bw() + theme(
				panel.grid.major=element_line(colour = "grey90"),
				panel.grid.minor=element_line(colour = "grey96"),
				panel.background=element_blank(),
				axis.ticks=element_blank()
				#legend.position="none"
		)
		# possibly rotate the x axis labels
		if(x.rotate)
			p <- p + theme(axis.text.x=element_text(angle=90,hjust=1))
		# add the bars
		if(areas)
		{	p <- p + geom_area(stat="identity", color="black")
			#print(temp)
		}
		else
		{	p <- p + geom_bar(stat="identity", color="black")
			p <- p + geom_text(aes(y=cumul, label=lbl), vjust=1.6, color="white", size=3.5)
		}
		# set up the y limits
		if(proportions)
		{	if(any(is.na(y.lim)))
				p <- p + scale_y_continuous(labels=percent)
			else
				p <- p + scale_y_continuous(labels=percent, limits=y.lim)
		}
		else
		{	if(!any(is.na(y.lim)))
				p <- p + ylim(y.lim)
		}
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
		if(length(dispersion)>0 && !is.na(dispersion) && !areas)
			p <- p + geom_errorbar(mapping=aes(ymin=cumul, ymax=upper), width=.2)
		print(p)
		#print(ggplot_build(p)$data[[1]])
		
		
		# finalize plot file
		if(!is.na(frmt))
			dev.off()
	}
}



#############################################################################################
# Plots the specified multiple series as a groupped bar plot. Each group corresponds to one categorical 
# or integer value specified in the parameter bar.names, and its height depends on the specified
# values. Each bar in a group corresponds to an other categorical or integer value specified in the 
# bar.names parameter. The value vectors are supposed to contain the symbols from bar.names (not group.names).
#
# plot.file: path and name of the generated files.
# group.names: vector of symbols or integer values used to name the groups of bars.
# bar.names: vector of symbols or integer values used to name the individual bars.
# values: list of vectors of raw values, which need to be counted to determine the bar heights.
#      	  Each vector corresponds to one group of bars. If this parameter is a list of lists of 
# 		  vectors instead of a single list of vectors, then the counts are processed for each vector 
#		  in a sublist, and then averaged to get a single count vector.
# proportions: whether to plot actual counts or proportions.
# y.lim: limits for the y axis (optional).
# x.label: general title of the bars. 
# plot.title: general title of the plot (NA for no title at all).
# x.rotate: whether the x axis labels should be rotated by 90°, or not. 
# format: vector of formats of the generated files (PDF and/or PNG, NA for the screen).
# returns: the counts processed in this function from the original values.
#############################################################################################
plot.unif.grouped.raw.bars <- function(plot.file, group.names, bar.names, values, proportions, y.lim=c(NA,NA), x.label, plot.title, x.rotate=FALSE, format=c("PDF","PNG",NA))
{	counts <- list()
	dispersion <- NA
	
	# if values is a list of lists, we average the counts
	if(is.list(values[[1]]))
	{	dispersion <- list()
		for(l in 1:length(values))
		{	mcounts <- t(sapply(values[[l]], function(v) as.vector(table(factor(v, levels=bar.names), useNA="no"))))
			#print(mcounts)
			counts[[l]] <- apply(mcounts, 2, mean)
			dispersion[[l]] <- apply(mcounts, 2, sd)
		}
	}
	# otherwise, we directly count the values
	else
	{	counts <- lapply(values, function(v) as.vector(table(factor(v, levels=bar.names), useNA="no")))
		dispersion <- NA
	}
	
	# set up label for y axis
	if(proportions)
		y.label <- "Percent"
	else
		y.label <- "Count"
	
	# use the other function to perfom the actual plot
	#print(counts)
	plot.unif.grouped.count.bars(plot.file, group.names, bar.names, counts, dispersion, proportions, y.lim, x.label, y.label, plot.title, x.rotate, format)
	return(counts)
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
# color.names: vector of symbols or integer values used to name the stacked colors forming the bars.
# counts: list of vectors of counts used to to determine the bar heights.
# dispersion: optional list of vectors of dispersion values (one for each count value).
# proportions: whether to plot actual counts or proportions.
# y.lim: limits for the y axis (optional).
# x.label: general title of the bars. 
# y.label: title of the bar heights. 
# colors.label: title of the color legend 
# plot.title: general title of the plot (NA for no title at all).
# x.rotate: whether the x axis labels should be rotated by 90°, or not. 
# format: vector of formats of the generated files (PDF and/or PNG, NA for the screen).
#############################################################################################
plot.unif.grouped.count.bars  <- function(plot.file, group.names, bar.names, counts, dispersion=NA, proportions=TRUE, y.lim=c(NA,NA), x.label, y.label, plot.title, x.rotate=FALSE, format=c("PDF","PNG",NA))
{	# possibly complete the y axis ranges
	if(is.na(y.lim[1]) & !is.na(y.lim[2]))
	{	temp <- sapply(counts, min)
		idx <- which.min(temp)
		if(proportions)
			y.lim[1] <- temp[idx] / sapply(counts, sum)[idx]
		else
			y.lim[1] <- temp[idx] 
	}
	else if(!is.na(y.lim[1]) & is.na(y.lim[2]))
	{	temp <- sapply(counts, max)
		idx <- which.max(temp)
		if(proportions)
			y.lim[2] <- temp[idx] / sapply(counts, sum)[idx]
		else
			y.lim[2] <- temp[idx] 
	}
	
	# create the data frame
	col.groups <- c()
	col.bars <- c()
	col.counts <- c()
	col.dispersion <- c()
	col.cumul <- c()
	for(s in 1:length(counts))
	{	group.name <- group.names[s]
		col.groups <- c(col.groups,rep(group.name,length(counts[[s]])))
		col.bars <- c(col.bars, bar.names)
		if(proportions)
		{	tc <- counts[[s]] / sum(counts[[s]])
			tc[is.infinite(tc) | is.nan(tc)] <- 0
			if(length(dispersion)>0 && !is.na(dispersion))
			{	td <- dispersion[[s]] / sum(counts[[s]])
				td[is.infinite(td) | is.nan(td)] <- 0
			}
		}
		else
		{	tc <- counts[[s]]
			if(length(dispersion)>0 && !is.na(dispersion))
				td <- dispersion[[s]]
		}
		col.counts <- c(col.counts, tc)
		if(length(dispersion)>0 && !is.na(dispersion))
			col.dispersion <- c(col.dispersion, tc + td)
	}
	#print(col.groups);print(col.counts);print(col.bars)
	col.label <- col.counts
	if(proportions)
		col.label <- round(col.label*100)
	col.label[col.label==0] <- NA
	temp <- data.frame(grps=col.groups, counts=col.counts, bars=col.bars, lbl=col.label)
	if(length(dispersion)>0 && !is.na(dispersion))
		temp[["upper"]] <- col.dispersion
	#print(counts)
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
			{	pdf(file=plot.filename,bg="white")
			}
		}
		
		# init the plot
		p <- ggplot(data=temp, aes(x=factor(grps),y=counts,fill=factor(bars)))
		p <- p + scale_x_discrete(limits=bar.names) #TODO not tested
		# change the theme
		p <- p + theme_bw() + theme(
			panel.grid.major=element_line(colour = "grey90"),
			panel.grid.minor=element_line(colour = "grey96"),
			panel.background=element_blank(),
			axis.ticks=element_blank(),
			legend.position="none"
		)
		# possibly rotate the x axis labels
		if(x.rotate)
			p <- p + theme(axis.text.x=element_text(angle=90,hjust=1))
		# add the bars
		p <- p + geom_bar(stat="identity", color="black", position=position_dodge())
		p <- p + geom_text(aes(y=counts, label=lbl), vjust=1.6, color="white", size=3.5, position=position_dodge(.9))
		# set up the y limits
		if(proportions)
		{	if(any(is.na(y.lim)))
				p <- p + scale_y_continuous(labels=percent)
			else
				p <- p + scale_y_continuous(labels=percent, limits=y.lim)
		}
		else
		{	if(!any(is.na(y.lim)))
				p <- p + ylim(y.lim)
		}
		# possiby add title and labels
		if(!is.na(plot.title))
			p <- p + ggtitle(plot.title)
		if(!is.na(x.label))
			p <- p + xlab(x.label)
		if(!is.na(y.label))
			p <- p + ylab(y.label)
		# possibly add dispersion bars
		if(length(dispersion)>0 && !is.na(dispersion))
			p <- p + geom_errorbar(mapping=aes(ymin=counts, ymax=upper), width=.2, position=position_dodge(.9))
		print(p)
		
		# finalize plot file
		if(!is.na(frmt))
			dev.off()
	}
}



#############################################################################################
# Plots the specified multiple series as a stacked groupped bar plot. Each group corresponds to one 
# categorical or integer value specified in the parameter bar.names, and its height depends on the specified
# values. Each bar in a group corresponds to an other categorical or integer value specified in the 
# bar.names parameter. Each color in a bar corresponds to yet another categorical or integer value
# specified in the color.names parameter. The value vectors are supposed to contain the symbols from 
# color.names.
#
# plot.file: path and name of the generated files.
# group.names: vector of symbols or integer values used to name the groups of bars.
# bar.names: vector of symbols or integer values used to name the individual bars.
# color.names: vector of symbols or integer values used to name the stacked colors forming the bars.
# values: list of list of vectors of raw values, which need to be counted to determine the bar heights.
#      	  Each high level list corresponds to a group, each vector it contains is a bar in this group.
# 		  If this parameter is a list of lists of lists of vectors instead, then the counts are processed 
#		  for each vector in a sublist, and then averaged to get a single count vector.
# proportions: whether to plot actual counts or proportions.
# y.lim: limits for the y axis (optional).
# x.label: general title of the bars. 
# plot.title: general title of the plot (NA for no title at all).
# x.rotate: whether the x axis labels should be rotated by 90°, or not. 
# format: vector of formats of the generated files (PDF and/or PNG, NA for the screen).
# returns: the counts processed in this function from the original values.
#############################################################################################
plot.stacked.grouped.raw.bars <- function(plot.file, group.names, bar.names, color.names, values, proportions=TRUE, y.lim=c(NA,NA), x.label, plot.title, x.rotate=FALSE, format=c("PDF","PNG",NA))
{	counts <- list()
	dispersion <- NA
	
	# if values is a list of lists of lists, we average the counts
	if(is.list(values[[1]][[1]]))
	{	dispersion <- list()
		for(l in 1:length(values))
		{	l.values <- values[[l]]
			l.counts <- list()
			l.dispersion <- list()
			for(s in 1:length(l.values))
			{	mcounts <- t(sapply(l.values[[s]], function(v) as.vector(table(factor(v, levels=color.names), useNA="no"))))
				#print(mcounts)
				l.counts[[s]] <- apply(mcounts, 2, mean)
				l.dispersion[[s]] <- apply(mcounts, 2, sd)
			}
			counts[[l]] <- l.counts
			dispersion[[l]] <- l.dispersion
		}
	}
	# otherwise, we directly count the values
	else
	{	for(l in 1:length(values))
		{	l.values <- values[[l]]
			l.counts <- lapply(l.values, function(v) as.vector(table(factor(v, levels=color.names), useNA="no")))
			counts[[l]] <- l.counts
		}
		dispersion <- NA
	}
	
	# set up label for y axis
	if(proportions)
		y.label <- "Percent"
	else
		y.label <- "Count"
	
	# use the other function to perfom the actual plot
	#print(counts)
	plot.stacked.grouped.count.bars(plot.file, group.names, bar.names, color.names, counts, dispersion, proportions, y.lim, x.label, y.label, colors.label, plot.title, x.rotate, format)
	return(counts)
}



#############################################################################################
# Plots the specified multiple series as a stacked groupped bar plot. Each group corresponds to one 
# categorical or integer value specified in the parameter bar.names, and its height depends on the specified
# values. Each bar in a group corresponds to an other categorical or integer value specified in the 
# bar.names parameter. Each color in a bar corresponds to yet another categorical or integer value
# specified in the color.names parameter. The value vectors are supposed to contain the symbols from 
# color.names.
#
# plot.file: path and name of the generated files.
# group.names: vector of symbols or integer values used to name the groups of bars.
# bar.names: vector of symbols or integer values used to name the individual bars.
# color.names: vector of symbols or integer values used to name the stacked colors forming the bars.
# counts: list of lists of vectors of counts used to to determine the bar heights.
# dispersion: optional list of vectors of dispersion values (one for each count value).
# proportions: whether to plot actual counts or proportions.
# y.lim: limits for the y axis (optional).
# x.label: general title of the bars. 
# y.label: title of the bar heights. 
# colors.label: title of the color legend 
# plot.title: general title of the plot (NA for no title at all).
# x.rotate: whether the x axis labels should be rotated by 90°, or not. 
# format: vector of formats of the generated files (PDF and/or PNG, NA for the screen).
#############################################################################################
plot.stacked.grouped.count.bars <- function(plot.file, group.names, bar.names, color.names, counts, dispersion, proportions=TRUE, y.lim=c(NA,NA), x.label, y.label="Count", colors.label, plot.title, x.rotate=FALSE, format=c("PDF","PNG",NA))
{	# possibly complete the y axis ranges
	if(is.na(y.lim[1]) & !is.na(y.lim[2]))
	{	if(proportions)
			y.lim[1] <- 0 #TODO not tested
		else
			y.lim[1] <- min(sapply(counts, function(l) min(sapply(l, sum)))) #TODO not tested  
	}
	
	
	else if(!is.na(y.lim[1]) & is.na(y.lim[2]))
	{	if(proportions)
			y.lim[2] <- 1 #TODO not tested
		else
			y.lim[2] <- max(sapply(counts, function(l) max(sapply(l, sum)))) #TODO not tested
	}
	
	# create the data frame
	col.groups <- c()
	col.bars <- c()
	col.colors <- c()
	col.counts <- c()
	col.dispersion <- c()
	col.cumul <- c()
	for(l in 1:length(counts))
	{	l.counts <- counts[[l]]
		if(length(dispersion)>0 && !is.na(dispersion))
			l.dispersion <- dispersion[[l]]
		group.name <- group.names[l]
		for(s in 1:length(l.counts))
		{	col.groups <- c(col.groups,rep(group.name,length(l.counts[[s]])))
			bar.name <- bar.names[s]
			col.bars <- c(col.bars,rep(bar.name,length(l.counts[[s]])))
			col.colors <- c(col.colors, color.names)
			if(proportions)
			{	tc <- l.counts[[s]] / sum(l.counts[[s]])
				tc[is.infinite(tc) | is.nan(tc)] <- 0
				if(length(dispersion)>0 && !is.na(dispersion))
				{	td <- l.dispersion[[s]] / sum(l.counts[[s]])
					td[is.infinite(td) | is.nan(td)] <- 0
				}
			}
			else
			{	tc <- l.counts[[s]]
				if(length(dispersion)>0 && !is.na(dispersion))
					td <- l.dispersion[[s]]
			}
			col.counts <- c(col.counts, tc)
			cs <- cumsum(tc)
			col.cumul <- c(col.cumul, cs)
			if(length(dispersion)>0 && !is.na(dispersion))
				col.dispersion <- c(col.dispersion, cs + td)
		}
	}
	#print(col.names);print(col.counts);print(col.colors)
	col.label <- col.counts
	if(proportions)
		col.label <- round(col.label*100)
	col.label[col.label==0] <- NA
	temp <- data.frame(grps=col.groups, bars=col.bars, counts=col.counts, clrs=col.colors, cumul=col.cumul, lbl=col.label)
	if(length(dispersion)>0 && !is.na(dispersion))
		temp[["upper"]] <- col.dispersion
	#print(counts)
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
			{	pdf(file=plot.filename,bg="white")
			}
		}
		
		# init the plot
		p <- ggplot(data=temp, aes(x=factor(bars),y=counts,fill=factor(clrs)))
		p <- p + scale_x_discrete(limits=bar.names) #TODO not tested
		# change the theme
		p <- p + theme_bw() + theme(
			panel.grid.major=element_line(colour = "grey90"),
			panel.grid.minor=element_line(colour = "grey96"),
			panel.background=element_blank(),
			axis.ticks=element_blank()
			#legend.position="none"
		)
		# possibly rotate the x axis labels
		if(x.rotate)
			p <- p + theme(axis.text.x=element_text(angle=90,hjust=1))
		# add the bars
		p <- p + geom_bar(stat="identity", color="black", position = 'stack')
		p <- p + facet_grid(~ grps)
		p <- p + geom_text(aes(y=cumul, label=lbl), vjust=1.6, color="white", size=3.5)
		# set up the y limits
		if(proportions)
		{	if(any(is.na(y.lim)))
				p <- p + scale_y_continuous(labels=percent)
			else
				p <- p + scale_y_continuous(labels=percent, limits=y.lim)
		}
		else
		{	if(!any(is.na(y.lim)))
				p <- p + ylim(y.lim)
		}
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
## Tests
#############################################################################################
#plot.file <- "temp"
#format <- c("PDF","PNG", NA)
#plot.title <- "Graph Title"
#proportions <- FALSE
#areas <- TRUE
#y.lim <- c(NA,NA)
##y.lim <- c(-10,40)
#x.rotate <- FALSE
#############################################################################################
### test uniform individual plot from count
##bar.names <- c("A", "B", "C", "D")
#bar.names <- 1:4
#counts <- c(10, 20, 0, 4)
##dispersion <- NA
#dispersion <- c(1,0,0.5,4)
#x.label <- "Categories"
#y.label <- "Values"
#plot.unif.indiv.count.bars(plot.file, bar.names, counts, dispersion, proportions, areas, y.lim, x.label, y.label, plot.title, x.rotate, format)
### test uniform individual plot from raw data
##values <- c("A", "D", "D", "C", "C", "D", "D", "D", "B", "B", NA, "D", "D", "D", "D")
##values <- c("1", "4", "4", "3", "3", "4", "4", "4", "2", "2", NA, "4", "4", "4", "4")
##plot.unif.indiv.raw.bars(plot.file, bar.names, values, proportions, areas, y.lim, x.label, plot.title, x.rotate, format)
### test uniform individual plot from multiple raw data
##values <- list()
##values[[1]] <- c("A", "D", "D", "C", "C", "D", "D", "D", "B", "B", NA, "D", "D", "D", "D")
##values[[2]] <- c("A", "A", "A", "C", "C", "A", "A", "D", "B", "B", "B", "A", "A", "D", "D")
##values[[1]] <- c("1", "4", "4", "3", "3", "4", "4", "4", "2", "2", NA, "4", "4", "4", "4")
##values[[2]] <- c("1", "1", "1", "3", "3", "1", "1", "4", "2", "2", "2", "1", "1", "4", "4")
##plot.unif.indiv.raw.bars(plot.file, bar.names, values, proportions, areas, y.lim, x.label, plot.title, x.rotate, format)
#############################################################################################
### test stacked individual plot from count
#bar.names <- c("A", "B", "C", "D")
#bar.names <- 1:4
#color.names <- c("x", "y", "z")
#counts <- list(
#	c(10, 0, 4),
#	c(1, 10, 3),
#	c(1, 0, 4),
#	c(6, 2, 4)
#)
#dispersion <- NA
##dispersion <- list(
##	c(0.8,0,2),
##	c(0.5,0.5,3),
##	c(0.2,0,0.75),
##	c(0.5,1,2)
##)
#x.label <- "Categories"
#y.label <- "Values"
#colors.label <- "Colors"
##plot.stacked.indiv.count.bars(plot.file, bar.names, color.names, counts, dispersion, proportions, areas, y.lim, x.label, y.label, colors.label, plot.title, x.rotate, format)
### test stacked individual plot from raw data
#values <- list()
#values[[1]] <- c("x", "x", "x", "x", "y", "y", "y", "z", "z", "z", NA, "x")
#values[[2]] <- c("y", "x", "z", "x", "z", "z", "z", "y", "x", "z", NA, "y", "x", "y", "x")
#values[[3]] <- c("y", "x", "x", "x", "y", "z", "y")
#values[[4]] <- c("x", "z", "z", "y", "z", "y", "z", "y", "z", "z", NA, "x", "x", "y", "y")
#plot.stacked.indiv.raw.bars(plot.file, bar.names, color.names, values, proportions, areas, y.lim, x.label, colors.label, plot.title, x.rotate, format)
###c("x","y","z")[round(runif(10,min=1,max=3))]
### test stacked individual plot from multiple raw data
##values <- list(); values[[1]] <- list(); values[[2]] <- list(); values[[3]] <- list(); values[[4]] <- list()
##values[[1]][[1]] <- c("x", "x", "x", "x", "y", "y", "y", "z", "z", "z", NA, "x")
##values[[1]][[2]] <- c("x", "y", "x", "x", "y", "y", "y", "z", "z", "z", "x", "x","x","x")
##values[[2]][[1]] <- c("y", "x", "z", "x", "z", "z", "z", "y", "x", "z", NA, "y", "x", "y", "x")
##values[[2]][[2]] <- c("y", "x", "z", "x", "z", "z", "z", "y", "x", "z", "z", "z", "z", "z", "x", "y", "y", "y")
##values[[3]][[1]] <- c("y", "x", "x", "x", "y", "z", "y", "y", "y", "y", "y")
##values[[3]][[2]] <- c("y", "x", "x", "x", "y", "z", "y")
##values[[4]][[1]] <- c("x", "z", "z", "y", "z", "y", "z", "y", "z", "z", NA, "x", "x", "y", "y")
##values[[4]][[2]] <- c("x", "z", "z", "y", "z", "y", "z", "y", "z", "z", NA, "x", "x", "y", "y", "x", "x", "x")
##plot.stacked.indiv.raw.bars(plot.file, bar.names, color.names, values, proportions, areas, y.lim, x.label, colors.label, plot.title, x.rotate, format)
#############################################################################################
### test uniform grouped plot from count
#group.names <- c("A", "B", "C", "D")
#bar.names <- c("x", "y", "z")
#counts <- list(
#	c(10, 0, 4),
#	c(1, 10, 3),
#	c(1, 0, 4),
#	c(6, 2, 4)
#)
##dispersion <- NA
#dispersion <- list(
#	c(0.8,0,2),
#	c(0.5,0.5,3),
#	c(0.2,0,0.75),
#	c(0.5,1,2)
#)
#x.label <- "Categories"
#y.label <- "Values"
##plot.unif.grouped.count.bars(plot.file, group.names, bar.names, counts, dispersion, proportions, y.lim, x.label, y.label, plot.title, x.rotate, format)
##### test uniform grouped plot from raw data
#values <- list()
#values[[1]] <- c("x", "x", "x", "x", "y", "y", "y", "z", "z", "z", NA, "x")
#values[[2]] <- c("y", "x", "z", "x", "z", "z", "z", "y", "x", "z", NA, "y", "x", "y", "x")
#values[[3]] <- c("y", "x", "x", "x", "y", "z", "y")
#values[[4]] <- c("x", "z", "z", "y", "z", "y", "z", "y", "z", "z", NA, "x", "x", "y", "y")
##plot.unif.grouped.raw.bars(plot.file, group.names, bar.names, values, proportions, y.lim, x.label, plot.title, x.rotate, format)
### test uniform grouped plot from multiple raw data
#values <- list(); values[[1]] <- list(); values[[2]] <- list(); values[[3]] <- list(); values[[4]] <- list()
#values[[1]][[1]] <- c("x", "x", "x", "x", "y", "y", "y", "z", "z", "z", NA, "x")
#values[[1]][[2]] <- c("x", "y", "x", "x", "y", "y", "y", "z", "z", "z", "x", "x","x","x")
#values[[2]][[1]] <- c("y", "x", "z", "x", "z", "z", "z", "y", "x", "z", NA, "y", "x", "y", "x")
#values[[2]][[2]] <- c("y", "x", "z", "x", "z", "z", "z", "y", "x", "z", "z", "z", "z", "z", "x", "y", "y", "y")
#values[[3]][[1]] <- c("y", "x", "x", "x", "y", "z", "y", "y", "y", "y", "y")
#values[[3]][[2]] <- c("y", "x", "x", "x", "y", "z", "y")
#values[[4]][[1]] <- c("x", "z", "z", "y", "z", "y", "z", "y", "z", "z", NA, "x", "x", "y", "y")
#values[[4]][[2]] <- c("x", "z", "z", "y", "z", "y", "z", "y", "z", "z", NA, "x", "x", "y", "y", "x", "x", "x")
#plot.unif.grouped.raw.bars(plot.file, group.names, bar.names, values, proportions, y.lim, x.label, plot.title, x.rotate, format)
#############################################################################################
### test stacked grouped plot from count
#group.names <- c("A", "B", "C", "D")
#bar.names <- c("B1", "B2")
#color.names <- c("x", "y", "z")
#counts <- list()
#counts[[1]] <- list(c(10, 0, 4), c(1, 10, 3))
#counts[[2]] <- list(c(1, 0, 4), c(6, 2, 4))
#counts[[3]] <- list(c(6, 1, 3), c(7, 8, 9))
#counts[[4]] <- list(c(11, 1, 9), c(5, 4, 3))
##dispersion <- NA
#dispersion <- list()
#dispersion[[1]] <- list(c(1, 1, 1), c(1, 1, 1))
#dispersion[[2]] <- list(c(2, 2, 2), c(2, 2, 2))
#dispersion[[3]] <- list(c(0.5, 0.5, 0.5), c(0.5, 0.5, 0.5))
#dispersion[[4]] <- list(c(3, 3, 3), c(3, 3, 3))
#x.label <- "Categories"
#y.label <- "Values"
#colors.label <- "Colors"
##plot.stacked.grouped.count.bars(plot.file, group.names, bar.names, color.names, counts, dispersion, proportions, y.lim, x.label, y.label, colors.label, plot.title, x.rotate, format)
###### test stacked grouped plot from raw data
##values <- list()
##values[[1]] <- list(c("x","z","z","y","y","x","y","y","y","z","z","x","x","x","x","x"), c("z","z","x","x","x","x","z","y","z","y","z","z"))
##values[[2]] <- list(c("z","z","y","y","y","y","z","y","z"), c("z","z","y","y","y","y","y","y","x","x","x"))
##values[[3]] <- list(c("z","z","x","z","y","y","x","x","z","z","y","y","z"), c("z","z","x","y","y","y","z","y","z","y","z","x","x","x"))
##values[[4]] <- list(c("z","y","y","y","z","z","x"), c("z","z","z","z","z","z","y","z","x","z","z","z","y","z","y","z"))
##plot.stacked.grouped.raw.bars(plot.file, group.names, bar.names, color.names, values, proportions, y.lim, x.label, plot.title, x.rotate, format)
##### test stacked grouped plot from multiple raw data
#values <- list(); values[[1]] <- list(); values[[2]] <- list(); values[[3]] <- list(); values[[4]] <- list()
#values[[1]][[1]] <- list(c("x", "x", "x", "x", "y"), c("y", "y", "z", "z", "z", NA, "x"))
#values[[1]][[2]] <- list(c("x", "y", "x", "x", "y", "y", "y", "z", "z", "z"), c("x", "x","x","x"))
#values[[2]][[1]] <- list(c("y", "x", "z", "x", "z", "z", "z"), c("y", "x", "z", NA, "y", "x", "y", "x"))
#values[[2]][[2]] <- list(c("y", "x", "z", "x", "z"), c("z", "z", "y", "x", "z", "z", "z", "z", "z", "x", "y", "y", "y"))
#values[[3]][[1]] <- list(c("y", "x", "x", "x"), c("y", "z", "y", "y", "y", "y", "y"))
#values[[3]][[2]] <- list(c("y", "x", "x"), c("x", "y", "z", "y"))
#values[[4]][[1]] <- list(c("x", "z", "z", "y", "z", "y"), c("z", "y", "z", "z", "z", "x", "x", "y", "y"))
#values[[4]][[2]] <- list(c("x", "z", "z", "y"), c("z", "y", "z", "y", "z", "z", NA, "x", "x", "y", "y", "x", "x", "x"))
#plot.stacked.grouped.raw.bars(plot.file, group.names, bar.names, color.names, values, proportions, y.lim, x.label, plot.title, x.rotate, format)
