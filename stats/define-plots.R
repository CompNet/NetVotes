# TODO: Add comment
# 
# Author: nejat
###############################################################################

# =========================================================
#
# =========================================================
retreive.col.val.from.df = function(input.filename, col.name){
	df = read.csv(input.filename)
	return(df[,col.name])
}


##################################################################################################
##################################################################################################
##################################################################################################


#
# point.type is used when plot.type="p"
# plot.filename: without file extention
# user can draw only 1 horizantal line and 1 vertical line => abline
# legend is added when either "dashed.line.desc " or ("desc1" and "desc2") are provided
# =========================================================
#
# =========================================================
my.plot = function(plot.filename, x1, x2=NA, y1, y2=NA,
		XTickPositions=NA, XTickLabels=NA,
		plot.type1="p", plot.type2="p", point.type1=1, point.type2=1, 
		xlabel="x", ylabel="y", color1="black", color2=NA, desc1=NA, desc2=NA, dashed.line.desc=NA,
		extra.ver.line.val=NA, extra.hor.line.val=NA, legend.title="",
		is.y.axis.log.scaled=FALSE)
{
	has.dahsed.line = FALSE
	has.legend = FALSE
	
	# XTickPositions'a bakmadim, direkt label'a baktim. Label yoksa bi anlami yok cunku
	has.X.tick.labels = ifelse(!is.na(XTickLabels), TRUE, FALSE)

	
	pdf(file=paste(plot.filename,".pdf",sep=""), bg="white",compress=FALSE)
	
	if(is.y.axis.log.scaled &&  !has.X.tick.labels){
		# it is important to set min value of "ylim" to 1, because the plot function behaves in a very strange way
		plot(x1, y1, log="y", xlim=range(c(x1,x2),na.rm=T), ylim=c(1, max(y1,y2,na.rm=T)),
				xlab = xlabel, ylab = ylabel, type=plot.type1, pch=point.type1, col=color1) # cex=1.5
	} else if(is.y.axis.log.scaled &&  has.X.tick.labels){
					# it is important to set min value of "ylim" to 1, because the plot function behaves in a very strange way
					plot(x1, y1, log="y", xlim=range(c(x1,x2),na.rm=T), ylim=c(1, max(y1,y2,na.rm=T)),
							xaxt="n", #for removing x-axis labels
							xlab = xlabel, ylab = ylabel, type=plot.type1, pch=point.type1, col=color1) # cex=1.5 
	} else if(!is.y.axis.log.scaled &&  has.X.tick.labels){
		plot(x1, y1, xlim=range(c(x1,x2),na.rm=T), ylim=range(c(y1,y2),na.rm=T),
				xaxt="n", #for removing x-axis labels
				xlab = xlabel, ylab = ylabel, type=plot.type1, pch=point.type1, col=color1) # cex=1.5
	} else{ # if(!is.y.axis.log.scaled &&  !has.X.tick.labels){"
		plot(x1, y1, xlim=range(c(x1,x2),na.rm=T), ylim=range(c(y1,y2),na.rm=T),
				xlab = xlabel, ylab = ylabel, type=plot.type1, pch=point.type1, col=color1) # cex=1.5
	}
	
	color2 = ifelse(is.na(color2), "black", color2)
	# no need to indicate that y axis is log scaled because we already indicated that above
	points(x2, y2, type=plot.type2, pch=point.type2, col=color2) # cex=1.5
	
	
	
	# ===================
	if(has.X.tick.labels)
		axis(side=1,at=XTickPositions, labels= XTickLabels,tcl=0.4,lwd.ticks=3,mgp=c(0,0.5,0))
	# ===================

	
	# =========================================================================
	if(!is.na(extra.ver.line.val)){ abline(v=extra.ver.line.val, lty = 2); has.dahsed.line=TRUE}
	if(!is.na(extra.hor.line.val)){ abline(h=extra.hor.line.val, lty = 2); has.dahsed.line=TRUE}

	if(!is.na(dashed.line.desc) || (!is.na(desc1) && !is.na(desc2)) ) has.legend=TRUE
	
	if(has.legend && !has.dahsed.line){
		legend('topright', title = legend.title, c(desc1, desc2), lty=c(1,1), lwd=c(1.5, 1.5),
				col=c(color1, color2)) # cex=1.5
	} else if(has.legend && has.dahsed.line){
		legend('topright', title = legend.title, c(desc1, desc2, dashed.line.desc), lty=c(1,1,2), lwd=c(1.5, 1.5, 1),
				col=c(color1, color2, "black")) # cex=1.5
	}
	# =========================================================================
	
	dev.off()
}



##################################################################################################
##################################################################################################
##################################################################################################


plot.exec.time.perf.analysis = function(output.path, input.filepath1, input.filepath2, filepath.graph.str, method1, method2){
	col1 = "exec.time"
	col2 = "graph.size"
	exec.time1 = retreive.col.val.from.df(input.filepath1, col1)
	exec.time2 = retreive.col.val.from.df(input.filepath2, col1)
	
	# graph size zaten ascending order oldugu icin ekstradan order.by yapmiyorum
	graph.size = retreive.col.val.from.df(filepath.graph.str, col2)

	color1 = "orange"
	color2 = "blue"
	
	# ==============================================================================
	# why a horizontal line at 450? Because after the 450. instance, ExCC runs really slow
	# why a vertical line at 3600? Because 3600 is equal to 1 hour
	# ==============================================================================
	
	my.plot(plot.filename=paste(output.path,"/ILSCC-ExCC-execTimePerf1", sep=""),
			x1=graph.size, x2=graph.size, y1=exec.time1, y2=exec.time2,
			xlabel=col2, ylabel=paste(col1,"(sec)"),
			plot.type1="l", plot.type2="l",
			color1=color1, color2=color2, desc1=method1, desc2=method2,
			#extra.ver.line.val=450, extra.hor.line.val=3600,
			is.y.axis.log.scaled=FALSE)
	
	# ==============================================================================
	# log scaled plot
	
	my.plot(plot.filename=paste(output.path,"/ILSCC-ExCC-execTimePerf2", sep=""),
			x1=graph.size, x2=graph.size, y1=exec.time1, y2=exec.time2,
			xlabel="graph size", ylabel="log(exec time) (sec)",
			plot.type1="l", plot.type2="l",
			color1=color1, color2=color2, desc1=method1, desc2=method2,
			#extra.ver.line.val=450, extra.hor.line.val=3600,
			is.y.axis.log.scaled=TRUE)
}



##################################################################################################
##################################################################################################
##################################################################################################

# simple methods
# =========================================================
#
# =========================================================
retreive.partitions = function(input.filepath){
	col = "sPartition"
	sPartition = retreive.col.val.from.df(input.filepath, col)
	
	partition = lapply(sPartition, function(x) as.integer( unlist( strsplit(as.character(x), " ") )) )
	return(partition)
}

# =========================================================
#
# =========================================================
remove.isolated.nodes = function(partitions, filepath.graph.str){
	col = "s.iso.nodes.indx"
	s.iso.nodes = retreive.col.val.from.df(filepath.graph.str, col)
	iso.nodes = lapply(s.iso.nodes, function(x) as.integer( unlist( strsplit(as.character(x), " ") )) )
	
	nb.obs.iso = length(iso.nodes)
	
	if(nb.obs.iso > 0){
	
		res = lapply(seq(1, nb.obs.iso), function(i){
		  		iso = iso.nodes[[i]]
		  		p = partitions[[i]]
		  		if(length(iso) > 0) p = p[-iso] # remove isolated nodes
  
  				return(p)
			})
			
		return(res)
	}
	
	return(partitions)
}


# =========================================================
#
# =========================================================
# if !is.na(filepath.graph.str) == TRUE, that means that isolated nodes will be removed from partitions
compute.nb.clusters = function(input.filepath, filepath.graph.str=NA){
	partitions = retreive.partitions(input.filepath)
	
	if(!is.na(filepath.graph.str)){
		partitions = remove.isolated.nodes(partitions, filepath.graph.str)
	}
	
	nb.obs = length(partitions)
	
	nb.clusters = 
			sapply(
					seq(1,nb.obs), function(i) return( length(unique(partitions[[i]])) )
			)   
	
	return(nb.clusters)
}


# =========================================================
#
# =========================================================
compute.delta.nb.cluster = function(input.filepath1, input.filepath2, filepath.graph.str1, filepath.graph.str2){	
	nb.clusters1 = compute.nb.clusters(input.filepath1, filepath.graph.str1)
	nb.clusters2 = compute.nb.clusters(input.filepath2, filepath.graph.str2)
	delta.nb.clusters = nb.clusters1 - nb.clusters2
	
	return(delta.nb.clusters)
}


# =========================================================
#
# =========================================================
# if !is.na(filepath.graph.str1) == TRUE && !is.na(filepath.graph.str2) == TRUE, that means that isolated nodes will be removed from partitions
compute.nmi.scores = function(input.filepath1, input.filepath2, filepath.graph.str1=NA, filepath.graph.str2=NA){
	partitions1 = retreive.partitions(input.filepath1)
	partitions2 = retreive.partitions(input.filepath2)
	
	if(!is.na(filepath.graph.str1) && !is.na(filepath.graph.str2)){
		partitions1 = remove.isolated.nodes(partitions1, filepath.graph.str1)
		partitions2 = remove.isolated.nodes(partitions2, filepath.graph.str2)
	}
	

	
	nb.obs = length(partitions1) # nb.obs = length(partitions2)
	
	nmi.scores = 
			sapply(seq(1,nb.obs),
					function(i){
						p1 = partitions1[[i]]
						p2 = partitions2[[i]]
						return( compare(p1, p2, method = "nmi") )
					}
			) 
	
	return(nmi.scores)
}

# =========================================================
#
# =========================================================
compute.delta.imb = function(input.filepath1, input.filepath2){
	res = list()
	for(imb.type in c("value","percentage")){
		col1 = paste("imbalance.", imb.type, sep="")
		imb1 = retreive.col.val.from.df(input.filepath1, col1)
		imb2 = retreive.col.val.from.df(input.filepath2, col1)
		delta.imb = imb2 - imb1 #(filtering - original)
		res[[imb.type]]=delta.imb
	}
	return(res)
}

##################################################################################################
##################################################################################################
##################################################################################################


# summary ? TODO
# =========================================================
#
# =========================================================
plot.nmi.vs.freq.betw.2.methods = function(output.path, input.filepath1, input.filepath2,
		filepath.graph.str=NA, method1, method2)
{
	
	nmi.scores = compute.nmi.scores(input.filepath1, input.filepath2, filepath.graph.str, filepath.graph.str)

	
	# plot
	output.filename = paste(output.path, "/",method1,"-",method2,"-nmi-vs-freq.pdf",sep="")
	pdf(file=output.filename, bg="white",compress=FALSE)
	hist(nmi.scores, main=NA, xlab = "NMI scores") # ylim=c(0, nb.obs)
	dev.off()
}


# =========================================================
#
# =========================================================
plot.nmi.vs.freq.betw.orig.and.filt = function(output.path, filepath.orig, filepath.filt, 
		filepath.orig.graph.str=NA, filepath.filt.graph.str=NA, method)
{
	
	nmi.scores = compute.nmi.scores(filepath.orig, filepath.filt, filepath.orig.graph.str, filepath.filt.graph.str)
	
	# plot
	output.filename = paste(output.path, "/", "filter-step-", method, "-nmi-vs-freq.pdf", sep="")
	pdf(file=output.filename, bg="white",compress=FALSE)
	hist(nmi.scores, main=NA, xlab = "NMI scores")
	dev.off()
}

##################################################################################################
##################################################################################################
##################################################################################################


# summary ? TODO
# =========================================================
#
# =========================================================
plot.imb.vs.graph.size = function(output.path, input.filepath1, input.filepath2, filepath.graph.str=NA, method1, method2){
	for(imb.type in c("value","percentage")){
		col1 = paste("imbalance.",imb.type,sep="")
		col2 = "graph.size"
		imb1 = retreive.col.val.from.df(input.filepath1, col1)
		imb2 = retreive.col.val.from.df(input.filepath2, col1)
		
		# graph size zaten ascending order oldugu icin ekstradan order.by yapmiyorum
		graph.size = retreive.col.val.from.df(filepath.graph.str, col2)
		color1 = "purple"
		color2 = "blue"
		
		# point types: http://www.sthda.com/french/wiki/les-differents-types-de-points-dans-r-comment-utiliser-pch
		filename = paste("perfAnalysis-imb", imb.type, "-graphsize", sep="")
		plot.filename = paste(output.path, filename, sep="/")
		my.plot(plot.filename=plot.filename,
				x1=graph.size, x2=graph.size, y1=imb1, y2=imb2, xlabel=col2, ylabel=col1,
				color1=color1, color2=color2, desc1=method1, desc2=method2,
				plot.type1="p", plot.type2="p", point.type1=8, point.type2=6,
				#extra.ver.line.val=450, extra.hor.line.val=3600,
				is.y.axis.log.scaled=FALSE)
	}
}


# summary ? TODO
# =========================================================
#
# =========================================================
plot.delta.imb.vs.freq.betw.2.methods = function(output.path, input.filepath1, input.filepath2, method1, method2){
    res = compute.delta.imb(input.filepath1, input.filepath2)
	for(imb.type in c("value","percentage")){
		delta.imb = res[[imb.type]]
		output.filename = paste(output.path, "/", method1,"-",method2,"-imb-",imb.type,"-vs-freq.pdf",sep="")
		pdf(file=output.filename, bg="white",compress=FALSE)
		
		formula.postfix=ifelse(imb.type == "percentage", "%", "")
		hist(delta.imb, main=paste("nb obs is ", length(delta.imb), sep=""),
				xlab=bquote(.(method1)*(G^f)*.(formula.postfix) ~ "-" ~ .(method2)*(G^f)*.(formula.postfix)))
		dev.off()
	}
}

# =========================================================
#
# =========================================================
plot.delta.imb.vs.freq.betw.orig.and.filt = function(output.path, filepath.orig, filepath.filt, method){
    res = compute.delta.imb(filepath.orig, filepath.filt)
	for(imb.type in c("value","percentage")){
		delta.imb = res[[imb.type]]
		output.filename = paste(output.path, "/filter-step-", method, "-imb", imb.type,"-vs-freq.pdf", sep="")
		pdf(file=output.filename, bg="white",compress=FALSE)
		
		formula.postfix=ifelse(imb.type == "percentage", "%", "")
		hist(delta.imb, main=paste("nb obs is ", length(delta.imb), sep=""),
				xlab=bquote(Delta[G*","*G^f] ~ (.(method)*.(formula.postfix) )))
		
		dev.off()
	}
}



##################################################################################################
##################################################################################################
##################################################################################################

# =========================================================
#
# =========================================================
# filepath.graph.str1 and filepath.graph.str2 are used for removing isolated nodes
plot.instances.vs.nbCluster.betw.2.methods = function(output.path, input.filepath1,
		input.filepath2, filepath.graph.str=NA, method1, method2)
{
	nb.clusters1 = compute.nb.clusters(input.filepath1, filepath.graph.str)
	nb.clusters2 = compute.nb.clusters(input.filepath2, filepath.graph.str)

	
	nb.obs = length(nb.clusters1) # nb.obs = length(nb.clusters2)
	
	order.by = order(-nb.clusters2) # negate in order to rank in desc order
	y1 = nb.clusters1[order.by] # before filtering
	y2 = nb.clusters2[order.by] # after filtering
	x = seq(1, nb.obs) # no need to reorder "x" because we already reordered "y1" and "y2"
	
	color1 = "orange"
	color2 = "blue"
	
	# point types: http://www.sthda.com/french/wiki/les-differents-types-de-points-dans-r-comment-utiliser-pch
	output.filename = paste(output.path, "/filter-step-", method1,"-", method2,"-instances-vs-nbCluster.pdf", sep="")
	my.plot(plot.filename=output.filename,
			x1=x, x2=x, y1=y1, y2=y2, xlabel="instances (decr ordered by #clusters based on unfiltered graphs)", ylabel="#cluster",
			color1=color1, color2=color2, desc1=method1, desc2=method2,
			plot.type1="p", plot.type2="p", point.type1=8, point.type2=6,
			is.y.axis.log.scaled=FALSE)
}


# =========================================================
#
# =========================================================
# filepath.graph.str1 and filepath.graph.str2 are used for removing isolated nodes
plot.instances.vs.nbCluster.betw.orig.and.filt = function(output.path, filepath.orig,
		filepath.filt, filepath.orig.graph.str=NA, filepath.filt.graph.str=NA, method)
{
	nb.clusters1 = compute.nb.clusters(filepath.orig, filepath.orig.graph.str)
	nb.clusters2 = compute.nb.clusters(filepath.filt, filepath.filt.graph.str)
	
	nb.obs = length(nb.clusters1) # nb.obs = length(nb.clusters2)
	
	order.by = order(-nb.clusters1) # negate in order to rank in desc order
	y1 = nb.clusters1[order.by] # before filtering
	y2 = nb.clusters2[order.by] # after filtering
	x = seq(1, nb.obs) # no need to reorder "x" because we already reordered "y1" and "y2"
	
	color1 = "orange"
	color2 = "blue"
	# point types: http://www.sthda.com/french/wiki/les-differents-types-de-points-dans-r-comment-utiliser-pch
	output.filename = paste(output.path, "/filter-step-", method, "-instances-vs-nbCluster.pdf", sep="")
	my.plot(plot.filename=output.filename,
			x1=x, x2=x, y1=y1, y2=y2, xlabel="instances (decr ordered by #clusters based on unfiltered graphs)", ylabel="#cluster",
			color1=color1, color2=color2, desc1="original", desc2="filtered",
			plot.type1="p", plot.type2="p", point.type1=8, point.type2=6,
			is.y.axis.log.scaled=FALSE)
}



##################################################################################################
##################################################################################################
##################################################################################################






# summary ? TODO
# 
# =========================================================
#
# =========================================================
plot.nmi.vs.nbCluster.betw.2.methods = function(output.path, input.filepath1, input.filepath2,
		filepath.graph.str=NA, method1, method2)
{
	nmi.scores = compute.nmi.scores(input.filepath1, input.filepath2, filepath.graph.str,  filepath.graph.str)
	delta.nb.clusters = compute.delta.nb.cluster(input.filepath1, input.filepath2,  filepath.graph.str, filepath.graph.str)

	# =================================================================
	imb.type="value"
	res = compute.delta.imb(input.filepath1, input.filepath2)
	delta.imb = res[[imb.type]] #percentage'a gerek yok cunku sadece imb ayni mi degil mi diye bakiyoruzé
	instances.with.the.same.imb.cost = which(delta.imb == 0)
	# =================================================================
	
	# we draw  as red the points which indicates the instances with the same imb cost
	y1 = delta.nb.clusters[-instances.with.the.same.imb.cost]
	x1 = nmi.scores[-instances.with.the.same.imb.cost]
	y2 = delta.nb.clusters[instances.with.the.same.imb.cost]
	x2 = nmi.scores[instances.with.the.same.imb.cost]
	color1="blue"
	color2="red"
	desc1="different imb cost"
	desc2="the same imb cost"

	output.filename = paste(output.path,"/",method1,"-",method2,"-nmi-vs-nbCluster",sep="")
	my.plot(plot.filename=output.filename, x1=x1, x2=x2, y1=y1, y2=y2, 	# xlim=c(0,1)
			xlabel="NMI", ylabel=bquote(Delta[.(method1)*","*.(method2)] ~ "#clusters"),
			plot.type1="p", plot.type2="p", color1=color1, color2=color2, desc1=desc1, desc2=desc2)
}


# =========================================================
#
# =========================================================
# input.filepath1: original version
# input.filepath2: filtered version
plot.nmi.vs.nbCluster.betw.orig.and.filt = function(output.path, filepath.orig,
		filepath.filt, filepath.orig.graph.str=NA, filepath.filt.graph.str=NA, method){
	
	x = nmi.scores = compute.nmi.scores(filepath.orig, filepath.filt, filepath.orig.graph.str, filepath.filt.graph.str)
	y = delta.nb.clusters = compute.delta.nb.cluster(filepath.orig, filepath.filt, filepath.orig.graph.str, filepath.filt.graph.str)
	
#	# =================================================================
#	imb.type="value"
#	res = compute.delta.imb(filepath.orig, filepath.filt)
#	delta.imb = res$imb.type #percentage'a gerek yok cunku sadece imb ayni mi degil mi diye bakiyoruzé
#	instances.with.the.same.imb.cost = which(delta.imb == 0)
#	# =================================================================
	
	output.filename = paste(output.path,"/filter-step-", method, "-nmi-vs-nbCluster.pdf", sep="")
	pdf(file=output.filename, bg="white",compress=FALSE)
	plot(x, y, xlim=c(0,1), xlab = "NMI scores", ylab = bquote(Delta[G*","*G^f] ~ "#clusters"), col="blue", lty=1, type="p")
	dev.off()
}


##################################################################################################
##################################################################################################
##################################################################################################

# =========================================================
#
# =========================================================
plot.graph.density = function(output.path, input.filepath){
	col = "density"
	densities = retreive.col.val.from.df(input.filepath, col)

	output.filename = paste(output.path,"/","perfAnalysis-graphs-density.pdf",sep="")
	pdf(file=output.filename, bg="white",compress=FALSE)
	hist(densities) # ylim=c(0, 100)
	dev.off()
}
