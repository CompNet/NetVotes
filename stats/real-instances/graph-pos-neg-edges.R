#!/usr/bin/Rscript
source("stats/define-plots.R")

csv.path = paste(DIR.REAL.INSTANCES.CSV,TARGET.TYPE,sep="/") # TODO bunun normlade FOR loop'dan gelmesi lazim : country and group
output.path = paste(DIR.REAL.INSTANCES.PLOTS,TARGET.TYPE,"graph-pos-neg-edges",sep="/")

filename.orig.graph.str = paste("original-",G.STR,"-info-after-data-cleansing.csv",sep="")
filepath.orig.graph.str = paste(csv.path,filename.orig.graph.str,sep="/")
filename.filt.graph.str = paste("filtered-",G.STR,"-info-after-data-cleansing.csv",sep="")
filepath.filt.graph.str = paste(csv.path,filename.filt.graph.str,sep="/")

df.f = read.csv(filepath.filt.graph.str)
df.o = read.csv(filepath.orig.graph.str)
nb.obs = nrow(df.f) # nb.obs = nrow(df.o)

# ==============================================================================
# Init for part2: for comparison between filtered and original version
# Use this double for loop in order to get the values of those 2 variables below
filt.neg.perc.links = NA
orig.neg.perc.links = NA
filt.neg.perc.weights = NA
orig.neg.perc.weights = NA

orig.total.weights = NA
orig.total.links = NA
filt.total.weights = NA
filt.total.links = NA
# ==============================================================================

for(filtering.type in c("filtered", "original")){
	for(edge.type in c("links", "weights")){
		
		col.prefix = "edges.total."
		col = paste(col.prefix,edge.type,sep="")
		neg.col = paste("neg.",col,sep="")
		pos.col = paste("pos.",col,sep="")
		
		# =====================================================================
		neg.edg = NA
		pos.edg = NA
		
		if(filtering.type == "filtered"){
			neg.edg = df.f[, neg.col] # "neg.edges.total.link" or "neg.edges.total.weight"
			pos.edg = df.f[, pos.col]
			
			if(edge.type == "links"){
				filt.total.links = neg.edg + pos.edg
				filt.neg.perc.links = neg.edg / filt.total.links
			} else if(edge.type == "weights"){
				filt.total.weights = neg.edg + pos.edg
				filt.neg.perc.weights = neg.edg / filt.total.weights
			}
		} else{ # if(filtering.type == "original")
			neg.edg = df.o[, neg.col] # "neg.edges.total.link" or "neg.edges.total.weight"
			pos.edg = df.o[, pos.col]
			
			if(edge.type == "links"){
				orig.total.links = neg.edg + pos.edg
				orig.neg.perc.links = neg.edg / orig.total.links
			} else if(edge.type == "weights"){
				orig.total.weights = neg.edg + pos.edg
				orig.neg.perc.weights = neg.edg / orig.total.weights
			}
		}
		# =====================================================================
		
		total = neg.edg + pos.edg
		order.by = order(-total) # negate in order to rank in desc order
		
		color1 = "dark red"
		color2 = "dark green"
		x = seq(1, nb.obs)
		#x = total[order.by]
		y1 = neg.edg[order.by]
		y2 = pos.edg[order.by]
		
		# ======== Tick Positions and labels --------- SONRADAN EKLEDIM
		desc.ord.total = total[order.by]
		step=500
		x.tick.pos = seq(1, length(total), step) # step by step
		x.tick.labels = desc.ord.total[x.tick.pos]
		if(edge.type == "weights") # float'li sayilarin virgulden sonra baya rakami oldugu icin label'larin cogunu gizliyodu
			x.tick.labels = round(x.tick.labels,digits=2)
		# ========
		
		# ======================================================================
		# positive and negative edges - without log()
		# ======================================================================
		output.filename = paste(output.path,"/",filtering.type,"-",edge.type,"-without-log", sep="")
		# point types: http://www.sthda.com/french/wiki/les-differents-types-de-points-dans-r-comment-utiliser-pch
		my.plot(plot.filename=output.filename,
				x1=x, x2=x, y1=y1, y2=y2,
				XTickPositions=x.tick.pos, XTickLabels=x.tick.labels,
				xlabel=paste("Total number of ",edge.type, ", by decreasing order ",sep=""),
#				xlabel=paste("Graph instances, by decreasing order of total number of ",edge.type,sep=""),
				ylabel=paste("Number of ",edge.type,sep=""), # "Number of weights" or "Number of links"
				color1=color1, color2=color2, desc1="Negative links", desc2="Positive links",
				plot.type1="p", plot.type2="p", point.type1=8, point.type2=6,
				legend.title=ifelse(filtering.type == "filtered", "Filtered Graphs", "Unfiltered Graphs"),
				is.y.axis.log.scaled=FALSE) # ========================> ONLY CHANGED HERE
		
		# ======================================================================
		# positive and negative edges - with log: use "log" option in plot
		# ======================================================================
		output.filename = paste(output.path,"/",filtering.type,"-",edge.type,"-with-log", sep="")
		# point types: http://www.sthda.com/french/wiki/les-differents-types-de-points-dans-r-comment-utiliser-pch
		my.plot(plot.filename=output.filename,
				x1=x, x2=x, y1=y1, y2=y2,
				XTickPositions=x.tick.pos, XTickLabels=x.tick.labels,
				xlabel=paste("Total number of ",edge.type, ", by decreasing order ",sep=""),
#				xlabel=paste("Graph instances, by decreasing order of total number of ",edge.type,sep=""),
				ylabel=paste("Number of ",edge.type,sep=""), # "Number of weights" or "Number of links"
				color1=color1, color2=color2, desc1="Negative links", desc2="Positive links",
				plot.type1="p", plot.type2="p", point.type1=8, point.type2=6,
				legend.title=ifelse(filtering.type == "filtered", "Filtered Graphs", "Unfiltered Graphs"),
				is.y.axis.log.scaled=TRUE) # ========================> ONLY CHANGED HERE
	}
}


################################################################################
################################################################################
################################################################################

# ======================================================================
# negative percentage - comparison between filtered and original version
# ======================================================================
color1 = "orange"
color2 = "blue"
order.by = order(-orig.total.links) # negate in order to rank in desc order
x = seq(1, nb.obs)	
y1 = orig.neg.perc.links[order.by]
y2 = filt.neg.perc.links[order.by]

output.filename = paste(output.path,"negative-link-percentage-comparison", sep="/")
# point types: http://www.sthda.com/french/wiki/les-differents-types-de-points-dans-r-comment-utiliser-pch
my.plot(plot.filename=output.filename,
		x1=x, x2=x, y1=y1, y2=y2,
		xlabel="Graph instances, by decreasing order of 'original'='unfiltered' total number of links",
		ylabel="Percentage of negative links",
		color1=color1, color2=color2, desc1="Original(Unfiltered) Graphs", desc2="Filtered Graphs",
		plot.type1="p", plot.type2="p", point.type1=8, point.type2=6,
		is.y.axis.log.scaled=FALSE)




################################################################################
################################################################################
################################################################################


# ======================================================================
# proportion of removed links
# ======================================================================

order.by = order(-orig.total.links) # negate in order to rank in desc order	
y = (orig.total.links - filt.total.links)/orig.total.links
y1 = y[order.by]
y2 = y[order(y)]


plot.filename = "proportion-removed-links-comparison.pdf"
output.filename = paste(output.path, plot.filename, sep="/")

pdf(file=output.filename, bg="white",compress=FALSE)
plot(y1, ylab="proportion of removed links", xlab="Graph instances, by decreasing order of total number of links")
dev.off()

#print(TARGET.TYPE)
#print("median for prop. of removed links")
#print(median(y2))
#print("mean for prop. of removed links")
#print(mean(y2))


################################################################################
################################################################################
################################################################################


# ======================================================================
# proportion of removed weighted links (i.e. weights)
# ======================================================================

order.by = order(-orig.total.weights) # negate in order to rank in desc order	
y = (orig.total.weights - filt.total.weights)/orig.total.weights
y1 = y[order.by]
y2 = y[order(y)]


plot.filename = "proportion-removed-weights-comparison.pdf"
output.filename = paste(output.path, plot.filename, sep="/")

pdf(file=output.filename, bg="white",compress=FALSE)
plot(y1, ylab="proportion of removed weights", xlab="Graph instances, by decreasing order of total number of weights")
dev.off()

#print(TARGET.TYPE)
#print("median for prop. of removed weights")
#print(median(y2))
#print("mean for prop. of removed weights")
#print(mean(y2))
