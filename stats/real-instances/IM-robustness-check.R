#!/usr/bin/Rscript

source("stats/define-plots.R")

csv.path = paste(DIR.REAL.INSTANCES.CSV,TARGET.TYPE,sep="/") # TODO bunun normlade FOR loop'dan gelmesi lazim : country and group
output.path = paste(DIR.REAL.INSTANCES.PLOTS,"/",TARGET.TYPE,"/",COMDET.INFOMAP,"-robustness-check",sep="")

filename.orig.IM = paste("original-",COMDET.INFOMAP,"-info-after-data-cleansing.csv",sep="")
filepath.orig.IM = paste(csv.path,filename.orig.IM,sep="/")
filename.filt.IM = paste("filtered-",COMDET.INFOMAP,"-info-after-data-cleansing.csv",sep="")
filepath.filt.IM = paste(csv.path,filename.filt.IM,sep="/")
filename.orig.ExCC = paste("original-",ExCC,"-info-after-data-cleansing.csv",sep="")
filepath.orig.ExCC = paste(csv.path,filename.orig.ExCC,sep="/")
filename.filt.ExCC = paste("filtered-",ExCC,"-info-after-data-cleansing.csv",sep="")
filepath.filt.ExCC = paste(csv.path,filename.filt.ExCC,sep="/")

filename.orig.graph.str = paste("original-",G.STR,"-info-after-data-cleansing.csv",sep="")
filepath.orig.graph.str = paste(csv.path,filename.orig.graph.str,sep="/")
filename.filt.graph.str = paste("filtered-",G.STR,"-info-after-data-cleansing.csv",sep="")
filepath.filt.graph.str = paste(csv.path,filename.filt.graph.str,sep="/")

df.filt.g.str = read.csv(filepath.filt.graph.str)
df.orig.g.str = read.csv(filepath.orig.graph.str)

# ====== Manually configuring ========= 
THRESHOLD.GRAPH.SIZE = 50
THRESHOLD.POS.DENS = 0.95
# =====================================

for(filtering.type in c("filtered", "original")){
	for(imb.type in c("value", "percentage")){
		
		df.g.str = NA
		if(filtering.type == "filtered") df.g.str = df.filt.g.str
		else df.g.str = df.orig.g.str
		
		
		col = paste("imbalance.", imb.type, sep="")
		imb.IM =NA
		if(filtering.type == "filtered") imb.IM = retreive.col.val.from.df(filepath.filt.IM, col)
		else imb.IM = retreive.col.val.from.df(filepath.orig.IM, col)
		
		imb.ExCC =NA
		if(filtering.type == "filtered") imb.ExCC = retreive.col.val.from.df(filepath.filt.ExCC, col)
		else imb.ExCC = retreive.col.val.from.df(filepath.orig.ExCC, col)
		
		nb.obs = length(imb.IM) # nb.obs = length(imb.ExCC)
		df.g.str$neg.edges.total.weights = as.numeric(df.g.str$neg.edges.total.weights)
		df.g.str$pos.edges.total.weights = as.numeric(df.g.str$pos.edges.total.weights)
		total.abs.weights = abs(df.g.str$neg.edges.total.weights) + df.g.str$pos.edges.total.weights
		df.g.str$pos.dens = df.g.str$pos.edges.total.weights/total.abs.weights	
		
		
		for(ordering.type in c("graph-size", "pos-dens")){
			# ========================================================
			# ordering
			order.by = NA
			starting.indx.threshold = NA
			dashed.line.desc = NA
			
			if(ordering.type == "graph-size"){
				order.by = order(df.g.str$graph.size) #  in asc order of graph size
				starting.indx.threshold = which(df.g.str$graph.size[order.by] >= THRESHOLD.GRAPH.SIZE)[1] # as we ordered by asc order, take the first value which will be the min number
				dashed.line.desc = paste("the 1st instance where graph size is ", THRESHOLD.GRAPH.SIZE, sep="")
			}
			else if(ordering.type == "pos-dens"){
				order.by = order(df.g.str$pos.dens) #  in asc order of graph size
				starting.indx.threshold = which(df.g.str$pos.dens[order.by] >= THRESHOLD.POS.DENS)[1] # as we ordered by asc order, take the first value which will be the min number
				dashed.line.desc = paste("the 1st instance where percentage of positive weighted links is ", THRESHOLD.POS.DENS, sep="")
			}
			
			y1 = imb.IM[order.by]
			y2 = imb.ExCC[order.by] 
			x = seq(1, nb.obs)
			
			color1 = "blue"
			color2 = "orange"
			y.label.suffix = ifelse(imb.type == "value", "", "%")
			# en sona bilerek ".pdf" koymadim, zaten my.plot ekliyo
			output.filename = paste(output.path,"/",filtering.type, "-imb-", imb.type, "-ordered-by-", ordering.type, sep="")
			
			#print(x)
			#print(y1)
			#print(y2)
			# point types: http://www.sthda.com/french/wiki/les-differents-types-de-points-dans-r-comment-utiliser-pch
			my.plot(plot.filename=output.filename,
					x1=x, x2=x, y1=y1, y2=y2,
					xlabel=paste("instances (ordered by ", ordering.type, " in asc order)", sep=""),
					ylabel=paste("I(P)", y.label.suffix, sep=""),
					color1=color1, color2=color2, desc1="Negative links", desc2="Positive links", dashed.line.desc=dashed.line.desc,
					plot.type1="p", plot.type2="p", point.type1=8, point.type2=6,
					extra.ver.line.val=starting.indx.threshold,
					is.y.axis.log.scaled=FALSE)
			# ========================================================
		}
	}
}