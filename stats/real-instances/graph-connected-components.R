#!/usr/bin/Rscript

# ====================================================
# This script aims to create two csv files
#   about connected components. No plot !
# ====================================================

source("stats/define-plots.R")

csv.path = paste(DIR.REAL.INSTANCES.CSV,TARGET.TYPE,sep="/") # TODO bunun normlade FOR loop'dan gelmesi lazim : country and group
output.path = paste(DIR.REAL.INSTANCES.PLOTS,TARGET.TYPE,"graph-connected-components",sep="/")

filename.orig.graph.str = paste("original-",G.STR,"-info-after-data-cleansing.csv",sep="")
filepath.orig.graph.str = paste(csv.path,filename.orig.graph.str,sep="/")
filename.filt.graph.str = paste("filtered-",G.STR,"-info-after-data-cleansing.csv",sep="")
filepath.filt.graph.str = paste(csv.path,filename.filt.graph.str,sep="/")

df.f = read.csv(filepath.filt.graph.str)
df.o = read.csv(filepath.orig.graph.str)


# =======================


nb.obs = nrow(df.f) # nb.obs = nrow(df.o)
df.after.filtering = c()

for(i in seq(1, nb.obs)){
	target = df.o[i, "target"] # df.f[i, "target"]
	domain = df.o[i, "domain"] # df.f[i, "domain"] 
	period = df.o[i, "period"] # df.f[i, "period"]
	
	o.nb.comp = as.integer( unlist( strsplit(as.character(df.o[i, "nb.compontent"]), " ") )) 
	o.comp.size = as.integer( unlist( strsplit(as.character(df.o[i, "component.size"]), " ") ))
	o.comp.mems = as.integer( unlist( strsplit(as.character(df.o[i, "component.membership"]), " ") ))
	
	# =======================================================================
	# HERE WE VERIFIED that instances have only 1 largest componenet (i.e. there are not 2 instances equaly sized)
	# 
	#largest.clu.val = max(o.comp.size)
	#largets.clu.id = which(o.comp.size == largest.clu.val)
	#if(length(largets.clu.id) > 1) print("multiple equal size largest comp")
	# =======================================================================
	
	largest.comp.id = which.max(o.comp.size)
	largest.comp.indx = which(o.comp.mems == largest.comp.id)
	
	f.comp.mems = as.integer( unlist( strsplit(as.character(df.f[i,"component.membership"]), " ") ))
	
	nb.comp = length(unique(f.comp.mems[largest.comp.indx]))
	comp.size = as.vector(table(f.comp.mems[largest.comp.indx]))
	max.indx = which.max(comp.size)
	perc.largest.comp = comp.size[max.indx] / sum(comp.size)
	
	s.comp.size = paste(comp.size, collapse=" ")
	tmp.df = data.frame(target, domain, period, nb.comp, s.comp.size, perc.largest.comp)
	
	df.after.filtering = rbind(df.after.filtering, tmp.df)
}

write.csv(file=paste(output.path,"component-info-after-filtering.csv",sep="/"), x=df.after.filtering)



# ==============================================================================
# generate some statistics such as mean, standard deviation
# ==============================================================================
df.table.info = table(df.after.filtering[,"nb.comp"])
comp.sizes = as.integer(names(df.table.info))

# show means and st deviation in each "nb.comp"
res = sapply(comp.sizes, function(x){
			subset.df = df.after.filtering[which(df.after.filtering[, "nb.comp"] == x), "perc.largest.comp"]
			return( c(mean(subset.df), sd(subset.df)) )
		})


# show how many instances are there in each "nb.comp"
res = rbind(table(df.after.filtering[, "nb.comp"]), res)
rownames(res) = c("#instances", "mean", "sd")
colnames(res) = names(df.table.info)

write.csv(file=paste(output.path,"components-mean-std-after-filtering.csv",sep="/"), x=res)