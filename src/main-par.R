#!/usr/bin/Rscript

# TODO: Add comment
# 
# Author: nejat
###############################################################################


##############################################################################
##############################################################################


# This function combines each index of the list during foreach loop.
# For example, suppose that we have the following foearch loop:
# foreach(i=1:2, .combine='comb', .init=list(list(), list())) %dopar% {
#  	 return(list( i+1, i+10))
# }

# Thanks to this 'comb' function, we'll have this result at the end:
#[[1]]
#[[1]][[1]]
#[1] 2
#
#[[1]][[2]]
#[1] 3
#
#
#[[2]]
#[[2]][[1]]
#[1] 11
#
#[[2]][[2]]
#[1] 12
comb <- function(output, ...) {
#	print(list(...))
	
	# list(...) is list of returned value from each foreach iteration
	# so list(...) is list of list

	
	plot.inputs = list(...)
	nb.plot.inputs.returned = sapply(plot.inputs, function(x) length(x))
	# if only infomap and ilscc are calculated, so nb.plot.inputs.returned will be 2
	
	lapply(seq(1, nb.plot.inputs.returned),
			function(i){
				c(
						output[[i]], 
						lapply(plot.inputs, 
								function(y){
									y[[i]]
								})
				)
			})

}


##############################################################################
##############################################################################

plot.network.by.time = function(plot.list){
	
#	print(plot.list)
	# ==========================================================================
	# This is an example of 'plot.list' for only 1 country = Finland (could be a pol group)
	# ==========================================================================
	
#	[[1]]
#	[[1]][[1]]
#	[[1]][[1]]$output.full.dir.name
#	[1] "../../data/NetVotes/out/nejat-partitions-alpha=0.4/m3/negtr=NA_postr=NA/bycountry/Finland/AFCO/2012-13/seq_ils-cc=1-rcc=0-a=0.4-l=1-i=10-p=3-t=3600-k=0"
#	
#	[[1]][[1]]$algo.output.file
#	[1] "../../data/NetVotes/out/nejat-partitions-alpha=0.4/m3/negtr=NA_postr=NA/bycountry/Finland/AFCO/2012-13/seq_ils-cc=1-rcc=0-a=0.4-l=1-i=10-p=3-t=3600-k=0/cc-result.txt"
#	
#	[[1]][[1]]$membership
#	[1] 1 1 1 1 1 1 1 1 1 2 1 1 1
#	
#	[[1]][[1]]$plot.title
#	[1] "Finland-AFCO-2012-13  |.|  seq_ils-cc=1-rcc=0-a=0.4-l=1-i=10-p=3-t=3600-k=0\nI(P)=0.00000-%I(P)=0.00000, RI(P)=0.00000-%RI(P)=0.00000, #cluster: 1\n"
#	
#	[[1]][[1]]$desc
#	[1] "ILSCC"
#	
#	
#	[[1]][[2]]
#	[[1]][[2]]$output.full.dir.name
#	[1] "../../data/NetVotes/out/nejat-partitions-alpha=0.4/m3/negtr=NA_postr=NA/bycountry/Finland/AFCO/2013-14/seq_ils-cc=1-rcc=0-a=0.4-l=1-i=10-p=3-t=3600-k=0"
#	
#	[[1]][[2]]$algo.output.file
#	[1] "../../data/NetVotes/out/nejat-partitions-alpha=0.4/m3/negtr=NA_postr=NA/bycountry/Finland/AFCO/2013-14/seq_ils-cc=1-rcc=0-a=0.4-l=1-i=10-p=3-t=3600-k=0/cc-result.txt"
#	
#	[[1]][[2]]$membership
#	[1] 1 1 1 1 1 1 1 1 1 2 1 1 1
#	
#	[[1]][[2]]$plot.title
#	[1] "Finland-AFCO-2013-14  |.|  seq_ils-cc=1-rcc=0-a=0.4-l=1-i=10-p=3-t=3600-k=0\nI(P)=0.00000-%I(P)=0.00000, RI(P)=0.00000-%RI(P)=0.00000, #cluster: 1\n"
#	
#	[[1]][[2]]$desc
#	[1] "ILSCC"
#	
#	
#	
#	[[2]]
#	[[2]][[1]]
#	[[2]][[1]]$output.full.dir.name
#	[1] "../../data/NetVotes/out/nejat-partitions-alpha=0.4/m3/negtr=NA_postr=NA/bycountry/Finland/AFCO/2012-13/infomap"
#	
#	[[2]][[1]]$algo.output.file
#	[1] "../../data/NetVotes/out/nejat-partitions-alpha=0.4/m3/negtr=NA_postr=NA/bycountry/Finland/AFCO/2012-13/infomap/IM-membership.txt"
#	
#	[[2]][[1]]$membership
#	[1] 1 1 1 2 1 1 1 1 1 3 1 1 1
#	
#	[[2]][[1]]$plot.title
#	[1] "Finland-AFCO-2012-13  |.|  infomap\nI(P)=0.00000-%I(P)=0.00000, RI(P)=0.00000-%RI(P)=0.00000, #cluster: 2\n"
#	
#	[[2]][[1]]$desc
#	[1] "IM"
#	
#	
#	[[2]][[2]]
#	[[2]][[2]]$output.full.dir.name
#	[1] "../../data/NetVotes/out/nejat-partitions-alpha=0.4/m3/negtr=NA_postr=NA/bycountry/Finland/AFCO/2013-14/infomap"
#	
#	[[2]][[2]]$algo.output.file
#	[1] "../../data/NetVotes/out/nejat-partitions-alpha=0.4/m3/negtr=NA_postr=NA/bycountry/Finland/AFCO/2013-14/infomap/IM-membership.txt"
#	
#	[[2]][[2]]$membership
#	[1] 1 1 1 3 1 1 1 1 1 2 1 1 1
#	
#	[[2]][[2]]$plot.title
#	[1] "Finland-AFCO-2013-14  |.|  infomap\nI(P)=0.00000-%I(P)=0.00000, RI(P)=0.00000-%RI(P)=0.00000, #cluster: 2\n"
#	
#	[[2]][[2]]$desc
#	[1] "IM"
#	
#	
#	
#	[[1]]
#	[[1]][[1]]
#	[[1]][[1]]$output.full.dir.name
#	[1] "../../data/NetVotes/out/nejat-partitions-alpha=0.4/m3/negtr=NA_postr=NA/bycountry/Finland/AGRI/2012-13/seq_ils-cc=1-rcc=0-a=0.4-l=1-i=10-p=3-t=3600-k=0"
#	
#	[[1]][[1]]$algo.output.file
#	[1] "../../data/NetVotes/out/nejat-partitions-alpha=0.4/m3/negtr=NA_postr=NA/bycountry/Finland/AGRI/2012-13/seq_ils-cc=1-rcc=0-a=0.4-l=1-i=10-p=3-t=3600-k=0/cc-result.txt"
#	
#	[[1]][[1]]$membership
#	[1] 1 1 1 1 1 1 1 1 1 1 1 1 1 1
#	
#	[[1]][[1]]$plot.title
#	[1] "Finland-AGRI-2012-13  |.|  seq_ils-cc=1-rcc=0-a=0.4-l=1-i=10-p=3-t=3600-k=0\nI(P)=0.83149-%I(P)=0.02513, RI(P)=0.83149-%RI(P)=0.02513, #cluster: 1\n"
#	
#	[[1]][[1]]$desc
#	[1] "ILSCC"
#	
#	
#	[[1]][[2]]
#	[[1]][[2]]$output.full.dir.name
#	[1] "../../data/NetVotes/out/nejat-partitions-alpha=0.4/m3/negtr=NA_postr=NA/bycountry/Finland/AGRI/2013-14/seq_ils-cc=1-rcc=0-a=0.4-l=1-i=10-p=3-t=3600-k=0"
#	
#	[[1]][[2]]$algo.output.file
#	[1] "../../data/NetVotes/out/nejat-partitions-alpha=0.4/m3/negtr=NA_postr=NA/bycountry/Finland/AGRI/2013-14/seq_ils-cc=1-rcc=0-a=0.4-l=1-i=10-p=3-t=3600-k=0/cc-result.txt"
#	
#	[[1]][[2]]$membership
#	[1] 1 1 1 1 2 1 1 1 1 1 2 1 1
#	
#	[[1]][[2]]$plot.title
#	[1] "Finland-AGRI-2013-14  |.|  seq_ils-cc=1-rcc=0-a=0.4-l=1-i=10-p=3-t=3600-k=0\nI(P)=0.00000-%I(P)=0.00000, RI(P)=0.00000-%RI(P)=0.00000, #cluster: 2\n"
#	
#	[[1]][[2]]$desc
#	[1] "ILSCC"
#	
#	
#	
#	[[2]]
#	[[2]][[1]]
#	[[2]][[1]]$output.full.dir.name
#	[1] "../../data/NetVotes/out/nejat-partitions-alpha=0.4/m3/negtr=NA_postr=NA/bycountry/Finland/AGRI/2012-13/infomap"
#	
#	[[2]][[1]]$algo.output.file
#	[1] "../../data/NetVotes/out/nejat-partitions-alpha=0.4/m3/negtr=NA_postr=NA/bycountry/Finland/AGRI/2012-13/infomap/IM-membership.txt"
#	
#	[[2]][[1]]$membership
#	[1] 1 1 1 1 1 2 1 1 1 2 1 2 1 1
#	
#	[[2]][[1]]$plot.title
#	[1] "Finland-AGRI-2012-13  |.|  infomap\nI(P)=2.03030-%I(P)=0.06135, RI(P)=0.83149-%RI(P)=0.02513, #cluster: 2\n"
#	
#	[[2]][[1]]$desc
#	[1] "IM"
#	
#	
#	[[2]][[2]]
#	[[2]][[2]]$output.full.dir.name
#	[1] "../../data/NetVotes/out/nejat-partitions-alpha=0.4/m3/negtr=NA_postr=NA/bycountry/Finland/AGRI/2013-14/infomap"
#	
#	[[2]][[2]]$algo.output.file
#	[1] "../../data/NetVotes/out/nejat-partitions-alpha=0.4/m3/negtr=NA_postr=NA/bycountry/Finland/AGRI/2013-14/infomap/IM-membership.txt"
#	
#	[[2]][[2]]$membership
#	[1] 1 1 1 1 2 1 1 1 1 1 2 1 1
#	
#	[[2]][[2]]$plot.title
#	[1] "Finland-AGRI-2013-14  |.|  infomap\nI(P)=0.00000-%I(P)=0.00000, RI(P)=0.00000-%RI(P)=0.00000, #cluster: 2\n"
#	
#	[[2]][[2]]$desc
#	[1] "IM"


	
	###########################################################################
	# Plot multi graph year by year
	###########################################################################

	# TODO tek sikinti var: yaratacagim plot'lari nereye kaydedecegim bilgisi yok
	# target, domain ve period bilgisi yok yani
}



##############################################################################
##############################################################################
##############################################################################
##############################################################################
##############################################################################



###############################################################################
### MAIN
###############################################################################


source("src/main-iteration-content.R")



do.parallel.computing = function()
{

	
	###############################################################################
	# configure parallel processing
	cl <- makeCluster(NB.CORES.FOR.PARALLEL.COMPUTING) # manually set the number of processors to use
	clusterExport(
			cl=cl, 
			varlist=c( # all variables used in "src/main.R"
					"PARTITIONS",
					"NETWORKS",
					"LIBRARY.LOC.PATH",
					"LOG.UNIQUE.DESC",
					"LOG.FILENAME.PREFIX",
					"LOG.ENABLED",
					"ILSCC.ENABLED", 
					"EXCC.ENABLED", 
					"ILSRCC.ENABLED", 
					"KMBS.ENABLED", 
					"INFOMAP.ENABLED", 
					"MULTIPLOT.ENABLED",
					"RUNNING.PARTITIONING.ALGOS.ENABLED",
					"PLOTTING.ENABLED",
					"UPDATING.GRAPHML.CONTENT.ENABLED",
					"RECORDING.STATS.CSV",
					"TARGET.DOMAINS", 
					"TARGET.PERIODS",
					"TARGETS",
					"TARGET.TYPE",
					"FILTERING.THRESHOLD",
					"EXE.DIR", 
					"DATA.DIR", 
					"OUT.DIR",
					"do.iteration" # function
			),
			envir=globalenv()
	)
	
	
	clusterEvalQ(cl, LIBRARY.LOC.PATH) # this is necessary in order that each parallel worker could load the libraries
	registerDoParallel(cl)
	
	# note that when parallel computing is enabled, the number of log files to be produced length(FILTERING.THRESHOLDS)*length(TARGET.TYPES) 
	# because each time we start parallel computing with 'registerDoParallel(cl)'
	###############################################################################
	
	# The following pdf files, vignettes, introduce the parallelism in R.
	# library(doParallel)
	# vignette("nested")
	# vignette("foreach")
	# vignette("parallel")
	


#	final.result = foreach(target=TARGETS, .combine='rbind') %:%
#		foreach(domain=TARGET.DOMAINS, .combine='rbind') %:%
#		foreach(period=TARGET.PERIODS, .combine='comb', .final=USR.DEF.FUNC, .init=replicate(50, list(), simplify=FALSE) ) %dopar%
#		{
#			# TODO
#		}
#	print(final.result)
		
		
	
	# Parallel version of FOREACH
	foreach(target=TARGETS) %:% # do not add 'curly bracket at the end'
		foreach(domain=TARGET.DOMAINS, .combine='rbind') %:% # do not add 'curly bracket at the end'
			foreach(period=TARGET.PERIODS,
					.combine='comb', # user defined function. To use this function, first we need to initialize with ".init"
					.final=plot.network.by.time,  # user defined function
					#.multicombine=TRUE,

					.init=replicate(50, list(), simplify=FALSE) # list of list in which inner list has "lenght(plot.list)" items.
					# This is a WORKAROUND in order that user does not need to provide a number of items
					# init should be static. However, nb plots returned in each iteration are unknown beforehand
					# So, we put a large number like 50. In any case, comb() function will use  "nb.plot.inputs.returned" items
					# "nb.plot.inputs.returned" is nb plots returned in each iteration. If only "ExCC" and "ILSCC" are used, "nb.plot.inputs.returned" is 2
			) %dopar% 
			{
				source("src/define-imports.R")
			
#				worker.id = paste(Sys.info()[['nodename']], Sys.getpid(), sep='-')
				worker.id = Sys.getpid()
				
				# ==============================================================
				do.iteration(worker.id, target, domain, period)
				# ==============================================================
			}
	
	
	################################################################################
	# stop parallel processing
	stopCluster(cl)
	################################################################################
}
