#!/usr/bin/Rscript

# TODO: Add comment
# 
# Author: nejat
###############################################################################


do.sequential.computing = function()
{
	
	for(target in TARGETS)
		for(domain in TARGET.DOMAINS)
			for(period in TARGET.PERIODS)
			{
				source("src/define-imports.R")
				
				#worker.id = paste(Sys.info()[['nodename']], Sys.getpid(), sep='-')
				worker.id = Sys.getpid()
				
				# ==============================================================
				do.iteration(worker.id, target, domain, period)
				# ==============================================================
				
			}
}

