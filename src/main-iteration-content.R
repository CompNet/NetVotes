# TODO: Add comment
# 
# Author: nejat
###############################################################################





###############################################################################
# This is a wrapper function in order to run multiple partitioning algorithms onto the input graphs.
# The steps are (be aware that lots of global variables are used):
# - copy graph files in .G and .graphml format from input directory to otput directory (later, we encode partition infos into the graphml file )
# - create a new graph file based on the transformation of the weighted graph into unweighted (fo kmbs)
# - retreive MEP details from .graphml file
# - run ILS-CC and plot individually with circos
# - run ILS-RCC and plot the result of this algo with circos
# - run ExCC and plot the result of this algo with circos
# - run KMBS and plot the result of this algo with circos
# - run Infomap and plot the result of this algo with circos
# - multi plot based on the results of the partitioning algorithms
#       Concretely, multiplot of ExCC and ILSCC
# - encode all partition infos into the graphml file
#
###############################################################################
do.iteration = function(worker.id = NA, target, domain, period){

	if(LOG.ENABLED)
		write.into.log(worker.id, 4, ".........WE ARE IN DO.ITERATION() - BEGIN..........")

	
	# ===========================================================
	# store all plots in this list
	plot.list = list()
	# ===========================================================
	
	input.directory = paste(NETWORKS, "/", target, "/", domain, "/", period, sep="")
	
	output.target.directory = paste(PARTITIONS, "/", target, sep="")
	output.domain.directory = paste(output.target.directory, "/", domain, sep="")
	output.directory = paste(output.domain.directory, "/", period, sep="")
	output.dir.desc = paste(target,"-",domain,"-",period,sep="")
	dir.create(output.directory, recursive = TRUE, showWarnings = FALSE)
	
	
	network.path.G = paste(input.directory, SIGNED.GRAPH.FILENAME, sep="/")
	network.path.graphml.input = paste(input.directory, GRAPHML.NETWORK.FILENAME, sep="/")
	print(network.path.graphml.input)
	network.path.graphml.output = paste(output.directory, GRAPHML.NETWORK.FILENAME, sep="/")

    if(file.exists(network.path.graphml.input)){
	    # just copy the .G file into output dir (for the ease of access to .G file from output)
	    file.copy(
			    network.path.G,
			    file.path(output.directory, SIGNED.GRAPH.FILENAME, fsep="/"),
			    overwrite=TRUE,
			    recursive=FALSE,
			    copy.mode=TRUE,
			    copy.date=FALSE
	    )
	    
	    # just copy the .G file into output dir (for the ease of access to .G file from output)
	    file.copy(
	        network.path.graphml.input,
	        network.path.graphml.output,
	        overwrite=TRUE,
	        recursive=FALSE,
	        copy.mode=TRUE,
	        copy.date=FALSE
	    )
	    
	    
	    # for KMBS
	    unweighted.network.path.G = paste(input.directory,SIGNED.UNWEIGHTED.GRAPH.FILENAME,sep="/")
	    convert.weight.into.unweight.ils.input.graph(network.path.G, unweighted.network.path.G)
	    

	    
	    ############################################################################
	    # in order to acces mep details, use "MEP.DETAILS"
	    
	    beg.curr.period = as.Date(paste(EUROPARL.BEGIN.PERIOD.DATE.WITHOUT.YEAR,period,sep=""), format="%d/%m/%Y")
	    end.curr.period = beg.curr.period + EUROPARL.PERIOD.LENGTH.AS.DAYS
	    
	    g.ml.temp = read.graph(network.path.graphml.input, "graphml")
	    curr.mep.indx.vec = V(g.ml.temp)$MEPid
	    current.MEP.details = MEP.DETAILS[curr.mep.indx.vec, ]
	    nb.obs = nrow(current.MEP.details)
	    network.size = get.network.size(network.path.graphml.input)
	    

	    
	    #print(network.path.graphml.input)
        #print(current.MEP.details[, "MEP Id"])
	    
	    # target.indx = which(MEP.DETAILS[ ,TARGET.TYPE] == target)
	    # target.MEP.DETAILS = MEP.DETAILS[target.indx, ]
	    # nb.obs = nrow(target.MEP.DETAILS)
	    # 
	    # curr.mep.indx.list = c()
	    # for(i in seq(1, nb.obs)){
	    # 	# in the "Periods" column, "NA" sometimes appears, we handle with that via "gsub()"
	    # 	# "01/05/2017" is chosen arbitrary, which is more recent than 2014 (last period)
	    # 	date.list = gsub("NA", "01/05/2017", as.character(MEP.DETAILS[i, "Periods"])) 
	    # 	s.date.interval.list = unlist(strsplit(x=date.list, split="::"))
	    # 	
	    # 	mep.availability.check.list = sapply(s.date.interval.list,
	    # 			function(s.date.interval){
	    # 			s.date.list = unlist(strsplit(x=s.date.interval, split=":"))
	    # 			date.list = as.Date(s.date.list, format="%d/%m/%Y")
	    # 			beg.period.mep = date.list[1]
	    # 			end.period.mep = date.list[2]
	    # 			
	    # 			# there are 2 posibilities:
	    # 			# 1) beg.period.mep = "01/01/2009", end.period.mep = "01/07/2014",
	    # 			# beg.curr.period = "01/07/2009", end.curr.period = "01/07/2010"
	    # 			case1 = beg.period.mep < beg.curr.period && beg.curr.period < end.period.mep
	    # 			
	    # 			# 2) beg.period.mep = "16/07/2009", end.period.mep = "01/07/2014",
	    # 			# beg.curr.period = "01/07/2009", end.curr.period = "01/07/2010"
	    # 			case2 = beg.curr.period < beg.period.mep && beg.period.mep < end.curr.period
	    # 			
	    # 			return(case1 || case2)
	    # 	})
	    # 
	    # 	if(any(mep.availability.check.list == TRUE))
	    # 		curr.mep.indx.list = c(curr.mep.indx.list, i)
	    # }
	    # 
	    # #current.MEP.details = target.MEP.DETAILS[curr.mep.indx.list, ]
	    # current.MEP.details = target.MEP.DETAILS
	    # ############################################################################
	    # 
	    # 
	    # # convert .G file into .graphml file in output dir (this is the case where graphml fiels are not rpesent in the input directory)
	    # gg = read.graph.ils(network.path.G)
	    # V(gg)$MEPid = current.MEP.details[, "MEP Id"]
	    # V(gg)$Country = current.MEP.details[, "State"]
	    # V(gg)$Group = current.MEP.details[, "Group"]
	    # V(gg)$Firstname = current.MEP.details[, "Firstname"]
	    # V(gg)$Lastname = current.MEP.details[, "Lastname"]
	    # write.graph(graph=gg, file=network.path.graphml.output, format="graphml")
	    # network.size = get.network.size(network.path.graphml.output)

	    

	    
	    if(LOG.ENABLED){ 
		    write.into.log(worker.id, 4, "network.path.G: ", network.path.G)
		    write.into.log(worker.id, 4, "network.path.graphml.output: ", network.path.graphml.output)
		    write.into.log(worker.id, 4, "unweighted.network.path.G: ", unweighted.network.path.G)
		    write.into.log(worker.id, 4, ".........BEGIN TRY/CATCH..........")
	    }

	    
	    
	    #try({	
				    
		    ###################################################################
		    #  ILS
		    ###################################################################
		    
		    if(ILSCC.ENABLED){	
			    # ====================
			    # without k-min
			    # ====================	
				    
			    if(LOG.ENABLED)
				    write.into.log(worker.id, 4, ".........BEGIN ILSCC WITHOUT K-MIN..........")
			    
			    desc = "ILSCC"
			    result = perform.ils.cc(worker.id = worker.id, desc = desc,
					    output.directory = output.directory, output.dir.desc = output.dir.desc,
					    target.name = target, network.path.G = network.path.G,
					    network.path.graphml = network.path.graphml.output, network.size = network.size, 
					    current.MEP.details = current.MEP.details,
					    LOG.ENABLED = LOG.ENABLED,
					    RUNNING.PARTITIONING.ALGOS.ENABLED = RUNNING.PARTITIONING.ALGOS.ENABLED,
					    UPDATING.GRAPHML.CONTENT.ENABLED = UPDATING.GRAPHML.CONTENT.ENABLED,
					    PLOTTING.ENABLED = PLOTTING.ENABLED)
			    
			    exec.time = result$exec.time
			    stats.vec = result$stats.vec
			    plot.inputs = result$plot.inputs
			    
			    plot.list[[desc]] = plot.inputs
			    
			    
			    k.ILSCC = get.nb.cluster.from.membership(plot.inputs$membership)
			    
			    if(RECORDING.STATS.CSV){
				    source("stats/common.R")
				    desc.part = prepare.curr.row.desc.part.for.real.instances(target, domain, period, FILTERING.THRESHOLD)
				    filename = ILSCC.STATS.CSV.FILENAME
				    dir.path = paste(DIR.REAL.INSTANCES.CSV,TARGET.TYPE,sep="/")
				    insert.curr.row.into.partitioning.stats.csv(desc.part, dir.path, filename, result, FILTERING.THRESHOLD)
			    }
		    
			    if(LOG.ENABLED)
				    write.into.log(worker.id, 4, ".........END ILSCC WITHOUT K-MIN..........")
		    }
		    
		    
		    ####################################################################
		    # ExCC
		    ####################################################################

		    if(EXCC.ENABLED){
			    if(LOG.ENABLED)
				    write.into.log(worker.id, 4, ".........BEGIN EXCC..........")
			    
			    desc = "ExCC"
			    result = perform.ExCC(worker.id = worker.id, desc = desc,
					    output.directory = output.directory, output.dir.desc = output.dir.desc,
					    target.name = target, network.path.G = network.path.G,
					    network.path.graphml = network.path.graphml.output, current.MEP.details = current.MEP.details,
					    LOG.ENABLED = LOG.ENABLED,
					    RUNNING.PARTITIONING.ALGOS.ENABLED = RUNNING.PARTITIONING.ALGOS.ENABLED,
					    UPDATING.GRAPHML.CONTENT.ENABLED = UPDATING.GRAPHML.CONTENT.ENABLED,
					    PLOTTING.ENABLED = PLOTTING.ENABLED)
			    
			    exec.time = result$exec.time
			    stats.vec = result$stats.vec
			    plot.inputs = result$plot.inputs
			    
			    plot.list[[desc]] = plot.inputs
			    
			    if(RECORDING.STATS.CSV){
				    source("stats/common.R")
				    desc.part = prepare.curr.row.desc.part.for.real.instances(target, domain, period, FILTERING.THRESHOLD)
				    filename = EXCC.STATS.CSV.FILENAME
				    dir.path = paste(DIR.REAL.INSTANCES.CSV,TARGET.TYPE,sep="/")
				    insert.curr.row.into.partitioning.stats.csv(desc.part, dir.path, filename, result, FILTERING.THRESHOLD)
			    }
			    
			    if(LOG.ENABLED)
				    write.into.log(worker.id, 4, ".........END EXCC..........")
			    
			    
		    }
		    
		    
		    ####################################################################
		    # ILS-RCC
		    #####################################################################					
		    
		    if(ILSRCC.ENABLED){
		        # just read the result

		        desc = "ILSCC"
		        result = perform.ils.cc(worker.id = worker.id, desc = desc,
                                output.directory = output.directory, output.dir.desc = output.dir.desc,
                                target.name = target, network.path.G = network.path.G,
                                network.path.graphml = network.path.graphml.output, network.size = network.size, 
                                current.MEP.details = current.MEP.details,
                                LOG.ENABLED = LOG.ENABLED,
                                RUNNING.PARTITIONING.ALGOS.ENABLED = RUNNING.PARTITIONING.ALGOS.ENABLED,
                                UPDATING.GRAPHML.CONTENT.ENABLED = FALSE,
                                PLOTTING.ENABLED = FALSE)

		        k.ILSCC = length(unique(result$plot.inputs$membership))
		        
			    for(pair in list(
					    c(paste("ILS-RCC_k-from=ILS-CC_k=",k.ILSCC,sep="")	  , k.ILSCC)
					    #c(paste("ILS-RCC_k-from=ILS-CC_k+1=",k.ILSCC+1,sep=""), k.ILSCC+1),
					    #c(paste("ILS-RCC_k-from=ILS-CC_k+2=",k.ILSCC+2,sep=""), k.ILSCC+2),
					    #c(paste("ILS-RCC_k-from=ILS-CC_k+3=",k.ILSCC+3,sep=""), k.ILSCC+3),
					    #c(paste("ILS-RCC_k-from=ILS-CC_k+4=",k.ILSCC+4,sep=""), k.ILSCC+4)
			    )
					    ){
			        
				    desc = pair[1]
				    k.value = pair[2]
				    
				    if(LOG.ENABLED)
					    write.into.log(worker.id, 4, ".........BEGIN ILS-RCC with k=", k.value,"..........")
				    
				    result = perform.ils.rcc(worker.id = worker.id, k = k.value, k.from = CORCLU.ILS, desc = desc,
						    output.directory = output.directory, output.dir.desc = output.dir.desc,
						    target.name = target, network.path.G = network.path.G,
						    network.path.graphml = network.path.graphml.output, network.size = network.size, 
						    current.MEP.details = current.MEP.details,
						    LOG.ENABLED = LOG.ENABLED,
						    RUNNING.PARTITIONING.ALGOS.ENABLED = RUNNING.PARTITIONING.ALGOS.ENABLED,
						    UPDATING.GRAPHML.CONTENT.ENABLED = UPDATING.GRAPHML.CONTENT.ENABLED,
						    PLOTTING.ENABLED = PLOTTING.ENABLED)
				    
				    exec.time = result$exec.time
				    stats.vec = result$stats.vec
				    plot.inputs = result$plot.inputs
				    
				    plot.list[[desc]] = plot.inputs
				    
				    if(RECORDING.STATS.CSV){
					    source("stats/common.R")
					    desc.part = prepare.curr.row.desc.part.for.real.instances(target, domain, period, FILTERING.THRESHOLD)
					    filename = paste("k", k.value, "-", ILSRCC.STATS.CSV.FILENAME, sep="")
					    dir.path = paste(DIR.REAL.INSTANCES.CSV,TARGET.TYPE,sep="/")
					    insert.curr.row.into.partitioning.stats.csv(desc.part, dir.path, filename, result, FILTERING.THRESHOLD)
				    }
				    
				    if(LOG.ENABLED)
					    write.into.log(worker.id, 4, ".........END ILS-RCC with k=", k.value,"..........")
				    
			    }
		        
		        
		    }
		    
	    
		    ####################################################################
		    # KMBS - k, k+1, k+2
		    ####################################################################
		    
		    if(KMBS.ENABLED){
			    
			    for(pair in list(
					    c(paste("KMBS-k=",k.ILSCC,sep="")  , k.ILSCC)
					    #c(paste("KMBS-k=",k.ILSCC+1,sep=""), k.ILSCC+1),
					    #c(paste("KMBS-k=",k.ILSCC+2,sep=""), k.ILSCC+2)
			    )
					    ){
				    desc = pair[1]
				    k.value = pair[2]
				    
				    if(LOG.ENABLED)
					    write.into.log(worker.id, 4, ".........BEGIN KMBS with k=", k.value,"..........")
				    
				    result = perform.KMBS(worker.id = worker.id, k = k.value, desc = desc,
						    output.directory = output.directory, output.dir.desc = output.dir.desc,
						    target.name = target, network.path.G = network.path.G,
						    network.path.graphml = network.path.graphml.output, current.MEP.details = current.MEP.details,
						    LOG.ENABLED = LOG.ENABLED,
						    RUNNING.PARTITIONING.ALGOS.ENABLED = RUNNING.PARTITIONING.ALGOS.ENABLED,
						    UPDATING.GRAPHML.CONTENT.ENABLED = UPDATING.GRAPHML.CONTENT.ENABLED,
						    PLOTTING.ENABLED = PLOTTING.ENABLED)
				    
				    exec.time = result$exec.time
				    stats.vec = result$stats.vec
				    plot.inputs = result$plot.inputs
				    
				    plot.list[[desc]] = plot.inputs
								    
				    if(RECORDING.STATS.CSV){
					    source("stats/common.R")
					    desc.part = prepare.curr.row.desc.part.for.real.instances(target, domain, period, FILTERING.THRESHOLD)
					    filename = paste("k", k.value, "-", KMBS.STATS.CSV.FILENAME, sep="")
					    dir.path = paste(DIR.REAL.INSTANCES.CSV,TARGET.TYPE,sep="/")
					    insert.curr.row.into.partitioning.stats.csv(desc.part, dir.path, filename, result, FILTERING.THRESHOLD)
				    }
				    
				    if(LOG.ENABLED)
					    write.into.log(worker.id, 4, ".........END KMBS with k=", k.value,"..........")
			    }
		    
		    }
		    
		    ####################################################################
		    # Infomap
		    ####################################################################
		    
		    if(INFOMAP.ENABLED){
			    
			    if(LOG.ENABLED)
				    write.into.log(worker.id, 4, ".........BEGIN INFOMAP..........")
			    
			    desc = "IM"
			    result = perform.IM(worker.id = worker.id, desc = desc,
					    output.directory = output.directory, output.dir.desc = output.dir.desc,
					    target.name = target, network.path.G = network.path.G,
					    network.path.graphml = network.path.graphml.output, current.MEP.details = current.MEP.details,
					    LOG.ENABLED = LOG.ENABLED,
					    RUNNING.PARTITIONING.ALGOS.ENABLED = RUNNING.PARTITIONING.ALGOS.ENABLED,
					    UPDATING.GRAPHML.CONTENT.ENABLED = UPDATING.GRAPHML.CONTENT.ENABLED,
					    PLOTTING.ENABLED = PLOTTING.ENABLED)
			    
			    exec.time = result$exec.time
			    stats.vec = result$stats.vec
			    plot.inputs = result$plot.inputs
			    
			    plot.list[[desc]] = plot.inputs
			    
			    infomap.output.file = plot.inputs$algo.output.file
			    
			    if(RECORDING.STATS.CSV){
				    source("stats/common.R")
				    desc.part = prepare.curr.row.desc.part.for.real.instances(target, domain, period, FILTERING.THRESHOLD)
				    filename = IM.STATS.CSV.FILENAME
				    dir.path = paste(DIR.REAL.INSTANCES.CSV,TARGET.TYPE,sep="/")
				    insert.curr.row.into.partitioning.stats.csv(desc.part, dir.path, filename, result, FILTERING.THRESHOLD)
			    }
			    
			    if(LOG.ENABLED)
				    write.into.log(worker.id, 4, ".........END INFOMAP..........")
		    
		    }
		    
    #		####################################################################
    #		# ILS.CC and init partition from IM
    #		####################################################################
    #		
    #		if(LOG.ENABLED)
    #			write.into.log(worker.id, 4, ".........BEGIN ILSCC & INIT PARTITION FROM INFOMAP..........")
    #		
    #		desc = paste("ILS-CC", "_Init-Partition-from-IM-1-iter",sep="")
    #		
    #		result = perform.ils.cc(init.partition.file = infomap.output.file, init.partition.from = COMDET.INFOMAP,
    #				worker.id = worker.id, desc = desc,
    #				output.directory = output.directory, output.dir.desc = output.dir.desc,
    #				target.name = target, network.path.G = network.path.G,
    #				network.path.graphml = network.path.graphml.output, network.size = network.size, 
    #				current.MEP.details = current.MEP.details,
    #				LOG.ENABLED = LOG.ENABLED,
    #				RUNNING.PARTITIONING.ALGOS.ENABLED = RUNNING.PARTITIONING.ALGOS.ENABLED,
    #				UPDATING.GRAPHML.CONTENT.ENABLED = UPDATING.GRAPHML.CONTENT.ENABLED,
    #				PLOTTING.ENABLED = PLOTTING.ENABLED)
    #		
    #		exec.time = result$exec.time
    #		stats.vec = result$stats.vec
    #		plot.inputs = result$plot.inputs
    #		
    #		plot.list[[desc]] = plot.inputs
    #		
    #		if(LOG.ENABLED)
    #			write.into.log(worker.id, 4, ".........END ILSCC & INIT PARTITION FROM INFOMAP..........")
    #		
    #		


		    ####################################################################
		    # MULTI PLOT - Plot only cc, rcc.from.cc 
		    ####################################################################
		    if(PLOTTING.ENABLED == TRUE){
			    if(MULTIPLOT.ENABLED && EXCC.ENABLED && ILSCC.ENABLED){

				    if(LOG.ENABLED)
					    write.into.log(worker.id, 4, ".........BEGIN MULTI PLOT..........")

				    plots.inputs =
						    list(
								    plot.list[["ExCC"]],
								    plot.list[["ILSCC"]]
						    )

				    plot.multigraph(
						    desc = "ExCC-ILSCC",
						    network.path = network.path.graphml.output,
						    plots.inputs = plots.inputs, target.type = TARGET.TYPE,
						    output.directory = output.directory, output.dir.desc = output.dir.desc,
						    current.MEP.details = current.MEP.details,
						    circ.layout.enabled=TRUE,
						    node.shape.enabled=FALSE, imbalance.edge.contribution.enabled=FALSE, node.id.label.enabled=FALSE
				    )

				    if(LOG.ENABLED)
					    write.into.log(worker.id, 4, ".........END MULTI PLOT..........")
			    }
		    }
		    
		    ####################################################################
		    
		    if(RECORDING.STATS.CSV){
			    # ==================================================================
			    # graph-structure-info
			    # ==================================================================
			    source("stats/common.R")
			    desc.part = prepare.curr.row.desc.part.for.real.instances(target, domain, period, FILTERING.THRESHOLD)
			    filename = GRAPH.STR.STATS.CSV.FILENAME
			    dir.path = paste(DIR.REAL.INSTANCES.CSV,TARGET.TYPE,sep="/")
			    insert.curr.row.into.graph.stats.csv(desc.part, dir.path, filename, network.path.graphml.output, FILTERING.THRESHOLD)
		    }
		    
		    
		    ## =========================================================================
		    if(LOG.ENABLED)
			    write.into.log(worker.id, 4, ".........BEGIN CREATING GEPHI GRAPHML..........")
		    
		    # at the end, convert graphml file into gephi-graphml file
		    # What is specific in the format of gephi-graphml is not using negative edge signs
		    # So, we remove negative edge signs and add them as a new edge atrribute in the graphml file
		    
		    file.path = file.path(output.directory, GRAPHML.NETWORK.FILENAME, fsep="/")
		    new.file.path = file.path(output.directory, GRAPHML.GEPHI.NETWORK.FILENAME, fsep="/")
		    newGraphDoc = addSignAttrIntoGraphmlFiles(file.path)
		    saveXML(newGraphDoc, file=new.file.path)
		    
		    if(LOG.ENABLED)
			    write.into.log(worker.id, 4, ".........END CREATING GEPHI GRAPHML..........")
		    ## =========================================================================
			    
	    #}, silent=FALSE) # END TRY BLOCK
	    
	    if(LOG.ENABLED){
		    write.into.log(worker.id, 4, ".........END TRY/CATCH..........")
		    write.into.log(worker.id, 4, ".........EXIT DO.ITERATION() - END..........")
	    }


    }
		
	
	return(plot.list)
}
