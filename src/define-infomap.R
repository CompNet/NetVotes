

########################################################################
# 
########################################################################
create.output.IM.result.dir = function(output.directory){
	sub.dirname = COMDET.INFOMAP
	dir.name = paste(output.directory, sub.dirname, sep="/")
	dir.create(dir.name, showWarnings = FALSE)
	
	return(sub.dirname)
}



########################################################################
# 
########################################################################
prepare.IM.full.output.filename = function(IM.directory){
	
	return(paste(IM.directory, INFOMAP.MEM.FILENAME, sep="/"))
}





########################################################################
# 
########################################################################
load.IM.partition = function(full.output.filename){
	membership = read.table(full.output.filename)$V1 # infomap: membership.txt file
	
	return(membership)
}


########################################################################
# 
########################################################################
prepare.IM = function(output.directory)
{
	inputs = list()
	output.dir.basename = create.output.IM.result.dir(output.directory)
	output.full.dir.name = paste(output.directory, output.dir.basename, sep="/")
	algo.output.file = 
			prepare.IM.full.output.filename(output.full.dir.name)
	
	inputs$output.dir.basename = output.dir.basename
	inputs$output.full.dir.name = output.full.dir.name
	inputs$algo.output.file = algo.output.file
	
	return(inputs)
}





########################################################################
# Vincent Labatut daha onceden calistirmis Infomap result'larini.
# O yuzden gerek yok benim bir daha burda calistirmaya
########################################################################
run.IM = function(network.path, full.output.filename){		
	g = read.graph.graphml(network.path)

	
    	# ===========================================================
	# remove negative links from the graph
	gpos <- delete.edges(graph=g,edges=which(E(g)$weight<0))
	# ===========================================================

	start=Sys.time()
	imc <- cluster_infomap(gpos)
	end=Sys.time()
	exec.time = as.numeric(end) - as.numeric(start)
	
	membership = membership(imc)
	write(membership, file=full.output.filename, ncolumns=1)
    	

	
	return(exec.time)
}




########################################################################
# 
########################################################################
perform.IM = function(worker.id, desc, output.directory, output.dir.desc,
		target.name, network.path.G, network.path.graphml, current.MEP.details,
		LOG.ENABLED = TRUE, RUNNING.PARTITIONING.ALGOS.ENABLED = TRUE, UPDATING.GRAPHML.CONTENT.ENABLED = TRUE,
		PLOTTING.ENABLED = TRUE)
{	
	result = list(exec.time = NA, stats.vec = NA, plot.inputs = NA)
	
	# =========================================================================
	if(LOG.ENABLED)
		write.into.log(worker.id, 12, ".........BEGIN PREPARE.INFOMAP.........")
	
	inputs = prepare.IM(
			output.directory = output.directory
	)
	
	if(LOG.ENABLED)
		write.into.log(worker.id, 12, ".........END PREPARE.INFOMAP.........")
	# =========================================================================

	
	# =========================================================================
	if(RUNNING.PARTITIONING.ALGOS.ENABLED == TRUE){
		if(LOG.ENABLED)
			write.into.log(worker.id, 12, ".........BEGIN RUN.INFOMAP.........")
		
	    
	    fpath = paste(inputs$output.full.dir.name,"/",EXEC.TIME.FILENAME,sep="")
	    if( (!dir.exists(inputs$output.full.dir.name) || !file.exists(fpath)) || FORCE){
	        exec.time = run.IM(
	            network.path = network.path.graphml,
	            full.output.filename = inputs$algo.output.file
	        )
	    } else {
	        exec.time = read.table(file=fpath)$V1
	        #print(exec.time)
	    }
		
		
		# save exec.time: write into file
		write(x=exec.time, file=paste(inputs$output.full.dir.name,"/",EXEC.TIME.FILENAME,sep=""))
		
		if(LOG.ENABLED)
			write.into.log(worker.id, 12, ".........END RUN.INFOMAP.........")	
	}
	
	# in case of RUNNING.PARTITIONING.ALGOS.ENABLED == FALSE, read the exec.time from the file
	result$exec.time = as.numeric(readLines(con=paste(inputs$output.full.dir.name,"/",EXEC.TIME.FILENAME,sep="")))
	
	membership = load.IM.partition(inputs$algo.output.file)
	membership = post.proc.membership.for.isolated.nodes(network.path.graphml, membership)
	
	
	if(LOG.ENABLED){
		write.into.log(worker.id, 12, "result$exec.time", result$exec.time)
		write.into.log(worker.id, 12, "membership", membership)		
	}
	# =========================================================================
	
	
	# ===================================================================
	if(UPDATING.GRAPHML.CONTENT.ENABLED == TRUE){
		# update graphml file with kmbs partition info
		attr.name = desc
		newGraphDoc = addPartitionInfoIntoNodes(
				network.path.graphml, attr.name, membership
		)
		saveXML(newGraphDoc, file=network.path.graphml)	
	}
	# =========================================================================
	
	
	# =========================================================================
	result$stats.vec = perform.stats.after.partitioning(
			network.path = network.path.graphml,
			membership = membership
	)
	
	plot.title = make.plot.title(output.dir.desc, inputs$output.dir.basename, result$stats.vec)
	result$plot.inputs = list(
			output.full.dir.name=inputs$output.full.dir.name,
			algo.output.file = inputs$algo.output.file,
			membership = membership,
			plot.title = plot.title,
			desc = desc
	)
	# =========================================================================
	
	
	# =========================================================================
	if(PLOTTING.ENABLED == TRUE){
		if(LOG.ENABLED)
			write.into.log(worker.id, 12, ".........BEGIN PLOTTING.........")
		
	    
	    g = read.graph(network.path.graphml, "graphml")
	    if( TARGET.TYPE == "Group" ){
	        print("circos group")
	        # plot circos - all links
	        produce.circos.plot.by.countries(g, partition=membership, part.algo.name=desc, absences=NA, abstentions=NA, 
                     show.imbalance.participation=FALSE, show.names=TRUE, show.clusters=TRUE, show.restricted.countries=FALSE,
                     show.restricted.groups=FALSE, show.restricted.clusters=FALSE, out.folder=inputs$output.full.dir.name, clean.files=TRUE)
	    } else { #  if( TARGET.TYPE == "State" )
	        print("circos state")
	        # plot circos - all links
	        produce.circos.plot.by.pol.groups(g, partition=membership, part.algo.name=desc, absences=NA, abstentions=NA, 
                      show.imbalance.participation=FALSE, show.names=TRUE, show.clusters=TRUE, show.restricted.countries=FALSE,
                      show.restricted.groups=FALSE, show.restricted.clusters=FALSE, out.folder=inputs$output.full.dir.name, clean.files=TRUE)
	    }
	    # ====================================
	    #         all.svg.files = list.files(path=inputs$output.full.dir.name,pattern=paste0(".*\\.svg$"),full.names=FALSE)
	    #         for(svg.file in all.svg.files){
	    #             filename = gsub(".svg","",svg.file)
	    #             print(svg.file)
	    #             print(inputs$output.full.dir.name)
	    #             convert.cmd <- paste0("convert -quality 10% ",inputs$output.full.dir.name,"/",svg.file," ",inputs$output.full.dir.name,"/",filename,".jpg")
	    #             print(convert.cmd)
	    #             system(command=convert.cmd)
	    # 	    }
	    ### if svg is not a desired format, delete svg files created by circos
	    # unlink(list.files(path=part.folder,pattern=paste0(".*\\.svg$"),full.names=TRUE))
	    # ====================================
	    
	    
		# # with circular layout
		# plot.graph(
		# 		network.path = network.path.graphml,
		# 		plot.inputs = result$plot.inputs, target.type = TARGET.TYPE,
		# 		output.directory = output.directory, output.dir.desc = output.dir.desc,
		# 		current.MEP.details = current.MEP.details,
		# 		circ.layout.enabled=FALSE,
		# 		node.shape.enabled=FALSE, imbalance.edge.contribution.enabled=FALSE, node.id.label.enabled=TRUE
		# )
		# # without circular layout
		# plot.graph(
		# 		network.path = network.path.graphml,
		# 		plot.inputs = result$plot.inputs, target.type = TARGET.TYPE,
		# 		output.directory = output.directory, output.dir.desc = output.dir.desc,
		# 		current.MEP.details = current.MEP.details,
		# 		circ.layout.enabled=TRUE,
		# 		node.shape.enabled=FALSE, imbalance.edge.contribution.enabled=FALSE, node.id.label.enabled=TRUE
		# )
	    
	    
		plot.treemap(
				network.path=network.path.graphml, plot.inputs = result$plot.inputs,
				target.type=TARGET.TYPE, target.name=target.name
		)
		
		
		output.full.path = paste(output.directory, "/", inputs$output.dir.basename, "/", CLUSTER.GRAPH.FILENAME, sep="")
		create.cluster.graph(
				TARGET.TYPE,
				network.path.graphml, 
				membership, 
				MEP.DETAILS, 
				output.full.path = output.full.path
		)
		
		if(LOG.ENABLED)
			write.into.log(worker.id, 12, ".........END PLOTTING.........")
	}
	# =========================================================================
	
	return(result)
}


