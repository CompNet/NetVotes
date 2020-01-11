# TODO: Add comment
# 
# Author: nejat
###############################################################################


########################################################################
# 
########################################################################
get.tmp.partition.no.for.removed.nodes.by.KMBS = function(membership){
	# I can not plot with a partition no which is -1
	# I replace -1 by nb.partition
	# It is 100% sure that there is not another partition no whose id=nb.partition
	
	nb.partition = length(unique(membership))
	return(nb.partition)
}



########################################################################
# 
########################################################################
load.KMBS.partition = function(kmbs.output.file){
	con <- file(kmbs.output.file, "r")
	lines <- readLines(con)
	close(con)
	
	# process the file content
	i <- 2
	line = lines[i]
	counter = 0
	membership <- c()
	while(is.na(suppressWarnings(as.integer(line))) == FALSE)
	{  # process current line
		#print(line)
		
		counter = counter + 1
		membership[counter] <- as.numeric(line)
		
		# process next line
		i = i + 1
		line <- lines[i]      
	}

	return(membership)
	
}



########################################################################
# 
########################################################################
update.KMBS.membership.for.plot = function(mems){
	
	# =====================================
	# I can not plot with a partition no which is -1
	# I replace -1 by nb.partition
	# It is 100% sure that there is not another partition no whose id=nb.partition
	tmp.part.no = get.tmp.partition.no.for.removed.nodes.by.kmbs(mems)
	
	mems[which(mems == PARTITION.NO.FOR.REMOVED.NODES.BY.KMBS)] = tmp.part.no
	# ======================================
	
	return(mems)
	
}



########################################################################
# 
########################################################################
create.output.KMBS.result.dir = function(output.directory, k){
	sub.dirname = paste(KMBS, "-", k, sep="")
	dir.name = paste(output.directory, sub.dirname, sep="/")
	dir.create(dir.name, showWarnings = FALSE)
	
	return(sub.dirname)
}



########################################################################
# 
########################################################################
prepare.KMBS.full.output.filename = function(kmbs.directory){
	return(paste(kmbs.directory, KMBS.RESULT.FILENAME, sep="/"))
	
}



########################################################################
# 
########################################################################
prepare.KMBS = function(output.directory, k)
{
	inputs = list()
	
	output.dir.basename = create.output.KMBS.result.dir(output.directory, k)
	output.full.dir.name = paste(output.directory, output.dir.basename, sep="/")
	algo.output.file = prepare.KMBS.full.output.filename(output.full.dir.name)
	
	inputs$output.dir.basename = output.dir.basename
	inputs$output.full.dir.name = output.full.dir.name
	inputs$algo.output.file = algo.output.file
	
	return(inputs)
}



########################################################################
# 
########################################################################
run.KMBS = function(network.path, full.output.filename, k){

    start=Sys.time()
    cmd = 
	    paste(
			    KMBS.EXECUTABLE.PATH,
			    paste0("'",network.path,"'"),
			    paste0("'",full.output.filename,"'"),
			    k,
			    sep=" "
		    )
    system(cmd, wait=TRUE)
    end=Sys.time()
    exec.time = as.numeric(end) - as.numeric(start)
    print(cmd)
	
	return(exec.time)
}



########################################################################
# 
########################################################################
perform.KMBS = function(worker.id, k, desc, output.directory, output.dir.desc,
		target.name, network.path.G, network.path.graphml, current.MEP.details,
		LOG.ENABLED = TRUE, RUNNING.PARTITIONING.ALGOS.ENABLED = TRUE, UPDATING.GRAPHML.CONTENT.ENABLED = TRUE,
		PLOTTING.ENABLED = TRUE)
{
	result = list(exec.time = NA, stats.vec = NA, plot.inputs = NA)
	
	# =========================================================================
	if(LOG.ENABLED)
		write.into.log(worker.id, 12, ".........BEGIN PREPARE.KMBS.........")
	
	inputs = prepare.KMBS(
			output.directory = output.directory,
			k = k
	)
	
	if(LOG.ENABLED)
		write.into.log(worker.id, 12, ".........END PREPARE.KMBS.........")
	# =========================================================================
	
	# =========================================================================
	if(RUNNING.PARTITIONING.ALGOS.ENABLED == TRUE){
		if(LOG.ENABLED)
			write.into.log(worker.id, 12, ".........BEGIN RUN.KMBS.........")
		
        exec.time = NA
        #print(output.full.dir.name)
        fpath = paste(inputs$output.full.dir.name,"/",EXEC.TIME.FILENAME,sep="")
        #print(fpath)
        if( (!dir.exists(inputs$output.full.dir.name) || !file.exists(fpath)) || FORCE){

		    exec.time = run.KMBS(
				    network.path = network.path.G,
				    full.output.filename = inputs$algo.output.file,
				    k = k
		    )
        } else {
            exec.time = read.table(file=fpath)$V1
            #print(exec.time)
        }
		
		# save exec.time: write into file
		write(x=exec.time, file=paste(inputs$output.full.dir.name,"/",EXEC.TIME.FILENAME,sep=""))
	
		
		if(LOG.ENABLED)
			write.into.log(worker.id, 12, ".........END RUN.KMBS.........")
	}
	
	# in case of RUNNING.PARTITIONING.ALGOS.ENABLED == FALSE, read the exec.time from the file
	result$exec.time = as.numeric(readLines(con=paste(inputs$output.full.dir.name,"/",EXEC.TIME.FILENAME,sep="")))
	
	membership = load.KMBS.partition(inputs$algo.output.file)
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


