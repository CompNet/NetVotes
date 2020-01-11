



################################################################################
# Loads the partition estimated by the CC.METHOD.EXACT tool.
# 
# file.name: the path and name of the file to load.
#
# returns: the corresponding partition as a membership vector.
###############################################################################
load.ExCC.partition <- function(file.name)
{	# open and read the file
#	print(file.name)
	con <- file(file.name, "r")
	lines <- readLines(con)
	close(con)
	
	
	# process the file content
	i <- 1
	line <- lines[i]
	res <- list()
	
	# TODO: change here if the result file has more information than just the partition
	# in that case, put this line: while(line!="")
	while(!is.na(line)) # line!=""
	{  # process current line
		#print(line)
		line <- strsplit(x=line, "[", fixed=TRUE)[[1]][2]
		line <- strsplit(x=line, "]", fixed=TRUE)[[1]][1]
		
		# we increment by 1 at the end because C++ starts counting from 0
		nodes <- as.integer(strsplit(x=line,", ", fixed=TRUE)[[1]]) + 1
		
		res[[length(res)+1]] <- nodes
		
		# process next line
		i <- i + 1
		line <- lines[i]  
	}
	
	
	# build the membership vector
	mx <- max(unlist(res))
	membership <- rep(NA,mx)
	for(i in 1:length(res))
	{  nodes <- res[[i]]
		membership[nodes] <- i 
	}
	
#	# record the partition using the internal format
#	write.table(x=membership, file=partition.file, row.names=FALSE, col.names=FALSE)
	
	return(membership)
}


########################################################################
# 
########################################################################
create.output.ExCC.result.dir = function(output.directory){
	sub.dirname = ExCC
	dir.name = paste(output.directory, sub.dirname, sep="/")
	
	dir.create(dir.name, showWarnings = FALSE)
	
	return(sub.dirname)
}


########################################################################
# 
########################################################################
prepare.ExCC.full.output.filename = function(ExCC.directory){
	
	return(paste(ExCC.directory, EXCC.RESULT.FILENAME, sep="/"))
}



########################################################################
# 
########################################################################
prepare.ExCC = function(output.directory)
{
	inputs = list()

	output.dir.basename = create.output.ExCC.result.dir(output.directory)
	output.full.dir.name = paste(output.directory, output.dir.basename, sep="/")
	algo.output.file = 
			prepare.ExCC.full.output.filename(output.full.dir.name)
	
	inputs$output.dir.basename = output.dir.basename
	inputs$output.full.dir.name = output.full.dir.name
	inputs$algo.output.file = algo.output.file

	return(inputs)
}




#############################################################################################
# 
#############################################################################################
get.ExCC.command <- function(network.path, output.full.dir.name)
{
    is.cp = "true" # in any case, use cutting plane approach
    is.enum.all = "false"
    tilim = 3600 # 1 hour
    
    # An example:
    # java -Djava.library.path=/users/narinik/Cplex/ILOG/CPLEX_Studio128/cplex/bin/x86-64_linux/
    # -DinFile=data/test.G -DoutDir=. -Dcp=false -DenumAll=false -Dtilim=-1 -DlazyInBB=false
    # -DusercutInBB=false -DMaxTimeForRelaxationImprovement=-1 -jar exe/cplex-partition.jar
    
    cmd = 
        paste(
            "java",		
            paste0("-Djava.library.path='", CPLEX.BIN.PATH,"'"),
            paste0("-DinFile='", network.path,"'"),
            paste0("-DoutDir='", output.full.dir.name,"'"),
            paste0("-Dcp=",is.cp),
            paste0("-DenumAll=",is.enum.all),
            paste0("-Dtilim=",tilim),
            paste0("-DMaxTimeForRelaxationImprovement=","-1"), # no specific time limit, use the default one
            "-DlazyInBB=false",
            "-DuserCutInBB=false",
            "-jar",
            ExCC.JAR.PATH,
            sep=" "
        )
    
    print(cmd)
    
    return(cmd)
}



########################################################################
# 
########################################################################
run.ExCC = function(network.path, output.full.dir.name){
	
    
    fpath = paste(output.full.dir.name,"/",EXEC.TIME.FILENAME,sep="")
    #print(fpath)
    if( (!dir.exists(output.full.dir.name) || !file.exists(fpath)) || FORCE){
        
    	start=Sys.time()
    	cmd = get.ExCC.command(network.path, output.full.dir.name)
    	
    	system(cmd, wait=TRUE)
    	end=Sys.time()
    	exec.time = as.numeric(end) - as.numeric(start)
    	#print(cmd)
	
    } else {
        exec.time = read.table(file=fpath)$V1
        #print(exec.time)
    }

	return(exec.time)
}


########################################################################
########################################################################
########################################################################




########################################################################
# 
########################################################################
perform.ExCC = function(worker.id, desc, output.directory, output.dir.desc,
		target.name, network.path.G, network.path.graphml, current.MEP.details,
		LOG.ENABLED = TRUE, RUNNING.PARTITIONING.ALGOS.ENABLED = TRUE, UPDATING.GRAPHML.CONTENT.ENABLED = TRUE,
		PLOTTING.ENABLED = TRUE)
{
	result = list(exec.time = NA, stats.vec = NA, plot.inputs = NA)
	
	# ==========================================================================
	if(LOG.ENABLED)
		write.into.log(worker.id, 12, "BEGIN PREPARE.EXCC")

	inputs = prepare.ExCC(
			output.directory = output.directory
	)
	
	if(LOG.ENABLED)
		write.into.log(worker.id, 12, "END PREPARE.EXCC")
	# ==========================================================================
	
	
	# ==========================================================================
	if(RUNNING.PARTITIONING.ALGOS.ENABLED == TRUE){
		if(LOG.ENABLED)
			write.into.log(worker.id, 12, "BEGIN RUN.EXCC")
		
		exec.time = run.ExCC(
				network.path = network.path.G,
				output.full.dir.name = inputs$output.full.dir.name
		)
		
		# save exec.time: write into file
		write(x=exec.time, file=paste(inputs$output.full.dir.name,"/",EXEC.TIME.FILENAME,sep=""))

		if(LOG.ENABLED)
			write.into.log(worker.id, 12, "END RUN.EXCC")
	}
	
	# in case of RUNNING.PARTITIONING.ALGOS.ENABLED == FALSE, read the exec.time from the file
	result$exec.time = as.numeric(readLines(con=paste(inputs$output.full.dir.name,"/",EXEC.TIME.FILENAME,sep="")))
	
	membership = load.ExCC.partition(inputs$algo.output.file)
	membership = post.proc.membership.for.isolated.nodes(network.path.graphml, membership)
	
	if(LOG.ENABLED){
		write.into.log(worker.id, 12, "result$exec.time", result$exec.time)
		write.into.log(worker.id, 12, "membership", membership)		
	}
	# ==========================================================================
	
	
	# ===================================================================
	if(UPDATING.GRAPHML.CONTENT.ENABLED == TRUE){
		# update graphml file with kmbs partition info
		attr.name = desc
		newGraphDoc = addPartitionInfoIntoNodes(
				network.path.graphml, attr.name, membership
		)
		saveXML(newGraphDoc, file=network.path.graphml)	
	}
	# ===================================================================


	# ===================================================================
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
	# ===================================================================
	
	

	if(PLOTTING.ENABLED == TRUE){
		if(LOG.ENABLED)
			write.into.log(worker.id, 12, "BEGIN PLOTTING")
		
	    
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
			write.into.log(worker.id, 12, "END PLOTTING")
	}
	
	return(result)
}



