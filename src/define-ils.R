# TODO: Add comment
# 
# Author: nejat
###############################################################################


#################################################################################
## Retreive the node ids who contributes to the imbalance value estimated by ILS.
## 
## file.name: the path and name of the file to load.
##
## returns: the node ids who contributes to the imbalance value
#################################################################################
#
## An example of the content of the rcc output result file (cc result file is similar)
##RCC Global time spent: 0.0881309
##SRI(P) = 5.42857
##Clustering configuration: 
##Partition 0 (36): [ 1 3 5 10 12 13 19 20 22 23 24 26 27 28 29 30 32 35 37 38 42 43 44 46 49 51 55 56 57 60 64 65 68 69 71 72 ] 
##Partition 1 (18): [ 0 2 4 11 15 17 18 21 31 33 39 41 45 47 54 59 63 70 ] 
##Partition 2 (19): [ 6 7 8 9 14 16 25 34 36 40 48 50 52 53 58 61 62 66 67 ] 
#
##Imbalance analysis (out edges contribution):
##Vertex,PositiveSum,NegativeSum
##0,0,0.6
##1,0,0
##2,0,0.6
##.
##.
##Imbalance analysis (in edges contribution):
##Vertex,PositiveSum,NegativeSum
##0,0,0
##1,0,0
##2,0,0
##3,0,0
##.
##.
## so on
#
#retreive.pils.imbalance.analysis.result <- function(file.name, nb.partition, graph.size)
#{	# open and read the file
#	con <- file(file.name, "r")
#	lines <- readLines(con)
#	close(con)
#	
#	imbalance.analysis = list()
#	
#	# example of the first 3 lines in rcc-result.txt. It is similar to cc-result.txt
#	# Starting from 4th line, partitions are displayed. 
#	# If there are 2 partitions estimated, the 4th and 5th lines display partitions.
#	# After displaying partitions lines, there is 1 empty line, 
#	# then the lines dedicated to imbalanace analysis start from there.
#	i <- 3 + nb.partition + 1 # go to the (3 + nb.partition + 1)th line
#	
#	for(j in 1:2){ #2 times: for "out edges contribtion" and "in edges contribution"
#		vertex.id = c()
#		positive.sum = c()
#		negative.sum = c()
#		
#		offset = (j-1)*graph.size
#		
#		i = i + 1 # go to next line
#		line = lines[i+offset]
#		part2 = strsplit(x=line, "(", fixed=TRUE)[[1]][2]
#		desc = strsplit(x=part2, ")", fixed=TRUE)[[1]][1]
#		
#		i = i + 1 # go to next line
#		line = lines[i+offset]
#		col.names = strsplit(x=line, ",", fixed=TRUE)[[1]]
#		
#		for(k in 1:graph.size){
#			line = lines[i+offset+k]
#			aValues = strsplit(x=line, ",", fixed=TRUE)[[1]]
#			vertex.id[k] = as.numeric(aValues[1])
#			positive.sum[k] = as.numeric(aValues[2])
#			negative.sum[k] = as.numeric(aValues[3])
#		}
#		
#		total.imbalance = Reduce("+", positive.sum) + Reduce("+", negative.sum)
#		positive.sum = positive.sum/total.imbalance
#		negative.sum = negative.sum/total.imbalance
#		
#		df=data.frame(vertex.id, positive.sum, negative.sum)	
#		imbalance.analysis[[desc]] = df
#	}
#	
#	return(imbalance.analysis)
#	
#}





########################################################################
#
########################################################################
prepare.output.dir.basename = function(params, init.partition.from){
	
	# retreive parameters in this order:
	# 1) st 2) is.par 3) cc 4) rcc 5) l 6) alpha 7) i 8) p 9) t 10) gf 
	# 11) k 12) k.from 13) init.partition.from.infomap.enabled 
	# 14) ils.k.min.enabled
	st = params[1]
	is.par = params[2]
	cc = params[3]
	rcc = params[4]
	l = params[5]
	a = params[6]
	i = params[7]
	p = params[8]
	t = params[9]
	k = params[11]
	k.from = params[12]
	init.partition.from.im.enab = params[13]
	ils.k.min.enabled = params[14]
	
	prefix = ifelse(is.par == 1, PREFIX.PAR.VERS, PREFIX.SEQ.VERS)
	program.type = ifelse(st == CORCLU.ILS, tolower(CORCLU.ILS), tolower(CORCLU.GRASP))
	
	foldername = paste(
			prefix, "_", program.type, 
			"-cc=", cc,
			"-rcc=", rcc, 
			"-a=", a, 
			"-l=", l, 
			"-i=", i, 
			"-p=", p, 
			"-t=", t,  
			sep=""
	)
	
	
	
	k.desc = ifelse(ils.k.min.enabled == TRUE, "kmin", "k")
	
	if(k == 0){
		foldername = paste(
				foldername, 
				"-", k.desc, "=", k, 
				sep=""
		)
	} else if(k != 0){
		foldername = paste(
				foldername, 
				"-", k.desc, "(from=", k.from, ")=", k, 
				sep=""
		)
	}
	
	if(init.partition.from.im.enab == TRUE){
		foldername = paste(
				foldername,
				"-init-partition(from=", init.partition.from, ")-iter=1", 
				sep=""
		)
	}

	
	return(foldername)
}



########################################################################
#

# An example of the command for ILS:
# ./build/graspcc -l 1 --iter=10 --k=3 --alpha=1.0 
# --input-file-dir "/home/nejat/lia/EuropeanParliament/input/threshold=NA1" 
# --output-folder "../output/threshold=NA/ils/seq-a1.0-l1-i10-k(ilscc)3-1h"
# --gain-function-type=0 --time-limit=3600 --rcc=true --strategy ILS


# TODO: what to do for parallel version of ils?
########################################################################
prepare.grasp.ils.command = 
		function(input.graph.filename, output.full.folder.name,
				init.partition.file, params)
{
	# TODO infomap.output.file is needed in the input params for all ils command
	# ils-cc does not need it for example
	
	# retreive parameters in this order:
	# 1) st 2) is.par 3) cc 4) rcc 5) l 6) alpha 7) i 8) p 9) t 10) gf 
	# 11) k 12) k.from 13) init.partition.from.infomap.enabled 
	# 14) ils.k.min.enabled
	st = params[1]
	is.par = params[2]
	cc = params[3]
	rcc = params[4]
	l = params[5]
	a = params[6]
	i = params[7]
	p = params[8]
	t = params[9]
	gf = params[10]
	k = params[11]
	k.from = params[12]
	init.partition.from.im.enab = params[13]
	ils.k.min.enabled = params[14]
	
	program.type = ifelse(st == CORCLU.ILS, CORCLU.ILS, CORCLU.GRASP)
	
	cmd = paste(
			GRASP.ILS.EXECUTABLE.PATH, 
			" --cc=", cc, 
			" --rcc=", rcc,
			" --alpha=", a, 
			" -l ", l, " ", 
			" --iter=", i, 
			" --perturbationLevelMax=", p, 
			" --k=", k,
			#" --min-k=", ils.k.min.enabled,
			" --time-limit=", t,
			" --input-file ", "'", input.graph.filename, "'", 
			" --output-folder ", "'", output.full.folder.name, "'",
			" --strategy ", program.type, 
			sep=""
	)
	print(cmd)
	
	if(init.partition.from.im.enab == TRUE){
		cmd = paste(
				cmd, 
				" --init-partition-file ", "'", init.partition.file, "'",
				sep=""
		)
	}
	
	return(cmd)
}



########################################################################
#
########################################################################
prepare.python.result.interpreter.command = 
		function(output.full.folder.name, cc, rcc){  
	
	cmd = ""
	if(cc == 1 && rcc==0){
		cmd = paste(
				PYTHON.RESULT.INTERPRETER.EXECUTABLE.PATH, 
				" --folder=", "'", output.full.folder.name, "'",
				" --cc", 
				sep=""
		)
	} 
	else if(cc == 0 && rcc==1){
		cmd = paste(
				PYTHON.RESULT.INTERPRETER.EXECUTABLE.PATH,
				" --folder=", "'", output.full.folder.name, "'",
				" --rcc", 
				sep=""
		)
	}
#	else{ # else if(cc == 1 && rcc==1){ # I think we do no need to process cc and rcc at the same time
#		cmd = paste(
#				PYTHON.RESULT.INTERPRETER.EXECUTABLE.PATH,
#				" --folder=", "'", output.full.folder.name, "'",
#				" --cc",
#				" --rcc", 
#				sep=""
#		)
#	}
	
	return(cmd)
}



########################################################################
#
########################################################################
keep.only.necessary.ils.output.files = function(output.full.folder.name){
	# when the algorithm ils or grasp is run, it creates a directory whose the 
	# name is as the same as the name of the variable SIGNED.GRAPH.FILENAME
	# And then, it creates another directory whose the name is strange
	# (Probably the reason is to handle parallel execution or multiple execution). 
	# To get this strange folder name, use list.dirs()
	result.files.dir = 
			list.dirs(
					path = paste(output.full.folder.name, "/", SIGNED.GRAPH.FILENAME, sep=""),
					full.names = TRUE, 
					recursive = FALSE
			)
	
	# normally there is only 1 directory. Even if we run this script many times,
	# at the end, we remove unnecesary files. "result.files.dir" has unnecessary files
	result.files.dir = result.files.dir[1] 
	# move cc-result-file AND/OR rcc-result.file into upfolder location 
	# for simplicity of further operations
	# upfolder location means 1-up folder: "/nejat/home/ils" > becomes "/nejat/home"
	file.copy(
			paste(result.files.dir,"/",CC.RESULT.FILENAME,sep=""),
			paste(output.full.folder.name,"/",CC.RESULT.FILENAME,sep=""),
			overwrite=TRUE,
			recursive=FALSE,
			copy.mode=TRUE,
			copy.date=FALSE
	)
	file.copy(
			paste(result.files.dir,"/",RCC.RESULT.FILENAME,sep=""),
			paste(output.full.folder.name,"/",RCC.RESULT.FILENAME, sep=""),
			overwrite=TRUE,
			recursive=FALSE,
			copy.mode=TRUE,
			copy.date=FALSE
	)
	
	# delete unnecessary files
	unlink(
			paste(output.full.folder.name,"/",SIGNED.GRAPH.FILENAME,sep=""), 
			recursive=TRUE
	)
}





#########################################################################
##
#########################################################################
#get.membership.from.file = function(algo.name, algo.output.file){
#	mems = NA
#	if(algo.name == CORCLU.ALGO.PILS){
#		mems = load.external.partition( # membership file after having run the algo
#				algo.output.file, 
#				CORCLU.ALGO.PILS
#		)
#	} else if(algo.name == COMDET.INFOMAP){
#		mems = read.table(algo.output.file)$V1 # infomap: membership.txt file
#	} else if(algo.name == KMBS){
#		mems = extract.membership.kmbs(algo.output.file)
#	} else if(algo.name == ExCC){
#		mems = load.ExCC.partition(algo.output.file)
#	}
#	
#	return(mems)
#}






########################################################################
#
########################################################################
prepare.and.run.python.result.interpreter = function(output.full.folder.name, k){
	cmd = NA
	if(k == 0)
		cmd = prepare.python.result.interpreter.command(output.full.folder.name, 1, 0)
	else
		cmd = prepare.python.result.interpreter.command(output.full.folder.name, 0, 1)
	
	system(cmd, wait=TRUE)
}



################################################################################
# Loads the partition estimated by the pILS tool.
# 
# file.name: the path and name of the file to load.
#
# returns: the corresponding partition as a membership vector.
###############################################################################
load.ils.partition <- function(file.name)
{	# open and read the file
	#print(file.name)
	con <- file(file.name, "r")
	lines <- readLines(con)
	close(con)
	
	# process the file content
	i <- 4
	line <- lines[i]
	res <- list()
	while(line!="")
	{  # process current line
		#print(line)
		line <- strsplit(x=line, "[ ", fixed=TRUE)[[1]][2]
		line <- strsplit(x=line, " ]", fixed=TRUE)[[1]][1]
		
		# we increment by 1 at the end because C++ starts counting from 0
		nodes <- as.integer(strsplit(x=line," ", fixed=TRUE)[[1]]) + 1 
		
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
prepare.and.run.grasp.ils = 
		function(in.graph.file, out.full.folder.name, init.partition.file, params){
	
	cmd = 
			prepare.grasp.ils.command(
					in.graph.file, 
					out.full.folder.name, 
					init.partition.file, 
					params
			)
	
	system(cmd, wait=TRUE)
	

}



########################################################################
# if network.size > 300, alpha will be 0.8. Otherwise, alpha will be 0.4
########################################################################
prepare.ils.generic = 
		function(desc, output.directory, network.size,
				ils.result.filename,
				k, 
				k.from,
				init.partition.enabled,
				init.partition.from,
				ils.k.min.enabled=FALSE,
				nb.ils.iter=ITER.DEFAULT
){

	inputs = list()

	
	# get the relevant parameter set for running the ILS algo (k, alpha, etc.)
	pset = 
			prepare.grasp.ils.parameter.set(
					network.size, k, k.from, 
					init.partition.enabled,
					nb.ils.iter,
					ils.k.min.enabled
			)
	
	params = pset$pop()$value # #	if needed, add this:  if(!pset$empty()){}
	output.dir.basename = prepare.output.dir.basename(params, init.partition.from)
	output.full.dir.name = paste(output.directory, output.dir.basename, sep="/")
	algo.output.file = paste(output.full.dir.name,"/", ils.result.filename,sep="")
	
	inputs$output.dir.basename = output.dir.basename
	inputs$output.full.dir.name = output.full.dir.name
	inputs$algo.output.file = algo.output.file
	inputs$params = params

	
	return(inputs)
}




########################################################################
#
########################################################################
run.ils.generic = function(input.graph.filename, output.full.dir.name, algo.output.file,
		init.partition.file, params, k)
{
    exec.time = NA
    #print(output.full.dir.name)
    fpath = paste(output.full.dir.name,"/",EXEC.TIME.FILENAME,sep="")
    #print(fpath)
    if( (!dir.exists(output.full.dir.name) || !file.exists(fpath)) || FORCE){
        #print(output.full.dir.name)
    	start=Sys.time()
    	prepare.and.run.grasp.ils(
    			input.graph.filename, output.full.dir.name, 
    			init.partition.file, 
    			params
    	)
    	end=Sys.time()
    	exec.time = as.numeric(end) - as.numeric(start)
    	
    	prepare.and.run.python.result.interpreter(output.full.dir.name, k)
    	
    	keep.only.necessary.ils.output.files(output.full.dir.name)
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
perform.ils.generic = function(worker.id, k, k.from, k.min.enabled, init.partition.file, init.partition.from,
		result.filename, nb.ils.iter,
		desc, output.directory, output.dir.desc,
		target.name, network.path.G, network.path.graphml, network.size, current.MEP.details,
		LOG.ENABLED = TRUE, RUNNING.PARTITIONING.ALGOS.ENABLED = TRUE, UPDATING.GRAPHML.CONTENT.ENABLED = TRUE,
		PLOTTING.ENABLED = TRUE)
{
    
    
	if(LOG.ENABLED)
		write.into.log(worker.id, 12, ".........BEGIN PERFORM.ILSCC.GENERIC..........")
	
	
	result = list(exec.time = NA, stats.vec = NA, plot.inputs = NA)
	
	# =========================================================================
	if(LOG.ENABLED)
		write.into.log(worker.id, 12, ".........BEGIN PREPARE.ILSCC.GENERIC.........")
	
    	init.partition.enabled = ifelse(is.na(init.partition.file), FALSE, TRUE)
    	inputs = prepare.ils.generic(
			desc = desc, output.directory = output.directory, network.size = network.size,
			ils.result.filename = result.filename,
			k = k, 
			k.from = k.from,
			init.partition.enabled = init.partition.enabled,
			init.partition.from = init.partition.from,
			ils.k.min.enabled = k.min.enabled,
			nb.ils.iter = nb.ils.iter
	)
	
	if(LOG.ENABLED)
		write.into.log(worker.id, 12, ".........END PREPARE.ILSCC.GENERIC.........")
	# =========================================================================
	
	
	# =========================================================================
	if(RUNNING.PARTITIONING.ALGOS.ENABLED == TRUE){
		if(LOG.ENABLED)
			write.into.log(worker.id, 12, ".........BEGIN RUN.ILSCC.GENERIC.........")
		
		exec.time = run.ils.generic(
				input.graph.filename = network.path.G,
				output.full.dir.name = inputs$output.full.dir.name,
				algo.output.file = inputs$algo.output.file,
				init.partition.file = init.partition.file,
				params = inputs$params,
				k = k
		)
		
		# save exec.time: write into file
		write(x=exec.time, file=paste(inputs$output.full.dir.name,"/",EXEC.TIME.FILENAME,sep=""))
		
		if(LOG.ENABLED)
			write.into.log(worker.id, 12, ".........END RUN.ILSCC.GENERIC.........")
	}
	
	# in case of RUNNING.PARTITIONING.ALGOS.ENABLED == FALSE, read the exec.time from the file
	result$exec.time = as.numeric(readLines(con=paste(inputs$output.full.dir.name,"/",EXEC.TIME.FILENAME,sep="")))
	
	membership = load.ils.partition(inputs$algo.output.file)
	membership = post.proc.membership.for.isolated.nodes(network.path.graphml, membership)
		
	
	if(LOG.ENABLED){
		write.into.log(worker.id, 12, "result$exec.time", result$exec.time)
		write.into.log(worker.id, 12, "membership", membership)		
	}
	# =========================================================================
	
	
	# ===================================================================
	if(UPDATING.GRAPHML.CONTENT.ENABLED == TRUE){
		# update graphml file with ils partition info
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

	
	# ===================================================================
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
		#  )
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
				current.MEP.details,
				output.full.path = output.full.path
		)

		if(LOG.ENABLED)
			write.into.log(worker.id, 12, ".........END PLOTTING.........")
	}
	# ===================================================================
	
	
	if(LOG.ENABLED)
		write.into.log(worker.id, 12, ".........END PERFORM.ILSCC.GENERIC..........")
	
	return(result)
}



########################################################################
# 
########################################################################
perform.ils.cc = function(worker.id, k.min.enabled=FALSE, init.partition.file=NA, init.partition.from=NA,
		nb.ils.iter = ITER.DEFAULT, result.filename = CC.RESULT.FILENAME,
		desc, output.directory, output.dir.desc,
		target.name, network.path.G, network.path.graphml, network.size, current.MEP.details,
		LOG.ENABLED = TRUE, RUNNING.PARTITIONING.ALGOS.ENABLED = TRUE, UPDATING.GRAPHML.CONTENT.ENABLED = TRUE,
		PLOTTING.ENABLED = TRUE)
{	
	
	if(LOG.ENABLED)
		write.into.log(worker.id, 8, ".........BEGIN PERFORM.ILS.CC..........")
	
	# ILS generates new partition ITER.DEFAULT times in each ils iteration
	# if we provide ILS with a partition, it should be fair and ils iteration should be only 1
	if(!is.na(init.partition.file)) nb.ils.iter = 1
	
	
	
	
	result = perform.ils.generic(worker.id = worker.id, k = NO.PREDEF.NB.CLUSTER, k.from = NA, k.min.enabled = k.min.enabled,
			init.partition.file = init.partition.file, init.partition.from = init.partition.from,
			result.filename = result.filename, nb.ils.iter = nb.ils.iter,
			desc = desc, output.directory = output.directory,
			output.dir.desc = output.dir.desc,
			target.name = target.name, network.path.G = network.path.G,
			network.path.graphml = network.path.graphml, network.size = network.size,
			current.MEP.details = current.MEP.details,
			LOG.ENABLED = LOG.ENABLED, RUNNING.PARTITIONING.ALGOS.ENABLED = RUNNING.PARTITIONING.ALGOS.ENABLED,
			UPDATING.GRAPHML.CONTENT.ENABLED = UPDATING.GRAPHML.CONTENT.ENABLED,
			PLOTTING.ENABLED = PLOTTING.ENABLED)
	
	if(LOG.ENABLED)
		write.into.log(worker.id, 8, ".........END PERFORM.ILS.CC..........")
	
	return(result)
}




########################################################################
# 
########################################################################
perform.ils.rcc = function(worker.id, k, k.from, init.partition.file = NA, init.partition.from = NA,
		nb.ils.iter = ITER.DEFAULT, result.filename = RCC.RESULT.FILENAME,
		desc, output.directory,
		output.dir.desc, target.name,
		network.path.G, network.path.graphml, network.size, current.MEP.details,
		LOG.ENABLED = TRUE, RUNNING.PARTITIONING.ALGOS.ENABLED = TRUE, UPDATING.GRAPHML.CONTENT.ENABLED = TRUE,
		PLOTTING.ENABLED = TRUE)
{	
	if(LOG.ENABLED)
		write.into.log(worker.id, 8, ".........BEGIN PERFORM.ILS.RCC..........")
	
	# ILS generates new partition ITER.DEFAULT times in each ils iteration
	# if we provide ILS with a partition, it should be fair and ils iteration should be only 1
	if(!is.na(init.partition.file)) nb.ils.iter = 1
	
	result = perform.ils.generic(worker.id = worker.id, k = k, k.from = k.from, k.min.enabled = FALSE,
			init.partition.file = init.partition.file, init.partition.from = init.partition.from,
			result.filename = result.filename, nb.ils.iter = nb.ils.iter,
			desc = desc, output.directory = output.directory,
			output.dir.desc = output.dir.desc,
			target.name = target.name, network.path.G = network.path.G,
			network.path.graphml = network.path.graphml, network.size = network.size,
			current.MEP.details = current.MEP.details,
			LOG.ENABLED = LOG.ENABLED, RUNNING.PARTITIONING.ALGOS.ENABLED = RUNNING.PARTITIONING.ALGOS.ENABLED,
			UPDATING.GRAPHML.CONTENT.ENABLED = UPDATING.GRAPHML.CONTENT.ENABLED,
			PLOTTING.ENABLED = PLOTTING.ENABLED)
	
	if(LOG.ENABLED)
		write.into.log(worker.id, 8, ".........END PLOTTING..........")
	
	return(result)
}