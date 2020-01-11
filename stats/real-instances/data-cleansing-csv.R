# TODO: Add comment
# 
# Author: nejat
###############################################################################

source("stats/define-consts.R")
source("stats/clean-data.R")

# ========== SUMMARY ============================================================
# Bu ornekte 3 tane data.frame kullandim.
#
# INPUT DATA  --------------------
#> df1
#target domain period partitions
#1     FR   AGRI  09-10  1 1 1 2 3
#2     FR   AGRI  10-11  1 2 1 2 1
#3     FR   AGRI  11-12  2 1 2 1 1
#4     FR   AGRI  12-13    1 1 1 1
#5     FR   ECON  13-14  1 2 2 2 2
#> df2
#target domain period partitions
#1     FR   AGRI  09-10  1 1 1 2 3
#2     FR   AGRI  10-11  1 2 1 2 1
#3     FR   AGRI  11-12  2 1 2 1 1
#4     FR   AGRI  12-13  1 1 1 1 2
#5     FR   AGRI  13-14      1 2 2
#> df3
#target domain period partitions
#1     FR   AGRI  09-10  1 1 1 2 3
#2     FR   AGRI  10-11  1 2 1 2 1
#3     FR   AGRI  11-12  2 1 2 1 1
#4     FR   AGRI  12-13  1 1 1 1 2
#
#
# RESULT: ------------------------
#[[1]]
#target domain period partitions
#1     FR   AGRI  09-10  1 1 1 2 3
#2     FR   AGRI  10-11  1 2 1 2 1
#3     FR   AGRI  11-12  2 1 2 1 1
#
#[[2]]
#target domain period partitions
#1     FR   AGRI  09-10  1 1 1 2 3
#2     FR   AGRI  10-11  1 2 1 2 1
#3     FR   AGRI  11-12  2 1 2 1 1
#
#[[3]]
#target domain period partitions
#1     FR   AGRI  09-10  1 1 1 2 3
#2     FR   AGRI  10-11  1 2 1 2 1
#3     FR   AGRI  11-12  2 1 2 1 1
#
# Burdaki problem cozme asamalari su:
# 1) Filtering ve original ayri ayri yapilcak: nrow(df2) != nrow(df1) olabilir. 
# Dolayisyla, nrow(df2) daha az sayida old. icin df1'den fazlaliklari cikarmaliyiz. 3. data frame varsa onu da update et.
# 2) Filtering ve original ayri ayri yapilcak: 1. etaptan sonra nrow(df2) == nrow(df1) olsa bile partition'lar arasinda bi farklilik olusabilir.
# Mesela df1.Adjusted[2,"partition"] != df2[2,"partition"] olabilir. 
# Dolayisyla, farkli olanlari hem df1.adjusted hem df2'den cikarticaz. 3. data frame varsa onu da update et.
# 3) 1 tane original csv 1 tane de filtered csv al. Bunlara 1. etabi uygula. Diger geriye kalan butun filtered and original df'lari update et.
#
# ========== SUMMARY ============================================================



# === BEGIN =====================================================================

####################
#
# Note that length(df.list) should be at least 2.
#
####################
retreive.adjusted.obs = function(df.list){
	
	# choose the 1st method as the best for comparison => the result of 1st method is the best result by now
	# start by 2nd method, apply remove.unmatched.obs(2nd methiod, best method) and compare the result with the best result
	# and continue with 3rd method AND SO ON
	df.final.adjusted = df.list[[1]]
	for(i in seq(2, length(df.list))){
		df.m = df.list[[i]]	
		res = remove.unmatched.obs(df.m, df.final.adjusted)
		df.m.new = res$df1 # note that res$df1 and res$df2 contain the same observations
		
		if(nrow(df.m.new) < nrow(df.final.adjusted)) df.final.adjusted = df.m.new
	}
	
	return(df.final.adjusted)
}

####################
#
# Note that length(df.list) should be at least 2.
#
####################
retreive.adjusted.obs.based.on.partitions = function(df.list){
	
	# choose the 1st method as the best for comparison => the result of 1st method is the best result by now
	# start by 2nd method, apply remove.unmatched.obs(2nd methiod, best method) and compare the result with the best result
	# and continue with 3rd method AND SO ON
	df.final.adjusted = df.list[[1]]
	partitions.final = lapply(df.final.adjusted[, "sPartition"], function(x) unlist( strsplit(as.character(x), " ") ))
	for(i in seq(2, length(df.list))){
		df.m = df.list[[i]]
		partitions.m = lapply(df.m[, "sPartition"], function(x) unlist( strsplit(as.character(x), " ") ))
		
		indx.to.be.removed = identify.erroneous.obs(partitions.m, partitions.final)
		df.m.new = remove.erreonous.obs(df.m, indx.to.be.removed)    
		
		if(nrow(df.m.new) < nrow(df.final.adjusted)) df.final.adjusted = df.m.new
	}
	
	return(df.final.adjusted)
}

####################
#
#
####################
remove.and.adjust.obs = function(df.list, df.reference){
	
	# As we found the best result, apply it for all method results in order to have the same nb obs
	final.df.list = list()
	for(i in seq(1, length(df.list))){
		df.m = df.list[[i]]
		
		res = remove.unmatched.obs(df.m, df.reference)
		# note that "res$df1" is the adjusted version of "df.m" and "res$df2" the adjusted version of "df.reference"
		# but, as "df.reference" is the optimum/reference data.frame among others, the adjusted version is the same as "df.reference"
		final.df.list[[i]] = res$df1
	}
	
	return(final.df.list)
}
# === END =======================================================================


csv.file.descs = c(ExCC, ILSCC, COMDET.INFOMAP, G.STR)
csv.methods = csv.file.descs[1:3]

for(TARGET.TYPE in TARGET.TYPES){

	csv.path = paste(DIR.REAL.INSTANCES.CSV,TARGET.TYPE,sep="/")

	    
	# -------------------------------------------
	# -- comparison between filtered csv files --
	# -------------------------------------------
	print("comparison between filtered csv files")
	
	filtering.type = "filtered"
	df.list.filt = lapply(csv.methods, function(method){
				filename = paste(filtering.type,"-",method,"-info.csv",sep="")
				print(paste(csv.path,filename,sep="/"))
				if(file.exists(paste(csv.path,filename,sep="/")))
				    return(read.csv(paste(csv.path,filename,sep="/")))
				else
				    return(NA)
			})
	
	
	if(!all(is.na(df.list.filt))){ # if the folder 'Group' contains some files to process
    	# based on target/domain/period differences
    	df.final.adjusted1 = retreive.adjusted.obs(df.list.filt)
    	final.df.list.filt = remove.and.adjust.obs(df.list.filt, df.reference=df.final.adjusted1)
    	
    	# based on partitions differences
    	df.final.adjusted2 = retreive.adjusted.obs.based.on.partitions(final.df.list.filt)
    	new.final.df.list.filt = remove.and.adjust.obs(final.df.list.filt, df.reference=df.final.adjusted2)
	}

    
	# -------------------------------------------
	# -- comparison between original csv files --
	# -------------------------------------------
	print("comparison between original csv files")
	
	filtering.type = "original"
	df.list.orig = lapply(csv.methods, function(method){
				filename = paste(filtering.type,"-",method,"-info.csv",sep="")
				print(paste(csv.path,filename,sep="/"))
				if(file.exists(paste(csv.path,filename,sep="/")))
				    return(read.csv(paste(csv.path,filename,sep="/")))
				else
				    return(NA)
			})

	
	if(!all(is.na(df.list.orig))){ # if the folder 'Group' contains some files to process
    	# based on target/domain/period differences
    	df.final.adjusted3 = retreive.adjusted.obs(df.list.orig)
    	final.df.list.orig = remove.and.adjust.obs(df.list.orig, df.reference=df.final.adjusted3)
    	
    	# based on partitions differences
    	df.final.adjusted4 = retreive.adjusted.obs.based.on.partitions(final.df.list.orig)
    	new.final.df.list.orig = remove.and.adjust.obs(final.df.list.orig, df.reference=df.final.adjusted4)
    }
	
	
	
	
	
	if( !all(is.na(df.list.filt)) && !all(is.na(df.list.orig)) ){
	
    	# ---------------------------------------------------------------------------------------------------
    	# -- comparison betw 1 filt csv and 1 orig csv AND find the common observations betw orig and filt --
    	# ---------------------------------------------------------------------------------------------------
    	print("comparison betw 1 filt csv and 1 orig csv")
    
    	df.list = list(df.final.adjusted2, df.final.adjusted4)
    	# based on target/domain/period differences
    	df.final.adjusted5 = retreive.adjusted.obs(df.list)
    	res.df.list.filt = remove.and.adjust.obs(new.final.df.list.filt, df.reference=df.final.adjusted5)
    	res.df.list.orig = remove.and.adjust.obs(new.final.df.list.orig, df.reference=df.final.adjusted5)
    	
    	
    	
    	# --------------------------------------
    	# -- update graph-structure csv files --
    	# --------------------------------------
    	# Bunu diger metodlarla birlikte process edemem. Cunku bu data.frame'in "sPartition" diye kolonu yok
    	# O yuzden en son bunu da update ediyorum
    	print("update graph-structure csv files")
    	
    	filtering.type = "filtered"
    	df.filt.graph.str = read.csv(paste(csv.path,"/",filtering.type,"-",G.STR,"-info.csv",sep=""))
    	df.list = list(df.filt.graph.str) # we should provide a list, that is why we create a list with 1 item
    	final.df.list = remove.and.adjust.obs(df.list, df.reference=df.final.adjusted5)
    	final.df.filt.graph.str = final.df.list[[1]]
    	
    	filtering.type = "original"
    	df.orig.graph.str = read.csv(paste(csv.path,"/",filtering.type,"-",G.STR,"-info.csv",sep=""))
    	df.list = list(df.orig.graph.str) # we should provide a list, that is why we create a list with 1 item
    	final.df.list = remove.and.adjust.obs(df.list, df.reference=df.final.adjusted5)
    	final.df.orig.graph.str = final.df.list[[1]]
    	
    	
    	# ----------------------
    	# -- write into files --
    	# ----------------------
    	print("write into files")
    
    	res.df.list.filt[[4]] = final.df.filt.graph.str
    	res.df.list.orig[[4]] = final.df.orig.graph.str
    	
    	
    	filtering.type = "filtered"
    	for(i in seq(1,length(csv.file.descs))){
    		filename = paste(filtering.type,"-",csv.file.descs[i],"-info-after-data-cleansing.csv",sep="")
    		write.csv(file=paste(csv.path,"/",filename,sep=""), x=res.df.list.filt[i])
    	}
    				
    	filtering.type = "original"
    	for(i in seq(1,length(csv.file.descs))){
    		filename = paste(filtering.type,"-",csv.file.descs[i],"-info-after-data-cleansing.csv",sep="")
    		write.csv(file=paste(csv.path,"/",filename,sep=""), x=res.df.list.orig[i])
    	}
	
    	print("end with success")
    	
	} else {
	    print("end with faillure")
	}
	
	
}