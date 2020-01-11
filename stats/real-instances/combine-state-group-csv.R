# TODO: Add comment
# 
# Author: nejat
###############################################################################

source("stats/define-consts.R")

if(all(c("State","Group") %in% TARGET.TYPES)){ # if both "State" and "Group" are considered in 'stats/main.R'
    output.path = paste(DIR.REAL.INSTANCES.CSV,"All",sep="/") # TODO
    dir.create(output.path, showWarnings = TRUE)
    
    
    print("aa")
    ################################################################################
    # combine "State" and "Group" and write into a file BEFORE DATA CLEANSING
    ################################################################################
    
    csv.file.descs = c(ExCC, ILSCC, COMDET.INFOMAP, G.STR)
    for(csv.file.desc in csv.file.descs){
    	for(filtering.type in c("filtered", "original")){
    	    df.state = NA
    		csv.path.state = paste(DIR.REAL.INSTANCES.CSV,"State",sep="/")
    		csv.filename = paste(csv.path.state,"/",filtering.type,"-",csv.file.desc, "-info.csv", sep="")
    		if(file.exists(csv.filename))
    		    df.state = read.csv(csv.filename)
    	
    		df.group = NA
    		csv.path.group = paste(DIR.REAL.INSTANCES.CSV,"Group",sep="/")
    		csv.filename = paste(csv.path.group,"/",filtering.type,"-",csv.file.desc, "-info.csv", sep="")
    		if(file.exists(csv.filename))
    		    df.group = read.csv(csv.filename)
    		
    		if(!is.na(df.state) && !is.na(df.group)){
        		df.all = rbind(df.state, df.group)
        		write.csv(file=paste(output.path,"/",filtering.type,"-",csv.file.desc, "-info.csv",sep=""), x=df.all)
    		}
    	}
    }
    
    print("bb")
    ################################################################################
    # combine "State" and "Group" and write into a file AFTER DATA CLEANSING
    ################################################################################
    
    csv.file.descs = c(ExCC, ILSCC, COMDET.INFOMAP, G.STR)
    for(csv.file.desc in csv.file.descs){
    	for(filtering.type in c("filtered", "original")){
    	    df.state = NA
    		csv.path.state = paste(DIR.REAL.INSTANCES.CSV,"State",sep="/")
    		csv.filename = paste(csv.path.state,"/",filtering.type,"-",csv.file.desc, "-info-after-data-cleansing.csv", sep="")
    		if(file.exists(csv.filename))
    		    df.state = read.csv(csv.filename)
    		
    		df.group = NA
    		csv.path.group = paste(DIR.REAL.INSTANCES.CSV,"Group",sep="/")
    		csv.filename = paste(csv.path.group,"/",filtering.type,"-",csv.file.desc, "-info-after-data-cleansing.csv", sep="")
    		if(file.exists(csv.filename))
    		    df.group = read.csv(csv.filename)
    		
    		if(!is.na(df.state) && !is.na(df.group)){
    		    print("girdi")
        		df.all = rbind(df.state, df.group)
        		write.csv(file=paste(output.path,"/",filtering.type,"-",csv.file.desc, "-info-after-data-cleansing.csv",sep=""), x=df.all)
    		}
    	}
    }
    
    
    
    # ================================================================
    if(length(list.files(output.path)) == 0)
        unlink(output.path,recursive=TRUE)
    
}