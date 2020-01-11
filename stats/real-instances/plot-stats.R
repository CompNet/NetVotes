# TODO: Add comment
# 
# Author: nejat
###############################################################################

source("stats/define-consts.R")
library(package="igraph")


if(dir.exists(DIR.REAL.INSTANCES.CSV) && length(list.files(DIR.REAL.INSTANCES.CSV))!=0 ){
    
    dir.create(DIR.REAL.INSTANCES.PLOTS, showWarnings = TRUE)
    
    all.output.path = paste(DIR.REAL.INSTANCES.CSV,"All",sep="/")
    if(dir.exists(all.output.path))
        TARGET.TYPES = c(TARGET.TYPES, "All")
    
    #for(TARGET.TYPE in c("State", "Group", "All")){
    for(TARGET.TYPE in TARGET.TYPES){
    	
    	dir.create(paste(DIR.REAL.INSTANCES.PLOTS,TARGET.TYPE,sep="/"), showWarnings = TRUE)
    	
    	# ============================================================================
    	# filtering-step-assessment
    	# ============================================================================
        print("filtering-step-assessment")
    	plot.folder = paste(DIR.REAL.INSTANCES.PLOTS,TARGET.TYPE,"filtering-step-assessment",sep="/")
    	dir.create(plot.folder, showWarnings = TRUE)
    	source(paste(DIR.REAL.INSTANCES,"/filtering-step-assessment.R",sep=""))
    	
    	# ============================================================================
    	# after-filtering-step-IM-vs-ExCC
    	# ============================================================================
    	print("after-filtering-step-IM-vs-ExCC")
    	plot.folder = paste(DIR.REAL.INSTANCES.PLOTS,"/",TARGET.TYPE,"/after-filtering-step-",COMDET.INFOMAP,"-vs-",ExCC,sep="")
    	dir.create(plot.folder, showWarnings = TRUE)
    	source(paste(DIR.REAL.INSTANCES,"/after-filtering-step-",COMDET.INFOMAP,"-vs-",ExCC,".R",sep=""))
    	
    	# ============================================================================
    	# after-filtering-step-ILSCC-vs-ExCC
    	# ============================================================================
    	print("after-filtering-step-ILSCC-vs-ExCC")
    	plot.folder = paste(DIR.REAL.INSTANCES.PLOTS,"/",TARGET.TYPE,"/after-filtering-step-",ILSCC,"-vs-",ExCC,sep="")
    	dir.create(plot.folder, showWarnings = TRUE)
    	source(paste(DIR.REAL.INSTANCES,"/after-filtering-step-",ILSCC,"-vs-",ExCC,".R",sep=""))
    	
    	# ============================================================================
    	# infomap-robustness-check
    	# ============================================================================
    	print("infomap-robustness-check")
    	plot.folder = paste(DIR.REAL.INSTANCES.PLOTS,"/",TARGET.TYPE,"/",COMDET.INFOMAP,"-robustness-check",sep="")
    	dir.create(plot.folder, showWarnings = TRUE)
    	source(paste(DIR.REAL.INSTANCES,"/",COMDET.INFOMAP,"-robustness-check.R",sep=""))
    	
    	
    	# ============================================================================
    	# graph strucure: connected-components
    	# ============================================================================
    	print("graph strucure: connected-components")
    	plot.folder = paste(DIR.REAL.INSTANCES.PLOTS,TARGET.TYPE,"graph-connected-components",sep="/") 
    	dir.create(plot.folder, showWarnings = TRUE)
    	source(paste(DIR.REAL.INSTANCES,"/graph-connected-components.R",sep=""))
    	
    	# ============================================================================
    	# graph strucure: graph-density
    	# ============================================================================
    	print("graph strucure: graph-density")
    	plot.folder = paste(DIR.REAL.INSTANCES.PLOTS,TARGET.TYPE,"graph-density",sep="/") 
    	dir.create(plot.folder, showWarnings = TRUE)
    	source(paste(DIR.REAL.INSTANCES,"/graph-density.R",sep=""))
    	
    	# ============================================================================
    	# graph strucure: pos-neg-edges
    	# ============================================================================
    	print("graph strucure: pos-neg-edges")
    	plot.folder = paste(DIR.REAL.INSTANCES.PLOTS,TARGET.TYPE,"graph-pos-neg-edges",sep="/") 
    	dir.create(plot.folder, showWarnings = TRUE)
    	source(paste(DIR.REAL.INSTANCES,"/graph-pos-neg-edges.R",sep=""))
    
    }


}