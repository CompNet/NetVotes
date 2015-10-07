#############################################################################################
# This script allows defining all the file constants used by the other scripts and functions.
# This include both file names and folder names.
# 
# 07/2015 Israel Mendonça (v1)
# 09/2015 Vincent Labatut (v2)
#############################################################################################


######## Folders
# general input folder
IN.FOLDER <- "in"
# folder containing configuration files
CFG.FOLDER <- "config"
# general ouput folder
OUT.FOLDER <- "out"
	# output folder for everything not network-related
	OVERALL.FOLDER <- file.path(OUT.FOLDER,"_overall")
	# pils folder (tempary)
	PILS.FOLDER <- file.path(OUT.FOLDER,"pils")
	
		
######## Files
# TODO


######## Vote values
VOTE.FOR <- "For"
VOTE.ABST <- "Abstention"
VOTE.AGST <- "Against"
VOTE.NONE <- "NoVote"
VOTE.ABSENT <- "Absent"
VOTE.DOCABSENT <- "DocAbsent"


######## Normalized policty domains names
DOMAIN.FULLNAMES <- c()
# Constitutional Affairs
DOM.AFCO <- "AFCO"
DOMAIN.FULLNAMES[DOM.AFCO] <- "Constitutional Affairs"
# Foreign Affairs
DOM.AFET <- "AFET"
DOMAIN.FULLNAMES[DOM.AFET] <- "Foreign Affairs"
	# Human Rights (seems included in AFET)
	#DOM.DROI <- "DROI"
	#DOMAIN.FULLNAMES[DOM.DROI] <- "Human Rights"
	# Security and Defence (seems included in AFET)
	#DOM.SEDE <- "SEDE"
	#DOMAIN.FULLNAMES[DOM.SEDE] <- "Security and Defence"
# Agriculture and Rural Development
DOM.AGRI <- "AGRI"
DOMAIN.FULLNAMES[DOM.AGRI] <- "Agriculture and Rural Development"
# Budgets
DOM.BUDG <- "BUDG"
DOMAIN.FULLNAMES[DOM.BUDG] <- "Budgets"
# Budgetary Control
DOM.CONT <- "CONT"
DOMAIN.FULLNAMES[DOM.CONT] <- "Budgetary Control"
# Culture and Education
DOM.CULT <- "CULT"
DOMAIN.FULLNAMES[DOM.CULT] <- "Culture and Education"
# Development
DOM.DEVE <- "DEVE"
DOMAIN.FULLNAMES[DOM.DEVE] <- "Development"
# Economic and Monetary Affairs
DOM.ECON <- "ECON"
DOMAIN.FULLNAMES[DOM.ECON] <- "Economic and Monetary Affairs"
# Employment and Social Affairs
DOM.EMPL <- "EMPL"
DOMAIN.FULLNAMES[DOM.EMPL] <- "Employment and Social Affairs"
# Environment, Public Health and Food Safety
DOM.ENVI <- "ENVI"
DOMAIN.FULLNAMES[DOM.ENVI] <- "Environment, Public Health and Food Safety"
# Women's Rights and Gender Equality
DOM.FEMM <- "FEMM"
DOMAIN.FULLNAMES[DOM.FEMM] <- "Women's Rights and Gender Equality"
# Internal Market and Consumer Protection
DOM.IMCO <- "IMCO"
DOMAIN.FULLNAMES[DOM.IMCO] <- "Internal Market and Consumer Protection"
# International Trade
DOM.INTA <- "INTA"
DOMAIN.FULLNAMES[DOM.INTA] <- "International Trade"
# Industry, Research and Energy
DOM.ITRE <- "ITRE"
DOMAIN.FULLNAMES[DOM.ITRE] <- "Industry, Research and Energy"
# Legal Affairs
DOM.JURI <- "JURI"
DOMAIN.FULLNAMES[DOM.JURI] <- "Legal Affairs"
# Civil Liberties, Justice and Home Affairs
DOM.LIBE <- "LIBE"
DOMAIN.FULLNAMES[DOM.LIBE] <- "Civil Liberties, Justice and Home Affairs"
# Fisheries
DOM.PECH <- "PECH"
DOMAIN.FULLNAMES[DOM.PECH] <- "Fisheries"
# Petitions
DOM.PETI <- "PETI"
DOMAIN.FULLNAMES[DOM.PETI] <- "Petitions"
# Regional Development
DOM.REGI <- "REGI"
DOMAIN.FULLNAMES[DOM.REGI] <- "Regional Development"
# Internal regulations of the EP
DOM.RIPE <- "RIPE"
DOMAIN.FULLNAMES[DOM.RIPE] <- "Internal regulations of the EP"
# Transport and Tourism
DOM.TRAN <- "TRAN"
DOMAIN.FULLNAMES[DOM.TRAN] <- "Transport and Tourism"




#
#
#
## Name of the directory where all the plots will be stored
#output.plots.dir <- file.path(output.dir,"plots")
#
## Name of the directory where all the community detection algorithms results will be stored
#output.community.dir <- file.path(output.dir,"community_algorithms_results")
#
## Name of the directory where all the detection algorithms csv files will be stored
#output.community.csv.dir <- file.path(output.dir,"community_algorithms_csv")
#
## Name of the directory where all the generated graphs will be stored
#output.graphs.dir <- file.path(output.dir,"graphs")
#
## Name of the directory where all the results related to agreement will be stored
#output.agreement.dir <- file.path(output.dir,"agreement")
