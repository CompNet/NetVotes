#############################################################################################
# This script allows defining all the file constants used by the other scripts and functions.
# This include both file names and folder names.
# 
# 07/2015 Israel Mendonça (v1)
# 09/2015 Vincent Labatut (v2)
#############################################################################################


#############################################################################################
# Folders
#############################################################################################
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
	
		
#############################################################################################
# Files
#############################################################################################
## created overall tables
DOC.DETAILS.FILE		<- file.path(OVERALL.FOLDER,"doc-details.csv")
ALL.VOTES.FILE			<- file.path(OVERALL.FOLDER,"all-votes.csv")
MEP.DETAILS.FILE		<- file.path(OVERALL.FOLDER,"mep-details.csv")
MEP.LOYALTY.FILE		<- file.path(OVERALL.FOLDER,"mep-loyalty.csv")
DOMAIN.FREQ.FILE		<- file.path(OVERALL.FOLDER,"domain-freq.csv")


#############################################################################################
# Vote values
#############################################################################################
VOTE.FOR <- "For"
VOTE.ABST <- "Abstention"
VOTE.AGST <- "Against"
VOTE.NONE <- "NoVote"
VOTE.ABSENT <- "Absent"
VOTE.DOCABSENT <- "DocAbsent"


#############################################################################################
# Normalized policy domain names
#############################################################################################
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


#############################################################################################
# Temporal constants
#############################################################################################
# TODO We might need to adapt that, these years seem quite arbitrary.
# Maybe switch to a sliding window based on plenary sessions.
DATE.T7 <- c()
DATE.T7.Y1 <- c("01/07/2009","31/05/2010")
DATE.T7[DATE.T7.Y1] <- "2009-10"
DATE.T7.Y2 <- c("01/06/2010","31/05/2011")
DATE.T7[DATE.T7.Y2] <- "2010-11"
DATE.T7.Y3 <- c("01/06/2011","31/05/2012")
DATE.T7[DATE.T7.Y3] <- "2011-12"
DATE.T7.Y4 <- c("01/06/2012","31/05/2013")
DATE.T7[DATE.T7.Y4] <- "2012-13"
DATE.T7.Y5 <- c("01/06/2013","01/05/2014")
DATE.T7[DATE.T7.Y5] <- "2013-14"
DATE.T7.ALL <- c("01/07/2009","01/05/2014")
DATE.T7[DATE.T7.ALL] <- "Term"


#############################################################################################
# Table column names
#############################################################################################
## raw data
COL.DATE		<- "Date"
COL.DOCID		<- "Doc Id"
COL.DOMAIN		<- "Domain"
COL.DOMID		<- "Domain Id"
COL.FIRSTNAME	<- "Firstname"
COL.FREQUENCY	<- "Frequency"
COL.FULLNAME	<- "Full name"
COL.GROUP		<- "Group"
COL.LASTNAME	<- "Lastname"
COL.MEPID		<- "MEP Id"
COL.RESULT		<- "Vote result"
COL.STATE		<- "State"
COL.TITLE		<- "Title"


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
