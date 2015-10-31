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
ALL.VOTES.FILE		<- file.path(OVERALL.FOLDER,"all-votes.csv")
DOC.DETAILS.FILE	<- file.path(OVERALL.FOLDER,"doc-details.csv")
GROUP.LINES.FILE	<- file.path(OVERALL.FOLDER,"group-lines.csv")
MEP.DETAILS.FILE	<- file.path(OVERALL.FOLDER,"mep-details.csv")
MEP.BEHAVIOR.FILE	<- file.path(OVERALL.FOLDER,"mep-behavior.csv")


#############################################################################################
# Vote values
#############################################################################################
VOTE.VALUES <- c()
VOTE.VALUES.SMPL <- c()
VOTE.FOR <- "For"
VOTE.VALUES <- c(VOTE.VALUES, VOTE.FOR)
VOTE.VALUES.SMPL <- c(VOTE.VALUES.SMPL,VOTE.FOR)
VOTE.ABST <- "Abstention"
VOTE.VALUES <- c(VOTE.VALUES, VOTE.ABST)
VOTE.AGST <- "Against"
VOTE.VALUES <- c(VOTE.VALUES, VOTE.AGST)
VOTE.VALUES.SMPL <- c(VOTE.VALUES.SMPL,VOTE.AGST)
VOTE.NONE <- "NoVote"
VOTE.VALUES <- c(VOTE.VALUES, VOTE.NONE)
VOTE.ABSENT <- "Absent"
VOTE.VALUES <- c(VOTE.VALUES, VOTE.ABSENT)
VOTE.DOCABSENT <- "DocAbsent"
VOTE.VALUES <- c(VOTE.VALUES, VOTE.DOCABSENT)
VOTE.OTHER <- "Other"
VOTE.VALUES.SMPL <- c(VOTE.VALUES.SMPL,VOTE.OTHER)


#############################################################################################
# Behavior values
#############################################################################################
BEHAVIOR.VALUES <- c()
BEHAVIOR.LOYAL = "Loyal"
BEHAVIOR.VALUES <- c(BEHAVIOR.VALUES, BEHAVIOR.LOYAL)
BEHAVIOR.REBEL = "Rebel"
BEHAVIOR.VALUES <- c(BEHAVIOR.VALUES, BEHAVIOR.REBEL)


#############################################################################################
# Normalized policy domain names
#############################################################################################
# general lists
DOMAIN.VALUES <- c()
DOMAIN.FULLNAMES <- c()
# all domains
DOM.ALL <- "_All"
# Constitutional Affairs
DOM.AFCO <- "AFCO"
DOMAIN.FULLNAMES[DOM.AFCO] <- "Constitutional Affairs"
DOMAIN.VALUES <- c(DOMAIN.VALUES,DOM.AFCO)
# Foreign Affairs
DOM.AFET <- "AFET"
DOMAIN.FULLNAMES[DOM.AFET] <- "Foreign Affairs"
DOMAIN.VALUES <- c(DOMAIN.VALUES,DOM.AFET)
	# Human Rights (seems included in AFET)
	#DOM.DROI <- "DROI"
	#DOMAIN.FULLNAMES[DOM.DROI] <- "Human Rights"
	#DOMAIN.VALUES <- c(DOMAIN.VALUES,DOM.DROI)
	# Security and Defence (seems included in AFET)
	#DOM.SEDE <- "SEDE"
	#DOMAIN.FULLNAMES[DOM.SEDE] <- "Security and Defence"
	#DOMAIN.VALUES <- c(DOMAIN.VALUES,DOM.SEDE)
# Agriculture and Rural Development
DOM.AGRI <- "AGRI"
DOMAIN.FULLNAMES[DOM.AGRI] <- "Agriculture and Rural Development"
DOMAIN.VALUES <- c(DOMAIN.VALUES,DOM.AGRI)
# Budgets
DOM.BUDG <- "BUDG"
DOMAIN.FULLNAMES[DOM.BUDG] <- "Budgets"
DOMAIN.VALUES <- c(DOMAIN.VALUES,DOM.BUDG)
# Budgetary Control
DOM.CONT <- "CONT"
DOMAIN.FULLNAMES[DOM.CONT] <- "Budgetary Control"
DOMAIN.VALUES <- c(DOMAIN.VALUES,DOM.CONT)
# Culture and Education
DOM.CULT <- "CULT"
DOMAIN.FULLNAMES[DOM.CULT] <- "Culture and Education"
DOMAIN.VALUES <- c(DOMAIN.VALUES,DOM.CULT)
# Development
DOM.DEVE <- "DEVE"
DOMAIN.FULLNAMES[DOM.DEVE] <- "Development"
DOMAIN.VALUES <- c(DOMAIN.VALUES,DOM.DEVE)
# Economic and Monetary Affairs
DOM.ECON <- "ECON"
DOMAIN.FULLNAMES[DOM.ECON] <- "Economic and Monetary Affairs"
DOMAIN.VALUES <- c(DOMAIN.VALUES,DOM.ECON)
# Employment and Social Affairs
DOM.EMPL <- "EMPL"
DOMAIN.FULLNAMES[DOM.EMPL] <- "Employment and Social Affairs"
DOMAIN.VALUES <- c(DOMAIN.VALUES,DOM.EMPL)
# Environment, Public Health and Food Safety
DOM.ENVI <- "ENVI"
DOMAIN.FULLNAMES[DOM.ENVI] <- "Environment, Public Health and Food Safety"
DOMAIN.VALUES <- c(DOMAIN.VALUES,DOM.ENVI)
# Women's Rights and Gender Equality
DOM.FEMM <- "FEMM"
DOMAIN.FULLNAMES[DOM.FEMM] <- "Women's Rights and Gender Equality"
DOMAIN.VALUES <- c(DOMAIN.VALUES,DOM.FEMM)
# Internal Market and Consumer Protection
DOM.IMCO <- "IMCO"
DOMAIN.FULLNAMES[DOM.IMCO] <- "Internal Market and Consumer Protection"
DOMAIN.VALUES <- c(DOMAIN.VALUES,DOM.IMCO)
# International Trade
DOM.INTA <- "INTA"
DOMAIN.FULLNAMES[DOM.INTA] <- "International Trade"
DOMAIN.VALUES <- c(DOMAIN.VALUES,DOM.INTA)
# Industry, Research and Energy
DOM.ITRE <- "ITRE"
DOMAIN.FULLNAMES[DOM.ITRE] <- "Industry, Research and Energy"
DOMAIN.VALUES <- c(DOMAIN.VALUES,DOM.ITRE)
# Legal Affairs
DOM.JURI <- "JURI"
DOMAIN.FULLNAMES[DOM.JURI] <- "Legal Affairs"
DOMAIN.VALUES <- c(DOMAIN.VALUES,DOM.JURI)
# Civil Liberties, Justice and Home Affairs
DOM.LIBE <- "LIBE"
DOMAIN.FULLNAMES[DOM.LIBE] <- "Civil Liberties, Justice and Home Affairs"
DOMAIN.VALUES <- c(DOMAIN.VALUES,DOM.LIBE)
# Fisheries
DOM.PECH <- "PECH"
DOMAIN.FULLNAMES[DOM.PECH] <- "Fisheries"
DOMAIN.VALUES <- c(DOMAIN.VALUES,DOM.PECH)
# Petitions
DOM.PETI <- "PETI"
DOMAIN.FULLNAMES[DOM.PETI] <- "Petitions"
DOMAIN.VALUES <- c(DOMAIN.VALUES,DOM.PETI)
# Regional Development
DOM.REGI <- "REGI"
DOMAIN.FULLNAMES[DOM.REGI] <- "Regional Development"
DOMAIN.VALUES <- c(DOMAIN.VALUES,DOM.REGI)
# Internal regulations of the EP
DOM.RIPE <- "RIPE"
DOMAIN.FULLNAMES[DOM.RIPE] <- "Internal regulations of the EP"
DOMAIN.VALUES <- c(DOMAIN.VALUES,DOM.RIPE)
# Transport and Tourism
DOM.TRAN <- "TRAN"
DOMAIN.FULLNAMES[DOM.TRAN] <- "Transport and Tourism"
DOMAIN.VALUES <- c(DOMAIN.VALUES,DOM.TRAN)


#############################################################################################
# Temporal constants
#############################################################################################
# TODO We might need to adapt that, these years seem quite arbitrary.
# Maybe switch to a sliding window based on plenary sessions.
DATE.STR.T7 <- c()
DATE.START.T7 <- list()
DATE.END.T7 <- list()
DATE.T7.Y1 <- "DATE.T7.Y1"
DATE.START.T7[[DATE.T7.Y1]] 	<- as.Date("01/07/2009","%d/%m/%Y")
DATE.END.T7[[DATE.T7.Y1]] 		<- as.Date("31/05/2010","%d/%m/%Y")
DATE.STR.T7[DATE.T7.Y1] <- "2009-10"
DATE.T7.Y2 <- "DATE.T7.Y2"
DATE.START.T7[[DATE.T7.Y2]] 	<- as.Date("01/06/2010","%d/%m/%Y")
DATE.END.T7[[DATE.T7.Y2]] 		<- as.Date("31/05/2011","%d/%m/%Y")
DATE.STR.T7[DATE.T7.Y2] <- "2010-11"
DATE.T7.Y3 <- "DATE.T7.Y3"
DATE.START.T7[[DATE.T7.Y3]] 	<- as.Date("01/06/2011","%d/%m/%Y")
DATE.END.T7[[DATE.T7.Y3] ]		<- as.Date("31/05/2012","%d/%m/%Y")
DATE.STR.T7[DATE.T7.Y3] <- "2011-12"
DATE.T7.Y4 <- "DATE.T7.Y4"
DATE.START.T7[[DATE.T7.Y4]] 	<- as.Date("01/06/2012","%d/%m/%Y")
DATE.END.T7[[DATE.T7.Y4]] 		<- as.Date("31/05/2013","%d/%m/%Y")
DATE.STR.T7[DATE.T7.Y4] <- "2012-13"
DATE.T7.Y5 <- "DATE.T7.Y5"
DATE.START.T7[[DATE.T7.Y5]] 	<- as.Date("01/06/2013","%d/%m/%Y")
DATE.END.T7[[DATE.T7.Y5]] 		<- as.Date("01/05/2014","%d/%m/%Y")
DATE.STR.T7[DATE.T7.Y5] <- "2013-14"
DATE.T7.ALL <- "DATE.T7.ALL"
DATE.START.T7[[DATE.T7.ALL]]	<- as.Date("01/07/2009","%d/%m/%Y")
DATE.END.T7[[DATE.T7.ALL]] 		<- as.Date("01/05/2014","%d/%m/%Y")
DATE.STR.T7[DATE.T7.ALL] <- "Term"
DATE.T7.YEARS <- c(DATE.T7.Y1,DATE.T7.Y2,DATE.T7.Y3,DATE.T7.Y4,DATE.T7.Y5)


#############################################################################################
# Political groups
#############################################################################################
GROUP.NAMES <- c()
GROUP.FULLNAMES <- c()
GROUP.ALDE <- "ALDE"
GROUP.NAMES <- c(GROUP.NAMES, GROUP.ALDE)
GROUP.FULLNAMES[GROUP.ALDE] <- "Alliance of Liberals and Democrats for Europe"
GROUP.ECR <- "ECR"
GROUP.NAMES <- c(GROUP.NAMES, GROUP.ECR)
GROUP.FULLNAMES[GROUP.ECR] <- "European Conservatives and Reformists"
GROUP.EFD <- "EFD"
GROUP.NAMES <- c(GROUP.NAMES, GROUP.EFD)
GROUP.FULLNAMES[GROUP.EFD] <- "Europe of Freedom and Democracy"
GROUP.EPP <- "EPP"
GROUP.NAMES <- c(GROUP.NAMES, GROUP.EPP)
GROUP.FULLNAMES[GROUP.EPP] <- "European People's Party"
GROUP.GREENS <- "Greens"
GROUP.NAMES <- c(GROUP.NAMES, GROUP.GREENS)
GROUP.FULLNAMES[GROUP.GREENS] <- "The Greens–European Free Alliance"
GROUP.GUENGL <- "GUENGL"
GROUP.NAMES <- c(GROUP.NAMES, GROUP.GUENGL)
GROUP.FULLNAMES[GROUP.GUENGL] <- "European United Left–Nordic Green Left"
GROUP.NI <- "NI"
GROUP.NAMES <- c(GROUP.NAMES, GROUP.NI)
GROUP.FULLNAMES[GROUP.NI] <- "Non-Inscrits"
GROUP.SD <- "SD"
GROUP.NAMES <- c(GROUP.NAMES, GROUP.SD)
GROUP.FULLNAMES[GROUP.SD] <- "Progressive Alliance of Socialists and Democrats"


#############################################################################################
# Country names
#############################################################################################
COUNTRY.VALUES <- c()
COUNTRY.AT <- "Austria"
COUNTRY.VALUES <- c(COUNTRY.VALUES, COUNTRY.AT)
COUNTRY.BE <- "Belgium"
COUNTRY.VALUES <- c(COUNTRY.VALUES, COUNTRY.BE)
COUNTRY.BG <- "Bulgaria"
COUNTRY.VALUES <- c(COUNTRY.VALUES, COUNTRY.BG)
COUNTRY.HR <- "Croatia"
COUNTRY.VALUES <- c(COUNTRY.VALUES, COUNTRY.HR)
COUNTRY.CY <- "Cyprus"
COUNTRY.VALUES <- c(COUNTRY.VALUES, COUNTRY.CY)
COUNTRY.CZ <- "Czech Republic"
COUNTRY.VALUES <- c(COUNTRY.VALUES, COUNTRY.CZ)
COUNTRY.DK <- "Denmark"
COUNTRY.VALUES <- c(COUNTRY.VALUES, COUNTRY.DK)
COUNTRY.EE <- "Estonia"
COUNTRY.VALUES <- c(COUNTRY.VALUES, COUNTRY.EE)
COUNTRY.FI <- "Finland"
COUNTRY.VALUES <- c(COUNTRY.VALUES, COUNTRY.FI)
COUNTRY.FR <- "France"
COUNTRY.VALUES <- c(COUNTRY.VALUES, COUNTRY.FR)
COUNTRY.DE <- "Germany"
COUNTRY.VALUES <- c(COUNTRY.VALUES, COUNTRY.DE)
COUNTRY.GR <- "Greece"
COUNTRY.VALUES <- c(COUNTRY.VALUES, COUNTRY.GR)
COUNTRY.HU <- "Hungary"
COUNTRY.VALUES <- c(COUNTRY.VALUES, COUNTRY.HU)
COUNTRY.IE <- "Ireland"
COUNTRY.VALUES <- c(COUNTRY.VALUES, COUNTRY.IE)
COUNTRY.IT <- "Italy"
COUNTRY.VALUES <- c(COUNTRY.VALUES, COUNTRY.IT)
COUNTRY.LV <- "Latvia"
COUNTRY.VALUES <- c(COUNTRY.VALUES, COUNTRY.LV)
COUNTRY.LT <- "Lithuania"
COUNTRY.VALUES <- c(COUNTRY.VALUES, COUNTRY.LT)
COUNTRY.LU <- "Luxembourg"
COUNTRY.VALUES <- c(COUNTRY.VALUES, COUNTRY.LU)
COUNTRY.MT <- "Malta"
COUNTRY.VALUES <- c(COUNTRY.VALUES, COUNTRY.MT)
COUNTRY.NL <- "Netherlands"
COUNTRY.VALUES <- c(COUNTRY.VALUES, COUNTRY.NL)
COUNTRY.PL <- "Poland"
COUNTRY.VALUES <- c(COUNTRY.VALUES, COUNTRY.PL)
COUNTRY.PT <- "Portugal"
COUNTRY.VALUES <- c(COUNTRY.VALUES, COUNTRY.PT)
COUNTRY.RO <- "Romania"
COUNTRY.VALUES <- c(COUNTRY.VALUES, COUNTRY.RO)
COUNTRY.SK <- "Slovakia"
COUNTRY.VALUES <- c(COUNTRY.VALUES, COUNTRY.SK)
COUNTRY.SI <- "Slovenia"
COUNTRY.VALUES <- c(COUNTRY.VALUES, COUNTRY.SI)
COUNTRY.ES <- "Spain"
COUNTRY.VALUES <- c(COUNTRY.VALUES, COUNTRY.ES)
COUNTRY.SE <- "Sweden"
COUNTRY.VALUES <- c(COUNTRY.VALUES, COUNTRY.SE)
COUNTRY.UK <- "United Kingdom"
COUNTRY.VALUES <- c(COUNTRY.VALUES, COUNTRY.UK)


#############################################################################################
# Table column names
#############################################################################################
## raw data
COL.COUNT		<- "Count"
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
COL.PERCENT		<- "Percent"
COL.RESULT		<- "Vote result"
COL.STATE		<- "State"
COL.TITLE		<- "Title"
COL.VOTE		<- "Vote"


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
