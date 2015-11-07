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
	# folder containing the score matrices
	SCORE.FOLDER <- file.path(IN.FOLDER,"score")
# general ouput folder
OUT.FOLDER <- "out"
	# output folder for everything not network-related
	OVERALL.FOLDER <- file.path(OUT.FOLDER,"_overall")
	# pils folder (tempary)
	PILS.FOLDER <- file.path(OUT.FOLDER,"pils")
	# votes folder
	VOTES.FOLDER <- file.path(OUT.FOLDER,"votes")
	# behavior folder
	BEHAVIOR.FOLDER <- file.path(OUT.FOLDER,"behavior")
	# agreement folder
	AGREEMENT.FOLDER <- file.path(OUT.FOLDER,"agreement")
	# networks folder
	NETWORKS.FOLDER <- file.path(OUT.FOLDER,"networks")
	
		
#############################################################################################
# Files
#############################################################################################
## overall tables created during the preprocessing
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
DOMAIN.ALL <- "_All"
# Constitutional Affairs
DOMAIN.AFCO <- "AFCO"
DOMAIN.FULLNAMES[DOMAIN.AFCO] <- "Constitutional Affairs"
DOMAIN.VALUES <- c(DOMAIN.VALUES,DOMAIN.AFCO)
# Foreign Affairs
DOMAIN.AFET <- "AFET"
DOMAIN.FULLNAMES[DOMAIN.AFET] <- "Foreign Affairs"
DOMAIN.VALUES <- c(DOMAIN.VALUES,DOMAIN.AFET)
	# Human Rights (seems included in AFET)
	#DOMAIN.DROI <- "DROI"
	#DOMAIN.FULLNAMES[DOMAIN.DROI] <- "Human Rights"
	#DOMAIN.VALUES <- c(DOMAIN.VALUES,DOMAIN.DROI)
	# Security and Defence (seems included in AFET)
	#DOMAIN.SEDE <- "SEDE"
	#DOMAIN.FULLNAMES[DOMAIN.SEDE] <- "Security and Defence"
	#DOMAIN.VALUES <- c(DOMAIN.VALUES,DOMAIN.SEDE)
# Agriculture and Rural Development
DOMAIN.AGRI <- "AGRI"
DOMAIN.FULLNAMES[DOMAIN.AGRI] <- "Agriculture and Rural Development"
DOMAIN.VALUES <- c(DOMAIN.VALUES,DOMAIN.AGRI)
# Budgets
DOMAIN.BUDG <- "BUDG"
DOMAIN.FULLNAMES[DOMAIN.BUDG] <- "Budgets"
DOMAIN.VALUES <- c(DOMAIN.VALUES,DOMAIN.BUDG)
# Budgetary Control
DOMAIN.CONT <- "CONT"
DOMAIN.FULLNAMES[DOMAIN.CONT] <- "Budgetary Control"
DOMAIN.VALUES <- c(DOMAIN.VALUES,DOMAIN.CONT)
# Culture and Education
DOMAIN.CULT <- "CULT"
DOMAIN.FULLNAMES[DOMAIN.CULT] <- "Culture and Education"
DOMAIN.VALUES <- c(DOMAIN.VALUES,DOMAIN.CULT)
# Development
DOMAIN.DEVE <- "DEVE"
DOMAIN.FULLNAMES[DOMAIN.DEVE] <- "Development"
DOMAIN.VALUES <- c(DOMAIN.VALUES,DOMAIN.DEVE)
# Economic and Monetary Affairs
DOMAIN.ECON <- "ECON"
DOMAIN.FULLNAMES[DOMAIN.ECON] <- "Economic and Monetary Affairs"
DOMAIN.VALUES <- c(DOMAIN.VALUES,DOMAIN.ECON)
# Employment and Social Affairs
DOMAIN.EMPL <- "EMPL"
DOMAIN.FULLNAMES[DOMAIN.EMPL] <- "Employment and Social Affairs"
DOMAIN.VALUES <- c(DOMAIN.VALUES,DOMAIN.EMPL)
# Environment, Public Health and Food Safety
DOMAIN.ENVI <- "ENVI"
DOMAIN.FULLNAMES[DOMAIN.ENVI] <- "Environment, Public Health and Food Safety"
DOMAIN.VALUES <- c(DOMAIN.VALUES,DOMAIN.ENVI)
# Women's Rights and Gender Equality
DOMAIN.FEMM <- "FEMM"
DOMAIN.FULLNAMES[DOMAIN.FEMM] <- "Women's Rights and Gender Equality"
DOMAIN.VALUES <- c(DOMAIN.VALUES,DOMAIN.FEMM)
# Internal Market and Consumer Protection
DOMAIN.IMCO <- "IMCO"
DOMAIN.FULLNAMES[DOMAIN.IMCO] <- "Internal Market and Consumer Protection"
DOMAIN.VALUES <- c(DOMAIN.VALUES,DOMAIN.IMCO)
# International Trade
DOMAIN.INTA <- "INTA"
DOMAIN.FULLNAMES[DOMAIN.INTA] <- "International Trade"
DOMAIN.VALUES <- c(DOMAIN.VALUES,DOMAIN.INTA)
# Industry, Research and Energy
DOMAIN.ITRE <- "ITRE"
DOMAIN.FULLNAMES[DOMAIN.ITRE] <- "Industry, Research and Energy"
DOMAIN.VALUES <- c(DOMAIN.VALUES,DOMAIN.ITRE)
# Legal Affairs
DOMAIN.JURI <- "JURI"
DOMAIN.FULLNAMES[DOMAIN.JURI] <- "Legal Affairs"
DOMAIN.VALUES <- c(DOMAIN.VALUES,DOMAIN.JURI)
# Civil Liberties, Justice and Home Affairs
DOMAIN.LIBE <- "LIBE"
DOMAIN.FULLNAMES[DOMAIN.LIBE] <- "Civil Liberties, Justice and Home Affairs"
DOMAIN.VALUES <- c(DOMAIN.VALUES,DOMAIN.LIBE)
# Fisheries
DOMAIN.PECH <- "PECH"
DOMAIN.FULLNAMES[DOMAIN.PECH] <- "Fisheries"
DOMAIN.VALUES <- c(DOMAIN.VALUES,DOMAIN.PECH)
# Petitions
DOMAIN.PETI <- "PETI"
DOMAIN.FULLNAMES[DOMAIN.PETI] <- "Petitions"
DOMAIN.VALUES <- c(DOMAIN.VALUES,DOMAIN.PETI)
# Regional Development
DOMAIN.REGI <- "REGI"
DOMAIN.FULLNAMES[DOMAIN.REGI] <- "Regional Development"
DOMAIN.VALUES <- c(DOMAIN.VALUES,DOMAIN.REGI)
# Internal regulations of the EP
DOMAIN.RIPE <- "RIPE"
DOMAIN.FULLNAMES[DOMAIN.RIPE] <- "Internal regulations of the EP"
DOMAIN.VALUES <- c(DOMAIN.VALUES,DOMAIN.RIPE)
# Transport and Tourism
DOMAIN.TRAN <- "TRAN"
DOMAIN.FULLNAMES[DOMAIN.TRAN] <- "Transport and Tourism"
DOMAIN.VALUES <- c(DOMAIN.VALUES,DOMAIN.TRAN)


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
DATE.T7.TERM <- "DATE.T7.TERM"
DATE.START.T7[[DATE.T7.TERM]]	<- as.Date("01/07/2009","%d/%m/%Y")
DATE.END.T7[[DATE.T7.TERM]] 		<- as.Date("01/05/2014","%d/%m/%Y")
DATE.STR.T7[DATE.T7.TERM] <- "Term"
DATE.T7.YEARS <- c(DATE.T7.Y1,DATE.T7.Y2,DATE.T7.Y3,DATE.T7.Y4,DATE.T7.Y5)


#############################################################################################
# Political groups (normalized names)
#############################################################################################
GROUP.VALUES <- c()
GROUP.FULLNAMES <- c()
GROUP.ALDE <- "ALDE"
GROUP.VALUES <- c(GROUP.VALUES, GROUP.ALDE)
GROUP.FULLNAMES[GROUP.ALDE] <- "Alliance of Liberals and Democrats for Europe"
GROUP.ECR <- "ECR"
GROUP.VALUES <- c(GROUP.VALUES, GROUP.ECR)
GROUP.FULLNAMES[GROUP.ECR] <- "European Conservatives and Reformists"
GROUP.EFD <- "EFD"
GROUP.VALUES <- c(GROUP.VALUES, GROUP.EFD)
GROUP.FULLNAMES[GROUP.EFD] <- "Europe of Freedom and Democracy"
GROUP.EPP <- "EPP"
GROUP.VALUES <- c(GROUP.VALUES, GROUP.EPP)
GROUP.FULLNAMES[GROUP.EPP] <- "European People's Party"
GROUP.GREENS <- "Greens"
GROUP.VALUES <- c(GROUP.VALUES, GROUP.GREENS)
GROUP.FULLNAMES[GROUP.GREENS] <- "The Greens–European Free Alliance"
GROUP.GUENGL <- "GUENGL"
GROUP.VALUES <- c(GROUP.VALUES, GROUP.GUENGL)
GROUP.FULLNAMES[GROUP.GUENGL] <- "European United Left–Nordic Green Left"
GROUP.NI <- "NI"
GROUP.VALUES <- c(GROUP.VALUES, GROUP.NI)
GROUP.FULLNAMES[GROUP.NI] <- "Non-Inscrits"
GROUP.SD <- "SD"
GROUP.VALUES <- c(GROUP.VALUES, GROUP.SD)
GROUP.FULLNAMES[GROUP.SD] <- "Progressive Alliance of Socialists and Democrats"


#############################################################################################
# Country names (using ISO codes)
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
