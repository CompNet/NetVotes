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
	# output folder for the domain-related overall files
	DOMAINS.FOLDER <- file.path(OVERALL.FOLDER,"_domains")
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
	# folder containing the estimated partitions
	PARTITIONS.FOLDER <- file.path(OUT.FOLDER,"partitions")
	
		
#############################################################################################
# Files
#############################################################################################
## overall tables created during the preprocessing
	ALL.VOTES.FILE		<- file.path(OVERALL.FOLDER,"all-votes.csv")
	DOC.DETAILS.FILE	<- file.path(OVERALL.FOLDER,"doc-details.csv")
	GROUP.LINES.FILE	<- file.path(OVERALL.FOLDER,"group-lines.csv")
	MEP.DETAILS.FILE	<- file.path(OVERALL.FOLDER,"mep-details.csv")
	MEP.BEHAVIOR.FILE	<- file.path(OVERALL.FOLDER,"mep-behavior.csv")
	DOMAIN.DETAILS.FILE <- file.path(OVERALL.FOLDER,"dom-details.csv")
	DOC.DOMAINS.FILE 	<- file.path(OVERALL.FOLDER,"doc-domains.csv")
# graph files
	SIGNED.FILE <- "signed"
	POSITIVE.FILE <- "positive"
	COMP.NEGATIVE.FILE <- "compneg"


#############################################################################################
# Table column names
#############################################################################################
## raw data
COL.BIRTHDATE 	<- "Birth date"
COL.BIRTHPLACE 	<- "Birth place"
COL.COUNT		<- "Count"
COL.DATE		<- "Date"
COL.DOCID		<- "Doc Id"
COL.DOMAIN		<- "Domain"
COL.DOMID		<- "Domain Id"
COL.EP.ID 		<- "EP Id"
COL.FIRSTNAME	<- "Firstname"
COL.FREQUENCY	<- "Frequency"
COL.FULLNAME	<- "Full name"
COL.GROUP		<- "Group"
COL.LASTNAME	<- "Lastname"
COL.MEPID		<- "MEP Id"
COL.PARTY 		<- "Party"
COL.PERCENT		<- "Percent"
COL.RESULT		<- "Vote result"
COL.STATE		<- "State"
COL.TITLE		<- "Title"
COL.VOTE		<- "Vote"
COL.DOC.REF		<- "Document Reference"
COL.FULL.TITLE	<- "Full title"
COL.EP.REF		<- "EP reference"
COL.REPORTER.ID	<- "Reporter ID"



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
DOMAIN.VALUES <- c()
DOMAIN.FULLNAMES <- c()
DOMAIN.ALL <- "_All"
DOMAIN.AFCO <- "AFCO"
	DOMAIN.FULLNAMES[DOMAIN.AFCO] <- "Constitutional Affairs"
	DOMAIN.VALUES <- c(DOMAIN.VALUES,DOMAIN.AFCO)
DOMAIN.AFET <- "AFET"
	DOMAIN.FULLNAMES[DOMAIN.AFET] <- "Foreign Affairs"
	DOMAIN.VALUES <- c(DOMAIN.VALUES,DOMAIN.AFET)
	#DOMAIN.DROI <- "DROI"
		#DOMAIN.FULLNAMES[DOMAIN.DROI] <- "Human Rights"
		#DOMAIN.VALUES <- c(DOMAIN.VALUES,DOMAIN.DROI)
	#DOMAIN.SEDE <- "SEDE"
		#DOMAIN.FULLNAMES[DOMAIN.SEDE] <- "Security and Defence"
		#DOMAIN.VALUES <- c(DOMAIN.VALUES,DOMAIN.SEDE)
DOMAIN.AGRI <- "AGRI"
	DOMAIN.FULLNAMES[DOMAIN.AGRI] <- "Agriculture and Rural Development"
	DOMAIN.VALUES <- c(DOMAIN.VALUES,DOMAIN.AGRI)
DOMAIN.BUDG <- "BUDG"
	DOMAIN.FULLNAMES[DOMAIN.BUDG] <- "Budgets"
	DOMAIN.VALUES <- c(DOMAIN.VALUES,DOMAIN.BUDG)
DOMAIN.CONT <- "CONT"
	DOMAIN.FULLNAMES[DOMAIN.CONT] <- "Budgetary Control"
	DOMAIN.VALUES <- c(DOMAIN.VALUES,DOMAIN.CONT)
DOMAIN.CULT <- "CULT"
	DOMAIN.FULLNAMES[DOMAIN.CULT] <- "Culture and Education"
	DOMAIN.VALUES <- c(DOMAIN.VALUES,DOMAIN.CULT)
DOMAIN.DEVE <- "DEVE"
	DOMAIN.FULLNAMES[DOMAIN.DEVE] <- "Development"
	DOMAIN.VALUES <- c(DOMAIN.VALUES,DOMAIN.DEVE)
DOMAIN.ECON <- "ECON"
	DOMAIN.FULLNAMES[DOMAIN.ECON] <- "Economic and Monetary Affairs"
	DOMAIN.VALUES <- c(DOMAIN.VALUES,DOMAIN.ECON)
DOMAIN.EMPL <- "EMPL"
	DOMAIN.FULLNAMES[DOMAIN.EMPL] <- "Employment and Social Affairs"
	DOMAIN.VALUES <- c(DOMAIN.VALUES,DOMAIN.EMPL)
DOMAIN.ENVI <- "ENVI"
	DOMAIN.FULLNAMES[DOMAIN.ENVI] <- "Environment, Public Health and Food Safety"
	DOMAIN.VALUES <- c(DOMAIN.VALUES,DOMAIN.ENVI)
DOMAIN.FEMM <- "FEMM"
	DOMAIN.FULLNAMES[DOMAIN.FEMM] <- "Women's Rights and Gender Equality"
	DOMAIN.VALUES <- c(DOMAIN.VALUES,DOMAIN.FEMM)
DOMAIN.IMCO <- "IMCO"
	DOMAIN.FULLNAMES[DOMAIN.IMCO] <- "Internal Market and Consumer Protection"
	DOMAIN.VALUES <- c(DOMAIN.VALUES,DOMAIN.IMCO)
DOMAIN.INTA <- "INTA"
	DOMAIN.FULLNAMES[DOMAIN.INTA] <- "International Trade"
	DOMAIN.VALUES <- c(DOMAIN.VALUES,DOMAIN.INTA)
DOMAIN.ITRE <- "ITRE"
	DOMAIN.FULLNAMES[DOMAIN.ITRE] <- "Industry, Research and Energy"
	DOMAIN.VALUES <- c(DOMAIN.VALUES,DOMAIN.ITRE)
DOMAIN.JURI <- "JURI"
	DOMAIN.FULLNAMES[DOMAIN.JURI] <- "Legal Affairs"
	DOMAIN.VALUES <- c(DOMAIN.VALUES,DOMAIN.JURI)
DOMAIN.LIBE <- "LIBE"
	DOMAIN.FULLNAMES[DOMAIN.LIBE] <- "Civil Liberties, Justice and Home Affairs"
	DOMAIN.VALUES <- c(DOMAIN.VALUES,DOMAIN.LIBE)
DOMAIN.PECH <- "PECH"
	DOMAIN.FULLNAMES[DOMAIN.PECH] <- "Fisheries"
	DOMAIN.VALUES <- c(DOMAIN.VALUES,DOMAIN.PECH)
DOMAIN.PETI <- "PETI"
	DOMAIN.FULLNAMES[DOMAIN.PETI] <- "Petitions"
	DOMAIN.VALUES <- c(DOMAIN.VALUES,DOMAIN.PETI)
DOMAIN.REGI <- "REGI"
	DOMAIN.FULLNAMES[DOMAIN.REGI] <- "Regional Development"
	DOMAIN.VALUES <- c(DOMAIN.VALUES,DOMAIN.REGI)
DOMAIN.RIPE <- "RIPE"
	DOMAIN.FULLNAMES[DOMAIN.RIPE] <- "Internal regulations of the EP"
	DOMAIN.VALUES <- c(DOMAIN.VALUES,DOMAIN.RIPE)
DOMAIN.TRAN <- "TRAN"
	DOMAIN.FULLNAMES[DOMAIN.TRAN] <- "Transport and Tourism"
	DOMAIN.VALUES <- c(DOMAIN.VALUES,DOMAIN.TRAN)
DOMAIN.AUTR <- "AUTR"
	DOMAIN.FULLNAMES[DOMAIN.AUTR] <- "Other domains"
	DOMAIN.VALUES <- c(DOMAIN.VALUES,DOMAIN.AUTR)
	

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
# Partition measures for signed networks
#############################################################################################
CORCLU.MEAS.VALUES <- c()
CORCLU.MEAS.NAMES <- c()
CORCLU.MEAS.BOUNDS <- list() #returns the inf and sup bounds of the measure
	# unweighted counts
	CORCLU.MEAS.IMB.UNW.CNT.NEG <- "unIc"
	CORCLU.MEAS.VALUES <- c(CORCLU.MEAS.VALUES, CORCLU.MEAS.IMB.UNW.CNT.NEG)
	CORCLU.MEAS.NAMES[CORCLU.MEAS.IMB.UNW.CNT.NEG] <- "Unweighted Negative Imbalance (count)"
	CORCLU.MEAS.BOUNDS[[CORCLU.MEAS.IMB.UNW.CNT.NEG]] <- function(g) if(all(is.na(g))) c(0,NA) else c(0,length(E(g)$weight[E(g)$weight<0]))
	CORCLU.MEAS.IMB.UNW.CNT.POS <- "upIc"
	CORCLU.MEAS.VALUES <- c(CORCLU.MEAS.VALUES, CORCLU.MEAS.IMB.UNW.CNT.POS)
	CORCLU.MEAS.NAMES[CORCLU.MEAS.IMB.UNW.CNT.POS] <- "Unweighted Positive Imbalance (count)"
	CORCLU.MEAS.BOUNDS[[CORCLU.MEAS.IMB.UNW.CNT.POS]] <- function(g) if(all(is.na(g))) c(0,NA) else c(0,length(E(g)$weight[E(g)$weight>=0]))
	CORCLU.MEAS.IMB.UNW.CNT.TOTAL <- "utIc"
	CORCLU.MEAS.VALUES <- c(CORCLU.MEAS.VALUES, CORCLU.MEAS.IMB.UNW.CNT.TOTAL)
	CORCLU.MEAS.NAMES[CORCLU.MEAS.IMB.UNW.CNT.TOTAL] <- "Unweighted Total Imbalance (count)"
	CORCLU.MEAS.BOUNDS[[CORCLU.MEAS.IMB.UNW.CNT.TOTAL]] <- function(g) if(all(is.na(g))) c(0,NA) else c(0,length(E(g)$weight))
	# weighted counts
	CORCLU.MEAS.IMB.WGT.CNT.NEG <- "wnIc"
	CORCLU.MEAS.VALUES <- c(CORCLU.MEAS.VALUES, CORCLU.MEAS.IMB.WGT.CNT.NEG)
	CORCLU.MEAS.NAMES[CORCLU.MEAS.IMB.WGT.CNT.NEG] <- "Weighted Negative Imbalance (count)"
	CORCLU.MEAS.BOUNDS[[CORCLU.MEAS.IMB.WGT.CNT.NEG]] <- function(g) if(all(is.na(g))) c(0,NA) else c(0,sum(abs(E(g)$weight[E(g)$weight<0])))
	CORCLU.MEAS.IMB.WGT.CNT.POS <- "wpIc"
	CORCLU.MEAS.VALUES <- c(CORCLU.MEAS.VALUES, CORCLU.MEAS.IMB.WGT.CNT.POS)
	CORCLU.MEAS.NAMES[CORCLU.MEAS.IMB.WGT.CNT.POS] <- "Weighted Positive Imbalance (count)"
	CORCLU.MEAS.BOUNDS[[CORCLU.MEAS.IMB.WGT.CNT.POS]] <- function(g) if(all(is.na(g))) c(0,NA) else c(0,sum(abs(E(g)$weight[E(g)$weight>=0])))
	CORCLU.MEAS.IMB.WGT.CNT.TOTAL <- "wtIc"
	CORCLU.MEAS.VALUES <- c(CORCLU.MEAS.VALUES, CORCLU.MEAS.IMB.WGT.CNT.TOTAL)
	CORCLU.MEAS.NAMES[CORCLU.MEAS.IMB.WGT.CNT.TOTAL] <- "Weighted Total Imbalance (count)"
	CORCLU.MEAS.BOUNDS[[CORCLU.MEAS.IMB.WGT.CNT.TOTAL]] <- function(g) if(all(is.na(g))) c(0,NA) else c(0,sum(abs(E(g)$weight)))
	# unweighted proportions
	CORCLU.MEAS.IMB.UNW.PROP.NEG <- "unIp"
	CORCLU.MEAS.VALUES <- c(CORCLU.MEAS.VALUES, CORCLU.MEAS.IMB.UNW.PROP.NEG)
	CORCLU.MEAS.NAMES[CORCLU.MEAS.IMB.UNW.PROP.NEG] <- "Unweighted Negative Imbalance (prop.)"
	CORCLU.MEAS.BOUNDS[[CORCLU.MEAS.IMB.UNW.PROP.NEG]] <- function(g) c(0,1)
	CORCLU.MEAS.IMB.UNW.PROP.POS <- "upIp"
	CORCLU.MEAS.VALUES <- c(CORCLU.MEAS.VALUES, CORCLU.MEAS.IMB.UNW.PROP.POS)
	CORCLU.MEAS.NAMES[CORCLU.MEAS.IMB.UNW.PROP.POS] <- "Unweighted Positive Imbalance (prop.)"
	CORCLU.MEAS.BOUNDS[[CORCLU.MEAS.IMB.UNW.PROP.POS]] <- function(g) c(0,1)
	CORCLU.MEAS.IMB.UNW.PROP.TOTAL <- "utIp"
	CORCLU.MEAS.VALUES <- c(CORCLU.MEAS.VALUES, CORCLU.MEAS.IMB.UNW.PROP.TOTAL)
	CORCLU.MEAS.NAMES[CORCLU.MEAS.IMB.UNW.PROP.TOTAL] <- "Unweighted Total Imbalance (prop.)"
	CORCLU.MEAS.BOUNDS[[CORCLU.MEAS.IMB.UNW.PROP.TOTAL]] <- function(g) c(0,1)
	# weighted proportions
	CORCLU.MEAS.IMB.WGT.PROP.NEG <- "wnIp"
	CORCLU.MEAS.VALUES <- c(CORCLU.MEAS.VALUES, CORCLU.MEAS.IMB.WGT.PROP.NEG)
	CORCLU.MEAS.NAMES[CORCLU.MEAS.IMB.WGT.PROP.NEG] <- "Weighted Negative Imbalance (prop.)"
	CORCLU.MEAS.BOUNDS[[CORCLU.MEAS.IMB.WGT.PROP.NEG]] <- function(g) c(0,1)
	CORCLU.MEAS.IMB.WGT.PROP.POS <- "wpIp"
	CORCLU.MEAS.VALUES <- c(CORCLU.MEAS.VALUES, CORCLU.MEAS.IMB.WGT.PROP.POS)
	CORCLU.MEAS.NAMES[CORCLU.MEAS.IMB.WGT.PROP.POS] <- "Weighted Positive Imbalance (prop.)"
	CORCLU.MEAS.BOUNDS[[CORCLU.MEAS.IMB.WGT.PROP.POS]] <- function(g) c(0,1)
	CORCLU.MEAS.IMB.WGT.PROP.TOTAL <- "wtIp"
	CORCLU.MEAS.VALUES <- c(CORCLU.MEAS.VALUES, CORCLU.MEAS.IMB.WGT.PROP.TOTAL)
	CORCLU.MEAS.NAMES[CORCLU.MEAS.IMB.WGT.PROP.TOTAL] <- "Weighted Total Imbalance (prop.)"
	CORCLU.MEAS.BOUNDS[[CORCLU.MEAS.IMB.WGT.PROP.TOTAL]] <- function(g) c(0,1)
PART.MEAS.VALUES <- CORCLU.MEAS.VALUES
PART.MEAS.NAMES <- CORCLU.MEAS.NAMES
PART.MEAS.BOUNDS <- CORCLU.MEAS.BOUNDS	

	
#############################################################################################
# Partition measures for unsigned networks
#############################################################################################
COMDET.MEAS.VALUES <- c()
COMDET.MEAS.NAMES <- c()
COMDET.MEAS.BOUNDS <- list()
	# modularity
	COMDET.MEAS.MOD.UNW.NEG <- "unM"
	COMDET.MEAS.VALUES <- c(COMDET.MEAS.VALUES, COMDET.MEAS.MOD.UNW.NEG)
	COMDET.MEAS.NAMES[COMDET.MEAS.MOD.UNW.NEG] <- "Unweighted negative modularity"
	COMDET.MEAS.BOUNDS[[COMDET.MEAS.MOD.UNW.NEG]] <- function(g) c(0,1)
	COMDET.MEAS.MOD.UNW.POS <- "upM"
	COMDET.MEAS.VALUES <- c(COMDET.MEAS.VALUES, COMDET.MEAS.MOD.UNW.POS)
	COMDET.MEAS.NAMES[COMDET.MEAS.MOD.UNW.POS] <- "Unweighted positive modularity"
	COMDET.MEAS.BOUNDS[[COMDET.MEAS.MOD.UNW.POS]] <- function(g) c(0,1)
	COMDET.MEAS.MOD.WGT.NEG <- "wnM"
	COMDET.MEAS.VALUES <- c(COMDET.MEAS.VALUES, COMDET.MEAS.MOD.WGT.NEG)
	COMDET.MEAS.NAMES[COMDET.MEAS.MOD.WGT.NEG] <- "Weighted negative modularity"
	COMDET.MEAS.BOUNDS[[COMDET.MEAS.MOD.WGT.NEG]] <- function(g) c(0,1)
	COMDET.MEAS.MOD.WGT.POS <- "wpM"
	COMDET.MEAS.VALUES <- c(COMDET.MEAS.VALUES, COMDET.MEAS.MOD.WGT.POS)
	COMDET.MEAS.NAMES[COMDET.MEAS.MOD.WGT.POS] <- "Weighted positive modularity"
	COMDET.MEAS.BOUNDS[[COMDET.MEAS.MOD.WGT.POS]] <- function(g) c(0,1)
PART.MEAS.VALUES <- c(PART.MEAS.VALUES,COMDET.MEAS.VALUES)
PART.MEAS.NAMES <- c(PART.MEAS.NAMES,COMDET.MEAS.NAMES)
PART.MEAS.BOUNDS <- c(PART.MEAS.BOUNDS,COMDET.MEAS.BOUNDS)


#############################################################################################
# Unsigned networks partitioning algorithms info
#############################################################################################
COMDET.ALGO.VALUES <- c()
COMDET.ALGO.NAMES <- c()
COMDET.ALGO.EDGEBETW <- "EB"
	# this implementation will use the weights and directions, if present
	COMDET.ALGO.VALUES <- c(COMDET.ALGO.VALUES, COMDET.ALGO.EDGEBETW)
	COMDET.ALGO.NAMES[COMDET.ALGO.EDGEBETW] <- "EdgeBetweenness"
COMDET.ALGO.INFOMAP <- "IM"
	# this implementation will use the weights and directions, if present
	COMDET.ALGO.VALUES <- c(COMDET.ALGO.VALUES, COMDET.ALGO.INFOMAP)
	COMDET.ALGO.NAMES[COMDET.ALGO.INFOMAP] <- "InfoMap"
COMDET.ALGO.LABELPROP <- "LP"
	# this implementation will use the weights and directions, if present
	COMDET.ALGO.VALUES <- c(COMDET.ALGO.VALUES, COMDET.ALGO.LABELPROP)
	COMDET.ALGO.NAMES[COMDET.ALGO.LABELPROP] <- "LabelPropagation"
COMDET.ALGO.LOUVAIN <- "LV"
	# this implementation will use the weights, if present, but cannot use directions
	COMDET.ALGO.VALUES <- c(COMDET.ALGO.VALUES, COMDET.ALGO.LOUVAIN)
	COMDET.ALGO.NAMES[COMDET.ALGO.LOUVAIN] <- "Louvain"
COMDET.ALGO.WALKTRAP <- "WT"
	# this implementation will use the weights, if present, and simply ignores directions
	COMDET.ALGO.VALUES <- c(COMDET.ALGO.VALUES, COMDET.ALGO.WALKTRAP)
	COMDET.ALGO.NAMES[COMDET.ALGO.WALKTRAP] <- "WalkTrap"
comdet.algo.ncg.value <- function(value) paste("NCG",value,sep="-")	# returns the negative complementary value (i.e. short code) associated to the specified (positive) value
for(value in COMDET.ALGO.VALUES) COMDET.ALGO.NAMES[comdet.algo.ncg.value(value)] <- paste("NCG",COMDET.ALGO.NAMES[value])
PART.ALGO.VALUES <- c(COMDET.ALGO.VALUES) # TODO not really useful (is it?)
PART.ALGO.NAMES <- c(COMDET.ALGO.NAMES)


#############################################################################################
# Signed networks partitioning algorithms info
#############################################################################################
CORCLU.ALGO.VALUES <- c()
CORCLU.ALGO.NAMES <- c()
CORCLU.ALGO.PILS <- "PI"
	CORCLU.ALGO.VALUES <- c(CORCLU.ALGO.VALUES, CORCLU.ALGO.PILS)
	CORCLU.ALGO.NAMES[CORCLU.ALGO.PILS] <- "pILS"
PART.ALGO.VALUES <- c(PART.ALGO.VALUES,CORCLU.ALGO.VALUES)
PART.ALGO.NAMES <- c(PART.ALGO.NAMES,CORCLU.ALGO.NAMES)
