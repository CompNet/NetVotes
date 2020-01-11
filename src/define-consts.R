# TODO: Add comment
# 
# Author: nejat
###############################################################################

EXE.DIR = "exe"
DATA.DIR = "data"
OUT.DIR = "out"
VOTE.AGREEMENT.TYPE = "m3" # there is actually only one choice


EUROPARL.BEGIN.PERIOD.DATE.WITHOUT.YEAR = "01/07/"
EUROPARL.BEGIN.DATE = paste(EUROPARL.BEGIN.PERIOD.DATE.WITHOUT.YEAR,"2009",sep="")
EUROPARL.END.DATE = paste(EUROPARL.BEGIN.PERIOD.DATE.WITHOUT.YEAR,"2014",sep="")
EUROPARL.PERIOD.LENGTH.AS.DAYS = 365


# Algo names
CORCLU.ILS = "ILS"
ILSRCC = "rcc" # desc from Mario's code
ILSCC = "cc" # desc from Mario's code
CORCLU.GRASP = "GRASP"
KMBS = "KMBS"
ExCC = "ExCC"
COMDET.INFOMAP = "IM"


##########################################################################################
## COMMON
##########################################################################################

SIGNED.GRAPH.FILENAME = "signed.G"
SIGNED.UNWEIGHTED.GRAPH.FILENAME = "signed-unweighted.G"
GRAPHML.NETWORK.FILENAME = "signed.graphml"
GRAPHML.GEPHI.NETWORK.FILENAME = "signed-gephi.graphml"
CLU.NO.FOR.ALL.ISOLATED.NODES = 0
CLUSTER.GRAPH.FILENAME = "cluster-graph.pdf"
EXEC.TIME.FILENAME = "exec-time.txt"



NO.PREDEF.NB.CLUSTER = 0 # to use in ilscc


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
COL.EP.ID 		<- "EP Id"			# official EP id of the MEP
COL.FIRSTNAME	<- "Firstname"
COL.FREQUENCY	<- "Frequency"
COL.FULLNAME	<- "Full name"
COL.GENDER		<- "Gender"
COL.GROUP		<- "Group"
COL.LASTNAME	<- "Lastname"
COL.MEPID		<- "MEP Id"			# EP id of the MEP, internal to NetVotes
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
COL.EP.URL		<- "EP URL"
COL.PERIODS		<- "Periods"
COL.RET.TITLE	<- "Retrieved title"



##########################################################################################
## Ex-CC
##########################################################################################

EXCC.RESULT.FILENAME = paste(ExCC,"-result.txt",sep="")
ExCC.JAR.PATH = paste(EXE.DIR,"cplex-partition.jar",sep="/")
# where the .so files are located
CPLEX.BIN.PATH = "/opt/ibm/ILOG/CPLEX_Studio128/cplex/bin/x86-64_linux/" #  path for Nejat's computer


##########################################################################################
## ILS-GRASP
##########################################################################################

GRASP.ILS.EXECUTABLE.PATH = paste(EXE.DIR, "/graspcc", sep="")
PYTHON.RESULT.INTERPRETER.EXECUTABLE.PATH = 
		paste("python ", EXE.DIR, "/process-graspcc-output.py", sep="")

RCC.RESULT.FILENAME = paste(ILSRCC,"-result.txt",sep="")
CC.RESULT.FILENAME = paste(ILSCC,"-result.txt",sep="")

IS.PARALLEL.VERSION = FALSE
PREFIX.PAR.VERS = "par"
PREFIX.SEQ.VERS = "seq"

# parameters
ALPHA.DEFAULT = 0.4
PERTURBATION.DEFAULT = 3
TIME.LIMIT.DEFAULT = 3600 # 3600
L.DEFAULT = 1
ITER.DEFAULT = 10
STRATEGY.DEFAULT = CORCLU.ILS
GAIN.FUNC.DEFAULT = 0


##########################################################################################
## INFOMAP
##########################################################################################

INFOMAP.MEM.FILENAME = paste(COMDET.INFOMAP,"-membership.txt",sep="")


##########################################################################################
## KMBS
##########################################################################################

PARTITION.NO.FOR.REMOVED.NODES.BY.KMBS = -1
KMBS.RESULT.FILENAME = paste(KMBS,"-result.txt",sep="")
KMBS.EXECUTABLE.PATH = paste(EXE.DIR, "kmbs", sep="/")


##########################################################################################
## PLOT CONFIG
##########################################################################################

# # TODO: STATE.LIST is the same as TARGET.STATES in src/main.R
# 
# STATE.LIST =
# 		c(
# 				"Austria","Belgium","Bulgaria","Croatia","Cyprus","Czech Republic",
# 				"Denmark","Estonia","Finland","France","Germany","Greece","Hungary",
# 				"Ireland","Italy","Latvia","Lithuania","Luxembourg","Malta",
# 				"Netherlands","Poland","Portugal","Romania","Slovakia","Slovenia",
# 				"Spain","Sweden","United Kingdom"
# 		)



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
GROUP.FULLNAMES[GROUP.GREENS] <- "The Greens-European Free Alliance"
GROUP.GUENGL <- "GUENGL"
GROUP.VALUES <- c(GROUP.VALUES, GROUP.GUENGL)
GROUP.FULLNAMES[GROUP.GUENGL] <- "European United Left-Nordic Green Left"
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
COUNTRY.SHORTNAMES <- c()
COUNTRY.AT <- "Austria"
    COUNTRY.VALUES <- c(COUNTRY.VALUES, COUNTRY.AT)
    COUNTRY.SHORTNAMES[COUNTRY.AT] <- "AT"
COUNTRY.BE <- "Belgium"
    COUNTRY.VALUES <- c(COUNTRY.VALUES, COUNTRY.BE)
    COUNTRY.SHORTNAMES[COUNTRY.BE] <- "BE"
COUNTRY.BG <- "Bulgaria"
    COUNTRY.VALUES <- c(COUNTRY.VALUES, COUNTRY.BG)
    COUNTRY.SHORTNAMES[COUNTRY.BG] <- "BG"
COUNTRY.HR <- "Croatia"
    COUNTRY.VALUES <- c(COUNTRY.VALUES, COUNTRY.HR)
    COUNTRY.SHORTNAMES[COUNTRY.HR] <- "HR"
COUNTRY.CY <- "Cyprus"
    COUNTRY.VALUES <- c(COUNTRY.VALUES, COUNTRY.CY)
    COUNTRY.SHORTNAMES[COUNTRY.CY] <- "CY"
COUNTRY.CZ <- "Czech Republic"
    COUNTRY.VALUES <- c(COUNTRY.VALUES, COUNTRY.CZ)
    COUNTRY.SHORTNAMES[COUNTRY.CZ] <- "CZ"
COUNTRY.DK <- "Denmark"
    COUNTRY.VALUES <- c(COUNTRY.VALUES, COUNTRY.DK)
    COUNTRY.SHORTNAMES[COUNTRY.DK] <- "DK"
COUNTRY.EE <- "Estonia"
    COUNTRY.VALUES <- c(COUNTRY.VALUES, COUNTRY.EE)
    COUNTRY.SHORTNAMES[COUNTRY.EE] <- "EE"
COUNTRY.FI <- "Finland"
    COUNTRY.VALUES <- c(COUNTRY.VALUES, COUNTRY.FI)
    COUNTRY.SHORTNAMES[COUNTRY.FI] <- "FI"
COUNTRY.FR <- "France"
    COUNTRY.VALUES <- c(COUNTRY.VALUES, COUNTRY.FR)
    COUNTRY.SHORTNAMES[COUNTRY.FR] <- "FR"
COUNTRY.DE <- "Germany"
    COUNTRY.VALUES <- c(COUNTRY.VALUES, COUNTRY.DE)
    COUNTRY.SHORTNAMES[COUNTRY.DE] <- "DE"
COUNTRY.GR <- "Greece"
    COUNTRY.VALUES <- c(COUNTRY.VALUES, COUNTRY.GR)
    COUNTRY.SHORTNAMES[COUNTRY.GR] <- "GR"
COUNTRY.HU <- "Hungary"
    COUNTRY.VALUES <- c(COUNTRY.VALUES, COUNTRY.HU)
    COUNTRY.SHORTNAMES[COUNTRY.HU] <- "HU"
COUNTRY.IE <- "Ireland"
    COUNTRY.VALUES <- c(COUNTRY.VALUES, COUNTRY.IE)
    COUNTRY.SHORTNAMES[COUNTRY.IE] <- "IE"
COUNTRY.IT <- "Italy"
    COUNTRY.VALUES <- c(COUNTRY.VALUES, COUNTRY.IT)
    COUNTRY.SHORTNAMES[COUNTRY.IT] <- "IT"
COUNTRY.LV <- "Latvia"
    COUNTRY.VALUES <- c(COUNTRY.VALUES, COUNTRY.LV)
    COUNTRY.SHORTNAMES[COUNTRY.LV] <- "LV"
COUNTRY.LT <- "Lithuania"
    COUNTRY.VALUES <- c(COUNTRY.VALUES, COUNTRY.LT)
    COUNTRY.SHORTNAMES[COUNTRY.LT] <- "LT"
COUNTRY.LU <- "Luxembourg"
    COUNTRY.VALUES <- c(COUNTRY.VALUES, COUNTRY.LU)
    COUNTRY.SHORTNAMES[COUNTRY.LU] <- "LU"
COUNTRY.MT <- "Malta"
    COUNTRY.VALUES <- c(COUNTRY.VALUES, COUNTRY.MT)
    COUNTRY.SHORTNAMES[COUNTRY.MT] <- "MT"
COUNTRY.NL <- "Netherlands"
    COUNTRY.VALUES <- c(COUNTRY.VALUES, COUNTRY.NL)
    COUNTRY.SHORTNAMES[COUNTRY.NL] <- "NL"
COUNTRY.PL <- "Poland"
    COUNTRY.VALUES <- c(COUNTRY.VALUES, COUNTRY.PL)
    COUNTRY.SHORTNAMES[COUNTRY.PL] <- "PL"
COUNTRY.PT <- "Portugal"
    COUNTRY.VALUES <- c(COUNTRY.VALUES, COUNTRY.PT)
    COUNTRY.SHORTNAMES[COUNTRY.PT] <- "PT"
COUNTRY.RO <- "Romania"
    COUNTRY.VALUES <- c(COUNTRY.VALUES, COUNTRY.RO)
    COUNTRY.SHORTNAMES[COUNTRY.RO] <- "RO"
COUNTRY.SK <- "Slovakia"
    COUNTRY.VALUES <- c(COUNTRY.VALUES, COUNTRY.SK)
    COUNTRY.SHORTNAMES[COUNTRY.SK] <- "SK"
COUNTRY.SI <- "Slovenia"
    COUNTRY.VALUES <- c(COUNTRY.VALUES, COUNTRY.SI)
    COUNTRY.SHORTNAMES[COUNTRY.SI] <- "SI"
COUNTRY.ES <- "Spain"
    COUNTRY.VALUES <- c(COUNTRY.VALUES, COUNTRY.ES)
    COUNTRY.SHORTNAMES[COUNTRY.ES] <- "ES"
COUNTRY.SE <- "Sweden"
    COUNTRY.VALUES <- c(COUNTRY.VALUES, COUNTRY.SE)
    COUNTRY.SHORTNAMES[COUNTRY.SE] <- "SE"
COUNTRY.UK <- "United Kingdom"
    COUNTRY.VALUES <- c(COUNTRY.VALUES, COUNTRY.UK)
    COUNTRY.SHORTNAMES[COUNTRY.UK] <- "UK"

EU.REGION.FOR.STATE = list()
EU.REGION.FOR.STATE[[COUNTRY.AT]] = "Central"
EU.REGION.FOR.STATE[[COUNTRY.BE]] = "Western"
EU.REGION.FOR.STATE[[COUNTRY.BG]] = "Southeastern"
EU.REGION.FOR.STATE[[COUNTRY.HR]] = "Central"
EU.REGION.FOR.STATE[[COUNTRY.CY]] = "Southeastern"
EU.REGION.FOR.STATE[[COUNTRY.CZ]] = "Central"
EU.REGION.FOR.STATE[[COUNTRY.DK]] = "Northern"
EU.REGION.FOR.STATE[[COUNTRY.EE]] = "Central"
EU.REGION.FOR.STATE[[COUNTRY.FI]] = "Northern"
EU.REGION.FOR.STATE[[COUNTRY.FR]] = "Western"
EU.REGION.FOR.STATE[[COUNTRY.DE]] = "Central"
EU.REGION.FOR.STATE[[COUNTRY.GR]] = "Southeastern"
EU.REGION.FOR.STATE[[COUNTRY.HU]] = "Central"
EU.REGION.FOR.STATE[[COUNTRY.IE]] = "Western"
EU.REGION.FOR.STATE[[COUNTRY.IT]] = "Southern"
EU.REGION.FOR.STATE[[COUNTRY.LV]] = "Central"
EU.REGION.FOR.STATE[[COUNTRY.LT]] = "Central"
EU.REGION.FOR.STATE[[COUNTRY.LU]] = "Central"
EU.REGION.FOR.STATE[[COUNTRY.MT]] = "Southern"
EU.REGION.FOR.STATE[[COUNTRY.NL]] = "Western"
EU.REGION.FOR.STATE[[COUNTRY.PL]] = "Central"
EU.REGION.FOR.STATE[[COUNTRY.PT]] = "Southern"
EU.REGION.FOR.STATE[[COUNTRY.RO]] = "Southeastern"
EU.REGION.FOR.STATE[[COUNTRY.SK]] = "Central"
EU.REGION.FOR.STATE[[COUNTRY.SI]] = "Central"
EU.REGION.FOR.STATE[[COUNTRY.ES]] = "Southern"
EU.REGION.FOR.STATE[[COUNTRY.SE]] = "Northern"
EU.REGION.FOR.STATE[[COUNTRY.UK]] = "Western"





# 'check.names'=FALSE ensures that the column names will not be changed 
# (e.g. when there are spaces between 2 words)
MEP.DETAILS = 
		read.table(
				paste(DATA.DIR,"/overall/mep-details.csv",sep=""), 
				header=1, 
				sep=";", 
				check.names= FALSE
		)

# insert "region" information for each state:
# Northern europe:1, Central Europe:2, Western Europe:3, 
# Southern Europe:4, Southeastern Europe:5

MEP.DETAILS["Region"] = NA # initialize the "Region" column with NA
for(i in 1:length(COUNTRY.VALUES)){
	state = COUNTRY.VALUES[i]
	region = EU.REGION.FOR.STATE[[state]]
	indx = which(state == MEP.DETAILS[,"State"])
	
	#if(length(indx)>0)
	MEP.DETAILS[indx,"Region"] = region
}

POLITICAL.GROUP.LIST = sort(unique(MEP.DETAILS[,"Group"]))
REGION.LIST = sort(unique(MEP.DETAILS[,"Region"]))
COUNTRY.LIST = sort(unique(MEP.DETAILS[,"State"])) # not used
