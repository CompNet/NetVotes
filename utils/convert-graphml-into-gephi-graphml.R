# TODO: Add comment
# 
# Author: nejat
###############################################################################

source("src/define-graphml-file-operations.R")


#file.path = "tests/signed-fr-agri-term.graphml"
file.path = "tests/signed.graphml"
new.file.path = "tests/signed-gephi.graphml"
newGraphFile = addSignAttrIntoGraphmlFiles(file.path)
saveXML(newGraphFile, file=new.file.path)

print("done")