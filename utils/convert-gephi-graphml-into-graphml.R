# TODO: Add comment
# 
# Author: nejat
###############################################################################

source("src/define-graphml-file-operations.R")


file.path = "tests/signed-gephi-circular.graphml"
new.file.path = "tests/signed-circular-layout.graphml"
newGraphFile = removeSignAttrFromGraphmlFile(file.path)
saveXML(newGraphFile, file=new.file.path)

print("done")