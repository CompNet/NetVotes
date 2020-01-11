#/bin/bash

for domainDir in "AGRI" "ECON" "FEMM"
do
  cd $domainDir
  echo "located in "$domainDir

  for yearDir in "2009-10" "2010-11" "2011-12" "2012-13" "2013-14"
  do 
    cd $yearDir
    echo "located in "$yearDir
    dirs=`ls | grep "seq*"`

    #kmbsAndIlsDirs=`{ ls -d kmbs*/ && ls -d seq_ils*/; }` # concat 2 outputs
    filesToBeDeleted=`ls graph-kmbs*.pdf` # concat 2 outputs

    while IFS=' ' read -ra ADDR
    do 
      for i in "${ADDR[@]}" # receiving inputs below (line 52):  <<< "$dirs"
      do 
        #echo $i
        rm $i
      done
    done <<< "$filesToBeDeleted"

    cd ..
    echo "back to main period dir"
  done
  
  cd ..
  echo "back to main domain dir"
done

