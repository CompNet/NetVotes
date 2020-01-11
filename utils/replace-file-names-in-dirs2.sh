#/bin/bash

#for domainDir in "AGRI" "ECON" "FEMM"
for domainDir in "FEMM"
do
  cd $domainDir
  echo "located in "$domainDir

  for yearDir in "2009-10" "2010-11" "2011-12" "2012-13" "2013-14"
  do 
    cd $yearDir
    echo "located in "$yearDir
    dirs=`ls | grep "seq*"`

    #kmbsAndIlsDirs=`{ ls -d kmbs*/ && ls -d seq_ils*/; }` # concat 2 outputs
    treemapFilesInKmbsAndIlsDirs=`ls seq_ils*/ils-treemap-rotated-circ-lyt-with-lgnd.pdf` # concat 2 outputs

    while IFS=' ' read -ra ADDR
    do 
      for i in "${ADDR[@]}" # receiving inputs below (line 52):  <<< "$dirs"
      do 
        #echo $i
        IFS='/' read -ra ADDR2 <<< "$i"
        p1=${ADDR2[0]}
        p2=${ADDR2[1]}
        delim="/"
	newName="ils-treemap.pdf"
	#echo $newstring
        #echo $p1
        #echo $p2
	old=$p1$delim$p2
	#echo $old
	new=$p1$delim$newName
	#echo $new
        mv $old $new
      done
    done <<< "$treemapFilesInKmbsAndIlsDirs"

    cd ..
    echo "back to main period dir"
  done
  
  cd ..
  echo "back to main domain dir"
done

