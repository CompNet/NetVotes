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
    pdfFilesInKmbsAndIlsDirs=`{ ls kmbs*/*.pdf && ls seq_ils*/*.pdf; }` # concat 2 outputs

    while IFS=' ' read -ra ADDR
    do 
      for i in "${ADDR[@]}" # receiving inputs below (line 52):  <<< "$dirs"
      do 
        #echo $i
        IFS='.' read -ra ADDR2 <<< "$i"
        p1=${ADDR2[0]}
        p2=${ADDR2[1]}
        p3=${ADDR2[2]}
	p4=${ADDR2[3]}
        delim="."
	newstring="-rotated-circ-lyt-with-lgnd"
	#echo $newstring
        #echo $p1
        #echo $p2
        #echo $p3
	#echo $p4
       if [ $p3 = "pdf" ]
        then
          cmd=$p1$delim$p2$delim$p3 
          echo $cmd
          new=$p1$delim$p2$newstring$delim$p3
          echo $new
          mv $cmd $new
          echo "done"
        else 
          if [ $p4 = "pdf" ]
          then
            cmd=$p1$delim$p2$delim$p3$delim$p4
            echo $cmd
            new=$p1$delim$p2$delim$p3$newstring$delim$p4
            echo $new
            mv $cmd $new
            echo "done"
          fi
        fi
      done
    done <<< "$pdfFilesInKmbsAndIlsDirs"

    cd ..
    echo "back to main period dir"
  done
  
  cd ..
  echo "back to main domain dir"
done

