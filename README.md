
NetVotes-iKnow
==================

* Copyright 2017 Arinik Nejat

NetVotes-iKnow is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation. For source availability and license information see `licence.txt`

* Contact: Nejat ARINIK  <nejat.arinik@univ-avignon.fr>

-----------------------------------------------------------------------

# Description
This branch *iknow* is based on (and is complementary to) the branch *master*:
https://github.com/CompNet/NetVotes. Our tool was applied to data representing the activity of the members of the European Parliament (MEPs) during the 7th term (from June 2009 to  June 2014), as described in *[iKNOW'17]*. The raw data describing this  activity were retrieved from the [It's Your Parliament](http://www.itsyourparliament.eu/) website. There were some minor issues with these data, which we had to  correct: some MEPs were represented twice, some profiles were  incomplete, the policy domains were not defined for all vote texts, etc. These cleaned data, as well as the obtained partition results and plots, are available on [FigShare](https://doi.org/10.6084/m9.figshare.5785833).

This set of R scripts is designed to process some signed or unsigned graph algorithms onto input graphs, plot their graph results, and perform some statistical plots.


# Data
Data representing the activity of the members of the European Parliament (MEPs) during the 7th term (from June 2009 to June 2014), as described in *[iKNOW'17]*.


# Organization
Here are the folders composing the project:
* Folder `src`: contains the source code (R scripts) to process signed graphs and plot their graph results.
* Folder `data`: contains the input networks and the subfolder `overall` containing MEP details. Currently, we provide the input networks for only *Austria*. All input networks can be generated through the code of the branch `master`. However, this will take some times. You can download the input networks for *France* and *Italy* on [FigShare](https://doi.org/10.6084/m9.figshare.5785833). 
* Folder `exe`: contains executable files for *ILS-CC* (see *[MRYL'17]*), *ILS-RCC* (see *[MRYL'17]*), *ExCC* (see *[AFL'19]*) and *kMBS* (see *[FF'14]*).
* Folder `stats`: contains the other source code (R scripts) to plot some statistics. Note that it requires user to obtain the results for both unfiltered and filtered graphs.
* Folder `out`: contains the produced files (by `src/run-netvotes.R`).


# Installation
1. Install the [`R` language](https://www.r-project.org/)
2. Install the following R packages:
   * XML
     * in terminal, type: `sudo apt-get install libxml2-dev`
     * in R, type: `install.packages("XML")`
   * [`igraph`](http://igraph.org/r/): required (tested with version 1.0.1).
     * in terminal, type: sudo apt-get install libblas-dev liblapack-dev gfortran
     * type in R: install.packages("irlba")
     * type in R: `install.packages("igraph")`
   * `iterators`
   * `foreach`
   * `doParallel`
   * `sourcetools`
   * `ggplot2` (if an error occurs like ‘ggplot2 is not available’, type in terminal ‘sudo apt-get install r-cran-ggplot2’)
   * `treemap`
3. Install `IBM Cplex 12.7.1`
   * for ubuntu, type the following command:
     * `sudo ./cplex_studio12.7.1.linux-x86-64.bin` 
       * the default installation location for education version is: `/opt/ibm/ILOG/CPLEX_Studio1271` 
       * the default installation location for trial version is:  `/opt/ibm/ILOG/CPLEX_Studio_Community127/cplex/bin/x86-64_linux/`
       * If you use trial version or want to change the default installation folder, update the corresponding variable `CPLEX.BIN.PATH` located in `define-consts.R`. Otherwise, no need to update it
4. Install Open Java 1.8 or Oracle Java 8
5. Install [`Circos`](http://circos.ca/). Tested with the version 0.69.6. Set correctly the variable `CIRCOS_CMD` in `circos.R` (e.g. `/opt/circos-0.69-6/bin/circos`).
6. Install `OpenMPI` for C++. Export `LD_LIBRARY_PATH` variable to the path where openmpi shared objects are located. For example, in my case: `export LD_LIBRARY_PATH=/usr/lib/openmpi/lib:$LD_LIBRARY_PATH`  (if `LD_LIBRARY_PATH` is not set correctly, you will get ‘error when loading shared object ...’)
   * in terminal: sudo apt-get install gcc g++ openmpi-bin openmpi-doc libopenmpi-dev 
   * check openmpi by typing in terminal: which mpicc
   * configure openmpi environment variables in your home by adding these 2 lines at the end of the `~/.bashrc` file (change the 2 paths below depending on your installation):
      * export PATH=/usr/include/openmpi/:$PATH
      * export LD_LIBRARY_PATH=/usr/lib/openmpi/lib:$LD_LIBRARY_PATH
    * type in terminal: source ~/.bashrc
7. (I am not sure if it is really needed) Install [`Boost C++ library`](http://www.boost.org/) 
   * download the boost c++ library
   * decompress it on "tempdir"
   * run on the "tempdir": ./boostrap.sh
   * once the configuration succeds edit the file "project-config.jam" and add the "using mpi ;" at the end.
   * run in terminal: sudo ./b2 --target=shared,static --with=all -j2 install
   * run in terminal: sudo ldconfig
   * check if mpi .so files are installed correctly
      * run: ls /usr/local/lib
      * check if the files "libbost_mpi.a", "libbost_mpi.so" are there
8. Install `numpy` and `scipy` for python
9. Download this project from GitHub and unzip the archive.
10. Go to the current project directory
11. In terminal: `chmod a+x exe/grapspcc` and `chmod a+x exe/kmbs`
12. configure `src/run-netvotes.R` file.
13. configure  `LIBRARY.LOC.PATH` in `src/run-netvotes.R` and `stats/real-instances/main.R`
    Initially, `LIBRARY.LOC.PATH = .libPaths()`. But, you can configure it (especially, if you downloaded all R libs in a specific dir). What is easier is to update your .libPaths() in `/etc/R/Rprofile.site` (i.e. I added my personal local lib dir by doing: .libPaths(c("~/R/R-library", .libPaths()))). Hence, I do not need to update the ".libPaths()" each R session
14. configure/update `CPLEX.BIN.PATH`  in `src/define-consts.R`
15. configure `MAX.G.SIZE` located in `stats/define-consts.R`. I set to 150 because when graph-size increases (especially after 250), the execution of ExCC is getting slower.
16. In case of you encounter a problem with *ExCC*, you can try to get the source code and compile it. Download the project of `ExCC` from [github](https://github.com/arinik9/ExCC). First, configure and then compile it. To test it, you can run the file `run.sh`.If everything works (i.e. if a file `ExCC-result.txt` created in the output folder), move the exectutable file `cplex-partition.jar`, which is in `exe`, into the folder `lib` in this project.



# Use
In order to replicate the experiments from the article, perform the following operations:

1. Open the `R` console.
2. Set the current project directory as the working directory, using `setwd("my/path/to/the/project/NetVotes")`.
3. Run `src/run-netvotes.R` (you might first check the boolean variables)
4. Run `stats/main.R`


# Info
* The variables we are using are global variables (`src/run-netvotes.R`, `src/define-consts.R`, ...). They will be used throughout the code.
* When comparing the results of two algorithms, we omit the information of isolated nodes.
* in `prepare.grasp.ils.parameter.set` is changed depending on graphs size, as explained in section 4 of *[MRYL'17]*
* handling membership/nb.cluster variables for isolated nodes: we put all isolated nodes in the same cluster in the 'membership' variable. Their cluster no is `CLU.NO.FOR.ALL.ISOLATED.NODES` ==> in plots, they are visualized in white color.
* To run correctly the scirpt in `stats/main.R`: it is required to provide it with the results for both the filtered and unfiltered graphs. Otherwise, there is less interest to run the script (also, there will be a bug).
* ExCC’yi parallel calistirirken bug oluyo. Muhtemelen memory cok gerektiren bi algo eger graph-size yuksekse. O yuzden o algo’yu sequential modda calistir
* When plotting networks via *igraph*, I wanted to use two specific information as node labels (*node-id-enabled* and *imb-edge-cont*), but it was not possible. You may use the property `vertex frame` to handle it. In any case, *imb-edge-cont* is not used in the code 


# To-do List
* Add more imbalance calculations ? `Total.imbalance`, `neg.imbalance` ve `pos.imbalance`
* implement vertex imbalance contributions from membership file. It will be the same task that Mario has done. But, we will be able to use it for algorithms if we implement it in R. Also, I do not understand what Mario's code for this task. The result might be wrong in some cases.

# References

* **[iKNOW'17]** N. Arinik, R. Figueiredo, and V. Labatut. “Signed Graph Analysis for the Interpretation of Voting Behavior”. International Conference on Knowledge Technologies and Data-driven Business - International Workshop on Social Network Analysis and Digital Humanities. Graz, AT, 2017. [URL](:http://ceur-ws.org/Vol-2025/paper%5C_rssna%5C_1.pdf)
* **[MRYL'17]** Mario Levorato, Rosa Figueiredo, Yuri Frota & Lúcia Drummond. Evaluating balancing on social networks through the efficient solution of correlation clustering problems, 2017
* **[FF'14]** Figueiredo, R & Frota, Y. The maximum balanced subgraph of a signed graph: Applications and solution approaches. European Journal of Operational Research 236(2): 473-487 (2014)

- **[AFL'19]** N. Arinik, R. Figueiredo & V. Labatut. *Multiple Partitioning of Multiplex Signed Networks: Application to European Parliament Votes.Social Networks*, 2019. [doi: 10.1016/j.socnet.2019.02.001](https://doi.org/10.1016/j.socnet.2019.02.001) - [⟨hal-02082574⟩](https://hal.archives-ouvertes.fr/hal-02082574)