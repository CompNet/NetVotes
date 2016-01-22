NetVotes v.2
=======
*Extraction and analysis of vote-based networks*

* Copyright 2015 Israel Mendonça (v1) & Vincent Labatut (v2). 

NetVotes is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation. For source availability and license information see `licence.txt`

* Lab site: http://lia.univ-avignon.fr/
* GitHub repo: https://github.com/CompNet/nerwip
* Contact: Vincent Labatut <vincent.labatut@univ-avignon.fr> and Rosa Figueiredo <rosa.figueiredo@univ-avignon.fr>

**Note:** this is a development version. Please, use the last available release instead.

-----------------------------------------------------------------------

# Description
This set of R scripts was designed for two purposes:

1. Extract so-called *vote networks* from raw data describing the voting activity of a population.
2. Perform various analyses on these data, in particular: estimate good partitions of the network, according to different measures.

Our tool was applied to data representing the activity of the members of the European Parliament (MEPs) during the 7th term (from June 2009 to June 2014), as described in [MFLM'15]. The raw data describing this activity comes from the [VoteWatch](http://www.votewatch.eu/) website.


# Organization
Here are the folders composing the project:
* Folder `src`: contains the source code (R scripts).
* Folder `in`: contains the files used by our scripts, i.e. the inputs.
  * Folder `pils`: results of the correlation clustering method (or any other graph partitioning method), for each considered parameter set (year, policy, etc). 
  * Folder `raw`: the raw data extracted from the VoteWatch website.
    * Folder `aggregated`: this folder contains several CSV files build from the original data:
      * `all-votes.csv`: concatenation of all vote outcomes for all documents and all MEPS. Can be considered as a compact representation of the data contained in the folder `votes_by_document`.
      * `mep-details.csv`: list of the MEPs having voted at least once in the considered term, with their details.
      * `mep-loyalty.csv`: same thing than `allvotes.csv`, but for the loyalty (i.e. whether or not the MEP voted like the majority of the MEPs in his political group).
      * `policy-freq.csv`: list of the topics considered during the term, with the corresponding number of documents.
      * `vote-details.csv`: list of the voted texts with their details.
    * `original`: this folder contains a collection of CSV files, each one describing the outcome of the vote session relatively to one specific document.
* Folder `out`: contains the file produced by our scripts. See the *Use* section for more details.


# Installation
1. Install the [`R` language](https://www.r-project.org/)
2. Install the following R packages:
   * [`igraph`](http://igraph.org/r/) v.0.7.1 (might not work with later versions, especially 1+)
   * [`plotrix`](https://cran.r-project.org/web/packages/plotrix/index.html)
   * [`ggplot2`](https://cran.r-project.org/web/packages/ggplot2/index.html)
3. Download this project from GitHub.

In order to reproduce the process from the papers, you also need to retrieve the data from Figshare. Just download and unzip the archive so that its folders match the project root. You will then have both the input raw data and the outputs resulting from the process. Apply the scripts again should therefore lead to the same results.
  
%This project contains the raw data for the 7th term MEPs activity, so it can be used as is. You can alternatively apply the scripts to other data, provided they follow the same structure and format. (not true anymore) 


# Use
In order to extract the networks from the raw data, compute certain statistics, apply the community detection methods, process their performance and generate various plots:

1. Open the `R` console.
2. Set the current directory as the working directory, using `setwd("<my directory>")`.
3. Run the main script `code/main.R`.

The script will produce the following files in the folder `output_files`:
* `agreement`: histograms representing the distributions of agreement and rebellion indices. Each subfolder corresponds to a specific topic.
* `community_algorithms_csv`: Performances obtained by the partitioning algorithms (for both community detection and correlation clustering). Each subfolder corresponds to a specific topic.
  * `xxxx_cluster_information.csv`: table containing several variants of the imbalance measure, for the considered algorithms.
* `community_algorithms_results`: Comparison of the partitions detected by the various algorithms considered, and distribution of the cluster/community sizes. Each subfolder corresponds to a specific topic.
  * `xxxx_cluster_comparison.csv`: table comparing the partitions detected by the community detection algorithms, in terms of Rand index and other measures.
  * `xxxx_ils_cluster_comparison.csv`: like `xxxx_cluster_comparison.csv`, except we compare the partition of community detection algorithms with that of the ILS.
  * `xxxx_yyyy_distribution.pdf`: histogram of the community (or cluster) sizes detected by algorithm `yyyy`.
* `graphs`: the networks extracted from the vote data. Each subfolder corresponds to a specific topic.
  * `xxxx_complete_graph.graphml`: network at the `Graphml` format, with all the information: nodes, edges, nodal attributes (including communities), weights, etc. 
  * `xxxx_edges_Gephi.csv`: only the links, with their weights (i.e. vote similarity). 
  * `xxxx_graph.g`: network at the `g` format (for ILS). 
  * `xxxx_net_measures.csv`: table containing some stats on the network (number of links, etc.).
  * `xxxx_nodes_Gephi.csv`: list of nodes (i.e. MEPs), with details.
  * `xxxx_infomap_community.txt`: membership vector generated by the application of the InfoMap algorithm.
  * `xxxx_multilevel_community.txt`: membership vector generated by the application of the Multi-Level algorithm.
  * `xxxx_fastgreedy_community.txt`: membership vector generated by the application of the FastGreedy algorithm.
  * `xxxx_walktrap_community.txt`: membership vector generated by the application of the Walktrap algorithm.

Note the ILS-related files are generated provided the ILS results have been placed in the `parallel_ils_results` folder.

In order to generate the plots from the paper, you additionally need to run the `code/chartsProcessor.R` script. The files are stored in the `output_files/plots` folder.


# Extension
You may want to apply the scripts to other raw data: this is possible, provided the they take the same form. For other VoteWatch data (e.g. a different term), this should not be a problem.
For different data, you may have to adapt the functions used to load the tabular data, in particular the ones using column names to identify the relevant information (see `src/`prepare-rawdata.R` and `src/filter-rawdata.R`). 


# Dependencies
* [`igraph`](http://igraph.org/r/) package: used to build and handle graphs.


# To-do List
* Use `ggplot also for the line plots.


# References
* **[MFLM'15]** Mendonça, I.; Figueiredo, R.; Labatut, V. & Michelon, P. Relevance of Negative Links in Graph Partitioning: A Case Study Using Votes From the European Parliament, 2nd European Network Intelligence Conference (ENIC), 2015.
http://arxiv.org/abs/1507.04215
