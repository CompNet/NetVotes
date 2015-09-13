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

Our tool was applied to data representing the activity of the members of the European Parliament (MEPs) during the 7th term (from june 2009 to
june 2014), as described in [MFLM'15].

## Organization



# Installation
1. Install the [`R` language](https://www.r-project.org/) and the [`igraph`](http://igraph.org/r/) package.
2. Download this project from GitHub.

This project contains the raw data for the 7th term MEPs activity, so it can be used as is. You can alternatively apply the scripts to other data, provided they follow the same structure and format. 


# Use
1. Open the R console.
2. Set the current directory as the working directory, using `setwd("<my directory>")`.
3. Run the main script `main.R`.

The plots are stored in the output folder called `plots`. To generate them you just need to run the `chartsProcessor.R` script.


# Extension



# Dependencies


# References
* **[MFLM'15]** Mendonça, I.; Figueiredo, R.; Labatut, V. & Michelon, P. Relevance of Negative Links in Graph Partitioning: A Case Study Using Votes From the European Parliament, 2nd European Network Intelligence Conference (ENIC), 2015.
http://arxiv.org/abs/1507.04215
