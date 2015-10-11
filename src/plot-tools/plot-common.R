#############################################################################################
# Function used to generate a plot containing lines, possibly several distinct series.
# Note: very old source code from the Ganetto project.
# 
# 10/2015 Vincent Labatut
#############################################################################################
library("plotrix")
library("ggplot2")



#############################################################################################
# Adds missing values to a list reprenting plot axes.
#
# axes: same as in the plot.lines function.
# returns: an updated axes list, with all necessary vectors and possibly NA values.
#############################################################################################
complete.axes <- function(axes)
{	if(is.null(axes$x))
		axes$x <- list()
	if(is.null(axes$y))
		axes$y <- list()
		
	if(is.null(axes$x$range))
		axes$x$range <- c(NA,NA)
	if(is.null(axes$y$range))
		axes$y$range <- c(NA,NA)
	
	return(axes)
}


#############################################################################################
# Performs the necessary transformations for numerical y values. Basically: replacing all
# NULL, NaN and INF symbols by NA symbols, to ease later processing. Applies to a list of
# vectors of values.
#
# series: same as in the plot.lines function.
# returns: a list containing updated variables.
#############################################################################################
clean.numerical.data <- function(series)
{	for(s in 1:length(series))
	{	serie <- series[[s]]
		serie$y <- clean.numerical.data.sec(serie$y)
		serie$stdev <- clean.numerical.data.sec(serie$stdev)
		series[[s]] <- serie
	}
	return(series)
}


#############################################################################################
# Performs the necessary transformations for numerical y values. Basically: replacing all
# NULL, NaN and INF symbols by NA symbols, to ease later processing. Applies to a single
# vector of values.
#
# values: vector of numerical values.
# returns: an updated vector.
#############################################################################################
clean.numerical.data.sec <- function(values)
{	values[is.null(values)] <- NA
	values[is.nan(values)] <- NA
	values[is.infinite(values)] <- NA
	return(values)
}



