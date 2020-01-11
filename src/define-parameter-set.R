# TODO: Add comment
# 
# Author: nejat
###############################################################################



###############################################################################
# Priority queue
# source: http://rosettacode.org/wiki/Priority_queue#R
###############################################################################

PriorityQueue <- function() {
	keys <- values <- NULL

	# key = 1 --> so it will be the same for all keys: because I want user to insert just values
	insert <- function(value, key=1) {
		ord <- findInterval(key, keys)
		keys <<- append(keys, key, ord)
		values <<- append(values, value, ord)
	}
	pop <- function() {
		head <- list(key=keys[1],value=values[[1]])
		values <<- values[-1]
		keys <<- keys[-1]
		return(head)
	}
	empty <- function() length(keys) == 0
	environment()
}


########################################################################
#
########################################################################
insert.into.grasp.ils.parameter.set =
		function(queue, cc, rcc, a, p, t, k, k.from=NA,
				init.partition.enabled=0,
				nb.ils.iter=ITER.DEFAULT,
				ils.k.min.enabled
){
	# insert parameters in this order:
	# 1) st 2) is.par 3) cc 4) rcc 5) l 6) alpha 7) i 8) p 9) t 10) gf
	# 11) k 12) k.from 13) init.partition.from.infomap.enabled
	# 14) ils.k.min.enabled
	params = list()
	params$paramset =
			c(
				STRATEGY.DEFAULT,
				IS.PARALLEL.VERSION,
				cc,
				rcc,
				L.DEFAULT,
				a,
				nb.ils.iter,
				p,
				t,
				GAIN.FUNC.DEFAULT,
				k,
				k.from,
				init.partition.enabled,
				ils.k.min.enabled
			)

	queue$insert(value=params)
}


########################################################################
#
########################################################################
prepare.grasp.ils.parameter.set =
		function(g.size, k, k.from, init.partition.enabled, nb.ils.iter,
				ils.k.min.enabled)
{
	queue <- PriorityQueue()

	alpha = ALPHA.DEFAULT
	perturbation = PERTURBATION.DEFAULT
	time.limit = TIME.LIMIT.DEFAULT


	# ===========================================================================
	# Acording to the following academic reference:
	# "Evaluating balancing on social networks through the efficient solution of correlation clustering problems"
	# Mario Levorato, Rosa Figueiredo, Yuri Frota & Lúcia Drummond, 2017
	# look at table 1 in section 4.2

	# NOTE THAT I slighltly changed the threshold which was 300. Instead, I use 302.
	# Because, we have 1 exception (EPP - <any domain name> - TERM): there is 301 nodes.
	# in order to handle it, I use 302.
	if(g.size >= 302){
		alpha = 1
		perturbation = 30
		time.limit = 7200
	}
	# ===========================================================================

	if(k == 0){ # ils-cc
		insert.into.grasp.ils.parameter.set(
				queue,
				cc=1,
				rcc=0,
				a=alpha,
				p=perturbation,
				t=time.limit,
				k=k,
				k.from=k.from,
				init.partition.enabled=init.partition.enabled,
				nb.ils.iter=nb.ils.iter,
				ils.k.min.enabled
		)
	}
	else if(k >= 1){
		insert.into.grasp.ils.parameter.set(
				queue,
				cc=0,
				rcc=1,
				a=alpha,
				p=perturbation,
				t=time.limit,
				k=k,
				k.from=k.from,
				init.partition.enabled=init.partition.enabled,
				nb.ils.iter=nb.ils.iter,
				ils.k.min.enabled
		)
	}

	return(queue)
}
