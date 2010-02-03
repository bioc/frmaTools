getProbeMean <- function(x, nb){
  x <- matrix(x, ncol=nb, byrow=TRUE)
  rowMeans(x)
}

getProbeVar <- function(x, nb){
  x <- matrix(x, ncol=nb, byrow=TRUE)
  apply(x, 1, var)
}

getPsetMAD <- function(x, nc, batch.id){
  x <- matrix(x, ncol=nc)
  x.tmp <- split(x, batch.id)
  median(unlist(lapply(x.tmp, mad)))
}

#####

makeVectors <- function(object, batch.id, verbose=TRUE){
  if(class(object) != "AffyBatch") stop("object must be of class AffyBatch.")

  batch.size <- table(batch.id)[1]
  if(!all(table(batch.id)==batch.size)) stop("Batches must be of the same size.")

  if(verbose) message("Background Correcting ...\n")
  object <- bg.correct.rma(object)
  if(verbose) message("Normalizing ...\n")
  normVec <- normalize.quantiles.determine.target(pm(object))
  pm(object) <- normalize.quantiles.use.target(pm(object), normVec)
  if(verbose) message("Summarizing ...\n")

  object <- fitPLM(object, background=FALSE, normalize=FALSE, output.param=list(weights=FALSE, residuals=TRUE, varcov="none", resid.SE=FALSE))
  r <- residuals(object)[[1]]
  
  probeVec <- unlist(coefs.probe(object))
  names(probeVec) <- NULL
  
  tmp <- split(t(r), batch.id)
  withinMean <- lapply(tmp, getProbeMean, batch.size)
  withinVar <- lapply(tmp, getProbeVar, batch.size)
  
  withinAvgVar <- rowMeans(matrix(unlist(withinVar), ncol=length(withinVar)))
  btwVar <- apply(matrix(unlist(withinMean), ncol=length(withinMean)), 1, var)

  tmp <- split(r, rownames(r))
  psetMAD <- unlist(lapply(tmp, getPsetMAD, ncol(r), batch.id))
  
  return(list(normVec=normVec, probeVec=probeVec, probeVarWithin=withinAvgVar, probeVarBetween=btwVar, probesetSD=psetMAD))
}
