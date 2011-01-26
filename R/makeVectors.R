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
  x.tmp <- split(t(x), batch.id)
  median(unlist(lapply(x.tmp, mad)))
}

#####

makeVectors <- function(object, batch.id, target="core", verbose=TRUE){
  if(!class(object) %in% c("AffyBatch", "ExonFeatureSet")) stop("object must be of class AffyBatch or ExonFeatureSet.")

  if(class(object)=="ExonFeatureSet") target <- match.arg(target, c("core", "full", "extended", "probeset"))
  
  batch.size <- table(batch.id)[1]
  if(!all(table(batch.id)==batch.size)) stop("Batches must be of the same size.")

  if(verbose & class(object)=="ExonFeatureSet") message(paste("Exon summarization at ", target, " level.\n", sep=""))
  
  if(verbose) message("Background Correcting ...\n")
  if(class(object)=="AffyBatch") object <- bg.correct.rma(object)
  if(class(object)=="ExonFeatureSet") object <- backgroundCorrect(object, verbose=FALSE)

  if(class(object)=="ExonFeatureSet"){
    if(target=="probeset"){
      featureInfo <- getFidProbeset(object)
    }
    if(target=="core"){
      featureInfo <- getFidMetaProbesetCore(object)
    }
    if(target=="full"){
      featureInfo <- getFidMetaProbesetFull(object)
    }
    if(target=="extended"){
      featureInfo <- getFidMetaProbesetExtended(object)
    }

    pmi <- featureInfo[["fid"]]
    pns <- as.character(featureInfo[["fsetid"]])
    pms <- exprs(object)[pmi,, drop=FALSE]
  }

  if(class(object)=="AffyBatch"){
    pms <- pm(object)
    pns <- probeNames(object)
  }
  
  if(verbose) message("Normalizing ...\n")
  normVec <- normalize.quantiles.determine.target(pms)
  pms <- normalize.quantiles.use.target(pms, normVec)
  
  if(verbose) message("Summarizing ...\n")
  pms <- log2(pms)
  N <- 1:dim(pms)[1]
  S <- split(N, pns)
  fit <- lapply(1:length(S), function(i) {
    s <- S[[i]]
    rcModelPLM(pms[s,, drop=FALSE])
  })
  names(fit) <- unique(pns)
  gc()	
  
  resids <- matrix(unlist(lapply(fit, function(x) t(x$Residuals))), ncol=ncol(pms), byrow=TRUE)
  
  medianSE <- unlist(lapply(fit, function(x) median(x$StdErrors[1:ncol(pms)])))
  names(medianSE) <- NULL
  
  probeVec <- unlist(lapply(fit, function(x) x$Estimates[(ncol(pms)+1):length(x$Estimates)]))
  names(probeVec) <- NULL
  
  tmp <- split(t(resids), batch.id)
  withinMean <- lapply(tmp, getProbeMean, batch.size)
  withinVar <- lapply(tmp, getProbeVar, batch.size)
  rm(tmp)
  gc()
  
  withinAvgVar <- rowMeans(matrix(unlist(withinVar), ncol=length(withinVar)))
  btwVar <- apply(matrix(unlist(withinMean), ncol=length(withinMean)), 1, var)
  rm(withinMean)
  rm(withinVar)
  gc()
  
  tmp <- split(resids, pns)
  psetMAD <- unlist(lapply(tmp, getPsetMAD, ncol(resids), batch.id))
  rm(tmp)
  gc()
  
  rm(resids)
  rm(pms)
  rm(pns)
  gc()

  return(list(normVec=normVec, probeVec=probeVec, probeVarWithin=withinAvgVar, probeVarBetween=btwVar, probesetSD=psetMAD, medianSE=medianSE))
}

