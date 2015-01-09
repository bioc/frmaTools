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

rwaFit2 <- function(x1, x2, x3, x4){
  ncols <- ncol(x1)
  w.tmp <- x2/max(x2)
  w.tmp <- matrix(rep(w.tmp, ncols), ncol=ncols)
  pe.tmp <- x3
  pe.tmp[1] <- pe.tmp[1]-sum(pe.tmp)
  rcModelWPLM(y=x1, w=w.tmp, row.effects=pe.tmp, input.scale=x4)
}

#####

makeVectorsAffyBatch <- function(files, batch.id, background="rma", normalize="quantile", normVec=NULL, cdfname=NULL, file.dir=".", verbose=TRUE){
  wd <- getwd()
  setwd(file.dir)
  object <- ReadAffy(filenames=files, cdfname=cdfname, verbose=verbose)
  setwd(wd)

  if(verbose) message("Data loaded \n")
  
  batch.size <- table(batch.id)[1]
  if(!all(table(batch.id)==batch.size)) stop("Batches must be of the same size.")

  if(background=="rma"){
    object <- bg.correct.rma(object)
    if(verbose) message("Background Corrected \n")
    gc()
  }
  
  pms <- pm(object)
  pns <- probeNames(object)
  pmi <- unlist(pmindex(object))

  if(!identical(as.character(pmi),rownames(pms))) stop("Mismatch between pmindex and rownames of pms")

  rm(object)
  gc()

  if(normalize=="quantile"){
    if(is.null(normVec)) normVec <- normalize.quantiles.determine.target(pms)
    pms <- normalize.quantiles.use.target(pms, normVec)
    names(normVec) <- as.character(pmi)
    if(verbose) message("Normalized \n")
  }

  pms <- log2(pms)
  gc()
  
  N <- 1:dim(pms)[1]
  S <- split(N, pns)
  nc <- ncol(pms)
  nr <- nrow(pms)
  resids <- matrix(ncol=nc, nrow=nr)
  probeVec <- vector(length=nr)
  if(verbose) message("Beginning Probe Effect Calculation ... \n")
  for(k in 1:length(S)){
    fit <- rcModelPLM(pms[S[[k]],, drop=FALSE])
    resids[S[[k]],] <- fit$Residuals
    probeVec[S[[k]]] <- fit$Estimates[(nc+1):length(fit$Estimates)]
    if((k%%1000)==0){
      message(paste("Finished probeset:",k,"\n"))
      gc()
    }
  }
  names(probeVec) <- as.character(pmi)
  if(verbose) message("Probe Effects Calculated \n")
  gc()
  
  tmp <- split(t(resids), batch.id)
  withinMean <- lapply(tmp, getProbeMean, batch.size)
  withinVar <- lapply(tmp, getProbeVar, batch.size)
  withinAvgVar <- rowMeans(matrix(unlist(withinVar), ncol=length(withinVar)))
  btwVar <- apply(matrix(unlist(withinMean), ncol=length(withinMean)), 1, var)
  rm(tmp)
  rm(withinMean)
  rm(withinVar)
  names(withinAvgVar) <- names(btwVar) <- as.character(pmi)
  if(verbose) message("Probe Variances Calculated \n")
  gc()
  
  tmp <- split(resids, pns)
  psetMAD <- unlist(lapply(tmp, getPsetMAD, nc, batch.id))
  names(psetMAD) <- names(tmp)
  rm(tmp)
  rm(resids)
  if(verbose) message("Probe Set SDs Calculated \n")
  gc()

  w <- 1/(withinAvgVar + btwVar)
  w[w==Inf] <- 1
  medianSE <- vector(length=length(psetMAD))
  if(verbose) message("Beginning Median SE Calculation ... \n")
  for(k in 1:length(S)){
    fit <- rwaFit2(pms[S[[k]],, drop=FALSE], w[S[[k]]], probeVec[S[[k]]], psetMAD[k])
    medianSE[k] <- median(fit$StdErrors)
    if((k%%1000)==0){
      message(paste("Finished probeset:",k,"\n"))
      gc()
    }
  }
  names(medianSE) <- names(psetMAD)
  if(verbose) message("Median SEs Calculated \n")
  gc()

  rm(w)
  rm(pms)
  rm(pns)
  gc()

  vers <- ifelse(!is.null(cdfname), as.character(packageVersion(cdfname)), "")
  
  return(list(normVec=normVec, probeVec=probeVec,
              probeVarWithin=withinAvgVar, probeVarBetween=btwVar, 
              probesetSD=psetMAD, medianSE=medianSE,
              version=vers))
}



