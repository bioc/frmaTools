simpleFrma <- function(e, background=TRUE, normalize=TRUE, input.vecs=list(normVec=NULL, probeVec=NULL, probeVarBetween=NULL, probeVarWithin=NULL, probesetSD=NULL), verbose=FALSE){

  e <- as.matrix(e)
  pns <- rownames(e)
  
  if(background){
    if(verbose) message("Background Correcting ...\n")
    e <- rma.background.correct(e, copy=FALSE)
  }
  
  if(is.null(input.vecs$normVec) | is.null(input.vecs$probeVec) | is.null(input.vecs$probeVarWithin) | is.null(input.vecs$probeVarBetween) | is.null(input.vecs$probesetSD)){
    stop("For simpleFrma all input.vecs must be supplied by the user.")
  }

  if(normalize){
    if(verbose) message("Normalizing ...\n")
    e <- normalize.quantiles.use.target(e, input.vecs$normVec)
  }

  if(verbose) message("Summarizing ...\n")
  pms <- log2(e)
  
  w <- 1/(input.vecs$probeVarWithin + input.vecs$probeVarBetween)
  tmp <- split(data.frame(pms, w, input.vecs$probeVec, rep(input.vecs$probesetSD, table(pns))), pns)
  fit <- lapply(tmp, rwaFit)
  exprs <- matrix(unlist(lapply(fit, function(x) x$Estimates)), ncol=ncol(pms), byrow=TRUE)
  rownames(exprs) <- names(fit)
  colnames(exprs) <- colnames(pms)
  
  return(exprs)
}

rwaFit <- function(x){
  pms.tmp <- as.matrix(x[,1:(ncol(x)-3)])
  w.tmp <- x$w/max(x$w)
  w.tmp <- matrix(rep(w.tmp,ncol(pms.tmp)), ncol=ncol(pms.tmp))
  pe.tmp <- x$input.vecs.probeVec
  pe.tmp[1] <- pe.tmp[1]-sum(pe.tmp)
  psd.tmp <- x$probesetSD[1]
  rcModelWPLM(y=pms.tmp, w=w.tmp, row.effects=pe.tmp, input.scale=psd.tmp)
}



  
