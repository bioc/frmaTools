hgu133a2ASaFrma <- function(object, verbose=FALSE){
  if(class(object) != "AffyBatch") stop("object must be of class AffyBatch.")
  if(cleancdfname(cdfName(object)) != "hgu133a2cdf") stop("object must be hgu133a2 data.")

  obj2 <- convertPlatform(object, "hgu133a")
  e <- pm(obj2)
  rownames(e) <- probeNames(obj2)
  colnames(e) <- sampleNames(object)
  
  ind <- grep("AFFX", probeNames(obj2))
  ps.ind <- unique(probeNames(obj2)[ind])
  e2 <- e[-ind,]

  require("hgu133afrmavecs", character.only=TRUE)
  data(list="hgu133afrmavecs")
  probevec <- hgu133afrmavecs$probeVec[-ind]
  probevarwithin <- hgu133afrmavecs$probeVarWithin[-ind]
  probevarbetween <- hgu133afrmavecs$probeVarBetween[-ind]
  probesetsd <- hgu133afrmavecs$probesetSD[-which(names(hgu133afrmavecs$probesetSD) %in% ps.ind)]

  normvec <- hgu133afrmavecs$normVec
  N=length(normvec)
  tmpN <- length(ind)
  if(tmpN>0){
    outIndex <- seq(1,N,len=tmpN+1)
    outIndex <- round((outIndex[-1]+outIndex[-length(outIndex)])/2)
    normvec <- normvec[-outIndex]
  }
  
  input.vecs <- list(normVec=normvec, probeVec=probevec, probeVarBetween=probevarbetween, probeVarWithin=probevarwithin, probesetSD=probesetsd)  
  exprs <- simpleFrma(e2, input.vecs=input.vecs, verbose=verbose)

  return(exprs)
}
