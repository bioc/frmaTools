convertPlatform <- function(object, new.platform){
  if(class(object) != "AffyBatch") stop("object must be of class AffyBatch.")
  if(length(new.platform) != 1 | !is.character(new.platform)) stop("new.platform must be a character vector of length 1.")

  cdfname <- cleancdfname(cdfName(object))
  old.platform <- gsub("cdf","",cdfname)
  map <- makeMaps(new.platform, old.platform)

  tmp <- new("AffyBatch", cdfName=new.platform)
  pns <- probeNames(tmp)
  index <- unlist(pmindex(tmp))
  mIndex <- match(index,map[,1])
  if(any(is.na(mIndex))) stop("new.platform is not a subset of the original platform")
  pmIndex <- map[mIndex,2]

  require(paste(new.platform,"cdf",sep=""), character.only=TRUE)
  env <- get(paste(new.platform,"dim",sep=""))
  nc <- env$NCOL
  nr <- env$NROW
  exprs2 <- matrix(nrow=nc*nr, ncol=length(object))
  exprs2[index,] <- exprs(object)[pmIndex,]
  
  new("AffyBatch", exprs=exprs2, cdfName=new.platform, nrow=nr, ncol=nc)
}
