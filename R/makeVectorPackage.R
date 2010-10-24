makeVectorPackage <- function(object, batch.id, target, version, maintainer, species, outdir=".", unlink=TRUE, verbose=TRUE){
  if(!class(object) %in% c("AffyBatch", "ExonFeatureSet")) stop("object must be of class AffyBatch or ExonFeatureSet.")

  if(class(object)=="ExonFeatureSet") target <- match.arg(target, c("core", "full", "extended", "probeset"))
  
  if(class(object)=="AffyBatch"){
    cdfname <- cleancdfname(cdfName(object))
    platform <- gsub("cdf","",cdfname)
    pkgname <- paste(platform, "frmavecs", sep="")
  }
  if(class(object)=="ExonFeatureSet"){
    cdfname <- annotation(object)
    platform <- paste(cdfname, target, sep="")
    pkgname <- paste(platform, "frmavecs", sep="")
  }
  thispkg <- "frmaTools"
  desc <- packageDescription(thispkg)
  thispkgVers <- desc$Version
  
  symbolValues <- list(ARRAYTYPE = platform,
                       VERSION = version,
                       CREATOR = paste("package", thispkg, "version", thispkgVers),
                       FRMATOOLSVERSION = thispkgVers,
                       MAINTAINER = maintainer,
                       SPECIES = species)

  
  createdPkg <- createPackage(pkgname,
                              destinationDir=outdir,
                              originDir=system.file("VectorPkg-template", package="frmaTools"),
                              symbolValues=symbolValues,
                              unlink=unlink)

  vecs <- makeVectors(object, batch.id, target, verbose)
  assign(pkgname, vecs)
  save(list=eval(pkgname), file=file.path(createdPkg$pkgdir, "data", paste(pkgname, ".rda", sep="")), compress=TRUE)
}


