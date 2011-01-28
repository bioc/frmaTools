makeVectorPackage <- function(files, batch.id, version, maintainer, species, type="AffyBatch", target=NULL, file.dir=".", output.dir=".", unlink=TRUE, verbose=TRUE){

  type <- match.arg(type, c("AffyBatch", "ExonFeatureSet"))

  if(type=="ExonFeatureSet"){
    target <- match.arg(target, c("core", "full", "extended", "probeset"))
    require(oligo)
  }
  
  if(type=="AffyBatch"){
    cdfname <- cleancdfname(cdfName(object))
    platform <- gsub("cdf","",cdfname)
    pkgname <- paste(platform, "frmavecs", sep="")
  }
  if(type=="ExonFeatureSet"){
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
                              destinationDir=output.dir,
                              originDir=system.file("VectorPkg-template", package="frmaTools"),
                              symbolValues=symbolValues,
                              unlink=unlink)

  if(type=="AffyBatch") vecs <- makeVectorsAffyBatch(files, batch.id, file.dir, verbose)
  if(type=="ExonFeatureSet") vecs <- makeVectorsExonFeatureSet(files, batch.id, target, file.dir, verbose)
  assign(pkgname, vecs)
  save(list=eval(pkgname), file=file.path(createdPkg$pkgdir, "data", paste(pkgname, ".rda", sep="")), compress=TRUE)
}


