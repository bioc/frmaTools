makeVectorPackage <- function(files, batch.id, version, maintainer, species, annotation, packageName, background="rma", normalize="quantile", normVec=NULL, type="AffyBatch", file.dir=".", output.dir=".", unlink=TRUE, verbose=TRUE){

  type <- match.arg(type, c("AffyBatch", "FeatureSet"))

  if(type=="AffyBatch") platform <- gsub("cdf","",annotation)  
  if(type=="FeatureSet"){
    platform <- annotation
    require(oligo)
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

  
  createdPkg <- createPackage(packageName,
                              destinationDir=output.dir,
                              originDir=system.file("VectorPkg-template", package="frmaTools"),
                              symbolValues=symbolValues,
                              unlink=unlink)

  if(type=="AffyBatch") vecs <- makeVectorsAffyBatch(files, batch.id, background, normalize, normVec, annotation, file.dir, verbose)
  if(type=="FeatureSet") vecs <- makeVectorsFeatureSet(files, batch.id, annotation, background, normalize, normVec, file.dir, verbose)
  assign(packageName, vecs)
  save(list=eval(packageName), file=file.path(createdPkg$pkgdir, "data", paste(packageName, ".rda", sep="")), compress=TRUE)
}


