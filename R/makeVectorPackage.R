makeVectorPackage <- function(object, batch.id, version, maintainer, species, outdir=".", unlink=TRUE, verbose=TRUE){
  cdfname <- cleancdfname(cdfName(object))
  platform <- gsub("cdf","",cdfname)
  pkgname <- paste(platform, "frmavecs", sep="")
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

  vecs <- makeVectors(object, batch.id, verbose)
  assign(pkgname, vecs)
  save(list=eval(pkgname), file=file.path(createdPkg$pkgdir, "data", paste(pkgname, ".rda", sep="")), compress=TRUE)
}
  
  
  


