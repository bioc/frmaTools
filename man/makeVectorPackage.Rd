\name{makeVectorPackage}
\alias{makeVectorPackage}
\alias{makeVectorsAffyBatch}
\alias{makeVectorsExonFeatureSet}
\title{makeVectorPackage}
\description{
  Make a package containing the vectors used by the frma package. If you
  don't plan to distribute and maintain this set of vectors, consider
  using either makeVectorsAffyBatch or makeVectorsExonFeatureSet instead.
}
\usage{
  makeVectorPackage(files, batch.id, version, maintainer, species,
annotation, packageName, background="rma", normalize="quantile", normVec=NULL, 
type="AffyBatch", target=NULL, file.dir=".", output.dir=".", unlink=TRUE, verbose=TRUE)
  makeVectorsAffyBatch(files, batch.id, background="rma",
normalize="quantile", normVec=NULL, cdfname=NULL, file.dir=".", verbose=TRUE)
  makeVectorsExonFeatureSet(files, batch.id, target, pkgname,
background="rma", normalize="quantile", normVec=NULL, file.dir=".", verbose=TRUE)
}
\arguments{
  \item{files}{the CEL file names from which to create the vectors}
  \item{batch.id}{a vector of batch ids used to compute within and
    between batch variances}
  \item{background}{type of background correction to perform: either
    "none" or "rma".}
  \item{normalize}{type of normalization to perform: either "none" or
    "quantile".}
  \item{normVec}{normalization vector. If NULL, a normalization vector
    is created based on the given CEL files. Ignored if normalize is
    "none".}
  \item{annotation}{the "cdfname" or "pkgname" to be passed to either
    makeVectorsAffyBatch or makeVectorsExonExpressionSet depending on
    the value of "type".}
  \item{cdfname}{used to specify the name of an alternative cdf package.
    Passed to the ReadAffy function. If set to "NULL", then the usual
    cdf package based on Affymetrix's mappings will be used.}
  \item{pkgname}{alternative data package to be loaded. Passed to the
    read.celfiles function. If set to "NULL", then the default package
    will be used.}
  \item{target}{summarization level for exon arrays. Must be one of:
    probeset, core, full, extended.}
  \item{packageName}{name of the package to be created}
  \item{version}{the version number of the package to be created}
  \item{maintainer}{typically your name}
  \item{species}{species of samples in object}
  \item{type}{the type of data object to be loaded. Must be one of:
    AffyBatch, ExonFeatureSet.}
  \item{file.dir}{directory in which the CEL files are located}
  \item{output.dir}{directory in which to create the package}
  \item{unlink}{logical value. If TRUE and output.dir already contains a
    file or directory with the same name as the package being generated,
    then try to unlink (remove) it.}
  \item{verbose}{logical value. If TRUE then some messages are displayed
  while the function runs}
}
\value{
  The makeVectorPackage function creates a package with the name
  <array platform>frmavecs. For example if the files are HGU133a
  arrays, the package would be called hgu133afrmavecs. 

  The makeVectors functions create a list with 6 elements:
  \tabular{ll}{
    normVec \tab normalization vector \cr
    probeVec \tab probe effect vector \cr
    probeVarWithin \tab within batch probe variance \cr
    probeVarBetween \tab between batch probe variance \cr
    probesetSD \tab within probeset standard deviation \cr
    medianSE \tab median standard errors \cr
  }
}
\author{Matthew N. McCall}
\examples{
## assuming the celfiledir points to a directory with 9 CEL files
## this takes a long time
\dontrun{
  vecs <- makeVectorsAffyBatch(celfiledir, batch.id=rep(1:3, each=3))
}
}
\keyword{manip}

