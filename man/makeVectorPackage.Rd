\name{makeVectorPackage}
\alias{makeVectorPackage}
\alias{makeVectors}
\title{makeVectorPackage}
\description{
  Make a package containing the vectors used by the frma package. If you
  don't plan to distribute and maintain this set of vectors, consider
  using the function makeVectors instead.
}
\usage{
  makeVectorPackage(object, batch.id, target, version, maintainer, species, outdir=".", unlink=TRUE, verbose=TRUE)
  makeVectors(object, batch.id, target="core", verbose=TRUE)
}
\arguments{
  \item{object}{an AffyBatch object from which to create the vectors}
  \item{batch.id}{a vector of batch ids used to compute within and
    between batch variances}
  \item{target}{summarization level for exon arrays. Must be one of:
    probeset, core, full, extended.}
  \item{version}{the version number of the package to be created}
  \item{maintainer}{typically your name}
  \item{species}{species of samples in object}
  \item{outdir}{directory in which to create the package}
  \item{unlink}{logical value. If TRUE and outdir already contains a
    file or directory with the same name as the package being generated,
    then try to unlink (remove) it.}
  \item{verbose}{logical value. If TRUE then some messages are displayed
  while the function runs}
}
\value{
  The makeVectorPackage function creates a package with the name
  <array platform>frmavecs. For example is the object contains HGU133a
  arrays, the package would be called HGU133afrmavecs. 

  The makeVectors function creates a list with 6 elements:
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
## this takes a long time
\dontrun{
  library(frmaExampleData)
  data(AffyBatch133a)
  batch.id <- rep(1:3, each=3)
  vecs <- makeVectors(AffyBatch133a, batch.id)
}
}
\keyword{manip}

