\name{hgu133plus2ASaFrma}
\alias{hgu133plus2ASaFrma}
\title{hgu133plus2ASaFrma}
\description{
  Preprocess Affymetrix HGU133plus2 arrays as if they were HGU133a
  arrays using fRMA. 
}
\usage{
  hgu133plus2ASaFrma(object, verbose=FALSE)
}
\arguments{
  \item{object}{an AffyBatch object}
  \item{verbose}{logical value. If TRUE then some messages are displayed
  while the function runs}
}
\value{
  A matrix of preprocessed expression values.
}
\author{Matthew N. McCall}
\examples{
## this take a long time
\dontrun{
  library(frmaExampleData)
  data(AffyBatch133plus2)
  e <- hgu133plus2ASaFrma(AffyBatch133plus2[,1])
}
}
\keyword{manip}

