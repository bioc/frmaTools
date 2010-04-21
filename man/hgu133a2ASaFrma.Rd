\name{hgu133a2ASaFrma}
\alias{hgu133a2ASaFrma}
\title{hgu133a2ASaFrma}
\description{
  Preprocess Affymetrix HGU133a2 arrays as if they were HGU133a
  arrays using fRMA. 
}
\usage{
  hgu133a2ASaFrma(object, verbose=FALSE)
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
## this takes a long time
\dontrun{
  data(AffyBatch133a2)
  e <- hgu133a2ASaFrma(AffyBatch133a2)
}
}
\keyword{manip}

