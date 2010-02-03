\name{convertPlatform}
\alias{convertPlatform}
\title{convertPlatform}
\description{
  Convert an AffyBatch object from one platform to another where the new
  platform is a subset of the original (i.e. hgu133atag to hgu133a).
}
\usage{
  convertPlatform(object, new.platform)
}
\arguments{
  \item{object}{an AffyBatch object to be converted}
  \item{new.platform}{the name of the platform to be converted to}
}
\value{
  An AffyBatch object containing the data from the original object that
  could be mapped to the new platform. 
}
\author{Matthew N. McCall}
\examples{
  library(frmaExampleData)
  data(AffyBatch133atag)
  object <- convertPlatform(AffyBatch133atag, "hgu133a")
}
\keyword{manip}

