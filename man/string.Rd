\name{string}
\alias{string}
\alias{print.string}
\alias{is.string}
\alias{as.string}
\alias{+.string}
\title{String handling functions}
\description{Private functions for conveniently working with strings.}
\usage{
  string(text,file="",append=FALSE) 
  print.string(x)
  +.string(x,y)
  as.string(x)
  is.string(x)
}
\arguments{
  \item{text}{A character object.}
  \item{file}{Name of the file that should receive the printed string.}
  \item{append}{Should the printed string be appended to the file?}
  \item{x}{A \code{string} object.}
  \item{y}{A \code{string} object.}
}
\details{
  These functions are private functions used by \code{print.xtable}.  They are
  not intended to be used elsewhere.
}
\author{David Dahl \email{dbdahl@stat.wisc.edu}}
\seealso{\code{\link{print.xtable}}}
\keyword{print}
