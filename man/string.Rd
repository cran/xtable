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
  \method{print}{string}(x,...)
  \method{+}{string}(x,y)
  as.string(x,file="",append=FALSE)
  is.string(x)
}
\arguments{
  \item{text}{A character object.}
  \item{file}{Name of the file that should receive the printed string.}
  \item{append}{Should the printed string be appended to the file?}
  \item{x}{A \code{string} object.}
  \item{y}{A \code{string} object.}
  \item{...}{Additional arguments.  (Currently ignored.)}
}
\details{
  These functions are private functions used by \code{print.xtable}.  They are
  not intended to be used elsewhere.
}
\author{David Dahl \email{dahl@stat.tamu.edu} with contributions and suggestions from many others (see source code).}
\seealso{\code{\link{print.xtable}}}
\keyword{print}
