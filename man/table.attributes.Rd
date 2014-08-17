\name{table.attributes}
\alias{caption}
\alias{caption.xtable}
\alias{caption<-}
\alias{caption<-.xtable}
\alias{label}
\alias{label.xtable}
\alias{label<-}
\alias{label<-.xtable}
\alias{align}
\alias{align.xtable}
\alias{align<-}
\alias{align<-.xtable}
\alias{digits}
\alias{digits.xtable}
\alias{digits<-}
\alias{digits<-.xtable}
\alias{display}
\alias{display.xtable}
\alias{display<-}
\alias{display<-.xtable}
\title{Retrieve and Set Options for Export Tables}
\description{Functions retrieving or setting table attributes for the supplied object of class \code{"xtable"}.}
\usage{
  caption(x,...)
  caption(x) <- value
  label(x,...)
  label(x) <- value
  align(x,...)
  align(x) <- value
  digits(x,...)
  digits(x) <- value
  display(x,...)
  display(x) <- value
}
\arguments{
  \item{x}{An \code{"xtable"} object.}
  \item{value}{The value of the corresponding attribute.}
  \item{...}{Additional arguments.  (Currently ignored.)}
}
\details{
  These functions retrieve or set table attributes of the object \code{x} of class \code{"xtable"}.  See
  \code{xtable} for a description of the options.
}
\author{David Dahl \email{dahl@stat.byu.edu} with contributions and suggestions from many others (see source code).}
\seealso{\code{\link{xtable}}, \code{\link{print.xtable}}, \code{\link{formatC}}}
\keyword{print}
