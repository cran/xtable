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
  caption(x) <- table.title
  label(x,...)
  label(x) <- table.label
  align(x,...)
  align(x) <- column.alignment
  digits(x,...)
  digits(x) <- column.digits
  display(x,...)
  display(x) <- column.format
}
\arguments{
  \item{x}{An \code{"xtable"} object.}
}
\details{
  These functions retrieve or set table attributes of the object \code{x} of class \code{"xtable"}.  See
  \code{xtable} for a description of the options.
}
\author{David Dahl \email{dbdahl@stat.wisc.edu}}
\seealso{\code{\link{xtable}}, \code{\link{print.xtable}}, \code{\link{formatC}}}
\keyword{print}
