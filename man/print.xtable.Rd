\name{print.xtable}
\alias{print.xtable}
\title{Print Export Tables}
\description{Function displaying or writing to disk the LaTeX or HTML code associated with the supplied object of class \code{xtable}.}
\usage{print.xtable(x, type="latex", file="", append=FALSE, floating=TRUE, ...)}
\arguments{
  \item{x}{An object of class \code{"xtable"}.}
  \item{type}{Type of table to produce.  Possible values for \code{type} are \code{"latex"} or \code{"html"}.
              Default value is \code{"latex"}.}
  \item{file}{Name of file where the resulting code should be saved.  If \code{file==""}, output is displayed on screen.
              Default value is \code{""}.}
  \item{append}{If \code{TRUE} and \code{file!=""}, code will be appended to \code{file} instead of overwriting \code{file}.
                Default value is \code{FALSE}.}
  \item{floating}{If \code{TRUE} and \code{type="latex"}, the resulting table will be a floating table (using \code{\\begin\{table\} \\begin\{center\}} and \code{\\end\{center\} \\end\{table\}}).
                Default value is \code{TRUE}.}
  \item{...}{Additional arguments.  (Currently ignored.)}
}
\details{
  This function displays or writes to disk the code to produce a table associated with an object \code{x} of class \code{"xtable"}.
  The resulting code is either a LaTeX or HTML table, depending on the value of \code{type}.
}
\author{David Dahl \email{dbdahl@stat.wisc.edu}}
\seealso{\code{\link{xtable}}, \code{\link{caption}}, \code{\link{label}}, \code{\link{vsep}},
         \code{\link{align}}, \code{\link{digits}}, \code{\link{display}}, \code{\link{formatC}}}
\keyword{print}
