\name{print.xtable}
\alias{print.xtable}
\title{Print Export Tables}
\description{Function displaying or writing to disk the LaTeX or HTML code associated with the supplied object of class \code{xtable}.}
\usage{print.xtable(x, type="latex", file="", append=FALSE, floating=TRUE, table.placement = "ht", caption.placement="bottom", latex.environments=c("center"), size=NULL, hline.after=NULL, ...)}
\arguments{
  \item{x}{An object of class \code{"xtable"}.}
  \item{type}{Type of table to produce.  Possible values for \code{type} are \code{"latex"} or \code{"html"}.
              Default value is \code{"latex"}.}
  \item{file}{Name of file where the resulting code should be saved.  If \code{file==""}, output is displayed on screen.
              Default value is \code{""}.}
  \item{append}{If \code{TRUE} and \code{file!=""}, code will be appended to \code{file} instead of overwriting \code{file}.
                Default value is \code{FALSE}.}
  \item{floating}{If \code{TRUE} and \code{type="latex"}, the resulting table will be a floating table (using \code{\\begin\{table\}} and \code{\\end\{table\}}).
                Default value is \code{TRUE}.}
  \item{table.placement}{If \code{floating=TRUE} and \code{type="latex"}, the floating table will have placement given by \code{table.placement} where \code{table.placement} must be \code{NULL} or contain only elements of {"h","t","b","p","!"}.
                Default value is \code{"ht"}.}
  \item{caption.placement}{The caption will be have placed at the bottom of the table if \code{caption.placement} is \code{"bottom"} and at the top of the table if it equals \code{"top"}.
                Default value is \code{"bottom"}.}
  \item{latex.environments}{If \code{floating=TRUE} and \code{type="latex"}, the specificed latex environments (provided as a character vector) will enclose the tabuluar environment.
                Default value is \code{"center"}.}
  \item{size}{An arbitrary character vector intended to be used to set the font size in a LaTeX table.  The supplied value (if not \code{NULL}) is inserted just before the tabular environment starts. Default value is \code{NULL}.}
  \item{hline.after}{When \code{type="latex"}, a vector of numbers between 1 and \code{"nrow(x)"}, inclusive, indicating the rows after which a horizontal line should appear.  If \code{NULL} is used, no extra lines are produced. Default value is \code{NULL}.}
  \item{...}{Additional arguments.  (Currently ignored.)}
}
\details{
  This function displays or writes to disk the code to produce a table associated with an object \code{x} of class \code{"xtable"}.
  The resulting code is either a LaTeX or HTML table, depending on the value of \code{type}.
}
\author{David Dahl \email{dahl@stat.tamu.edu}}
\seealso{\code{\link{xtable}}, \code{\link{caption}}, \code{\link{label}}, \code{\link{vsep}},
         \code{\link{align}}, \code{\link{digits}}, \code{\link{display}}, \code{\link{formatC}}}
\keyword{print}
