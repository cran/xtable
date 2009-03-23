\name{print.xtable}
\alias{print.xtable}
\title{Print Export Tables}
\description{Function returning and displaying or writing to disk the LaTeX or HTML code associated with the supplied object of class \code{xtable}.}
\usage{\method{print}{xtable}(x, type="latex", file="", append=FALSE, floating=TRUE, floating.environment="table", table.placement = "ht", caption.placement="bottom", latex.environments=c("center"),tabular.environment = "tabular", size=NULL,hline.after=c(-1,0,nrow(x)), NA.string = "", include.rownames=TRUE, include.colnames=TRUE, only.contents=FALSE, add.to.row=NULL, sanitize.text.function=NULL, sanitize.rownames.function=sanitize.text.function, sanitize.colnames.function=sanitize.text.function, math.style.negative=FALSE, html.table.attributes="border=1", ...)}
\arguments{
  \item{x}{An object of class \code{"xtable"}.}
  \item{type}{Type of table to produce.  Possible values for \code{type} are \code{"latex"} or \code{"html"}.
              Default value is \code{"latex"}.}
  \item{file}{Name of file where the resulting code should be saved.  If \code{file=""}, output is displayed on screen.  Note that
              the function also (invisibly) returns a character vector of the results (which can be helpful for post-processing).
              Default value is \code{""}.}
  \item{append}{If \code{TRUE} and \code{file!=""}, code will be appended to \code{file} instead of overwriting \code{file}.
                Default value is \code{FALSE}.}
  \item{floating}{If \code{TRUE} and \code{type="latex"}, the resulting table will be a floating table (using, for example, \code{\\begin\{table\}} and \code{\\end\{table\}}).  See \code{floating.environment} below. Default value is \code{TRUE}.}
  \item{floating.environment}{If \code{floating=TRUE} and \code{type="latex"}, the resulting table uses the specified floating environment.
  Possible values are \code{"table"} or \code{"sidewaystable"} (defined in the LaTeX package 'rotating').
                Default value is \code{"table"}.}
  \item{table.placement}{If \code{floating=TRUE} and \code{type="latex"}, the floating table will have placement given by \code{table.placement} where \code{table.placement} must be \code{NULL} or contain only elements of \{"h","t","b","p","!","H"\}.
                Default value is \code{"ht"}.}
  \item{caption.placement}{The caption will be have placed at the bottom of the table if \code{caption.placement} is \code{"bottom"} and at the top of the table if it equals \code{"top"}.
                Default value is \code{"bottom"}.}
  \item{latex.environments}{If \code{floating=TRUE} and \code{type="latex"}, the specificed latex environments (provided as a character vector) will enclose the tabuluar environment.
                Default value is \code{"center"}.}
  \item{tabular.environment}{When \code{type="latex"}, the tabular environment that will be used. Defaults to \code{"tabular"}. When working with tables that extend more than one page, using \code{tabular.environment="longtable"} and the LaTeX package \code{"longtable"} (see Fairbairns, 2005) allows one to typeset them uniformly. Note that \code{"floating"}  should be set to \code{"FALSE"} when using the \code{"longtable"} environment.}
  \item{size}{An arbitrary character vector intended to be used to set the font size in a LaTeX table.  The supplied value (if not \code{NULL}) is inserted just before the tabular environment starts. Default value is \code{NULL}.}
  \item{hline.after}{When \code{type="latex"}, a vector of numbers between -1 and \code{"nrow(x)"}, inclusive, indicating the rows after which a horizontal line should appear.  If \code{NULL} is used no lines are produced. Default value is \code{c(-1,0,nrow(x))} which means draw a line before and after the columns names and at the end of the table. Repeated values are allowed.}
  \item{NA.string}{String to be used for missing values in table entries.  Default value is \code{""}.}
  \item{include.rownames}{logical. If \code{TRUE} the rows names is printed. Default value is \code{TRUE}.}
  \item{include.colnames}{logical. If \code{TRUE} the columns names is printed. Default value is \code{TRUE}.}
  \item{only.contents}{logical. If \code{TRUE} only the rows of the table is printed. Default value is \code{FALSE}.}
  \item{add.to.row}{a list of two components. The first component (which should be called 'pos') is a list contains the position of rows on which extra commands should be added at the end, The second component (which should be called 'command') is a character vector of the same length of the first component which contains the command that should be added at the end of the specified rows. Default value is \code{NULL}, i.e. do not add commands.}
  \item{sanitize.text.function}{All non-numeric enteries (except row and column names) are sanitised in an attempt to remove characters which have special meaning for the output format. If \code{sanitize.text.function} is not NULL (the default), it should be a function taking a character vector and returning one, and will be used for the sanitization instead of the default internal function.}
  \item{sanitize.rownames.function}{Like the \code{sanitize.text.function}, but applicable to row names.  The default uses the \code{sanitize.text.function}.}
  \item{sanitize.colnames.function}{Like the \code{sanitize.text.function}, but applicable to column names.  The default uses the \code{sanitize.text.function}.}
  \item{math.style.negative}{In a LaTeX table, if \code{TRUE}, then use $-$ for the negative sign (as was the behavior prior to version 1.5-3).  Default value is \code{FALSE}.}
  \item{html.table.attributes}{In an HTML table, attributes associated with the \code{<TABLE>} tag.  Default value is \code{border=1}.}
  \item{...}{Additional arguments.  (Currently ignored.)}
}
\details{
  This function displays or writes to disk the code to produce a table associated with an object \code{x} of class \code{"xtable"}.
  The resulting code is either a LaTeX or HTML table, depending on the value of \code{type}.  The function also (invisibly) returns a character vector
  of the results (which can be helpful for post-processing).

  Since version 1.4 the non default behavior of \code{hline.after} is changed. To obtain the same results as the previous versions add to the \code{hline.after} vector the vector \code{c(-1, 0, nrow(x))} where \code{nrow(x)} is the numbers of rows of the object.

  From version 1.4-3, all non-numeric columns are sanitized, and all LaTeX special characters are sanitised for LaTeX output.  See Section 3 of the \code{xtableGallery} vignette for an example of customising the sanitization.
  From version 1.4-4, the sanitization also applies to column names.  To remove any text sanitization, specify \code{sanitize.text.function=function(x){x}}.
}
\author{David Dahl \email{dahl@stat.tamu.edu} with contributions and suggestions from many others (see source code).}
\references{
	Fairbairns, Robin (2005) \emph{Tables longer than a single page} The UK List of TeX Frequently Asked Questions on the Web. \url{http://www.tex.ac.uk/cgi-bin/texfaq2html?label=longtab}
}
\seealso{\code{\link{xtable}}, \code{\link{caption}}, \code{\link{label}}, 
         \code{\link{align}}, \code{\link{digits}}, \code{\link{display}}, \code{\link{formatC}}}

\keyword{print}
