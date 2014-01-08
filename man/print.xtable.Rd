\name{print.xtable}
\alias{print.xtable}
\title{Print Export Tables}
\description{
  Function returning and displaying or writing to disk the LaTeX or HTML
  code associated with the supplied object of class \code{xtable}.
}
\usage{
\method{print}{xtable}(x, 
  type = getOption("xtable.type", "latex"),
  file = getOption("xtable.file", ""),
  append = getOption("xtable.append", FALSE),
  floating = getOption("xtable.floating", TRUE),
  floating.environment = getOption("xtable.floating.environment", "table"),
  table.placement = getOption("xtable.table.placement", "ht"),
  caption.placement = getOption("xtable.caption.placement", "bottom"),
  caption.width = getOption("xtable.caption.width", NULL),
  latex.environments = getOption("xtable.latex.environments", c("center")),
  tabular.environment = getOption("xtable.tabular.environment", "tabular"),
  size = getOption("xtable.size", NULL),
  hline.after = getOption("xtable.hline.after", c(-1,0,nrow(x))),
  NA.string = getOption("xtable.NA.string", ""),
  include.rownames = getOption("xtable.include.rownames", TRUE),
  include.colnames = getOption("xtable.include.colnames", TRUE),
  only.contents = getOption("xtable.only.contents", FALSE),
  add.to.row = getOption("xtable.add.to.row", NULL),
  sanitize.text.function = getOption("xtable.sanitize.text.function", NULL),
  sanitize.rownames.function = getOption("xtable.sanitize.rownames.function", 
                                         sanitize.text.function),
  sanitize.colnames.function = getOption("xtable.sanitize.colnames.function", 
                                         sanitize.text.function),
  math.style.negative = getOption("xtable.math.style.negative", FALSE),
  html.table.attributes = getOption("xtable.html.table.attributes",
                                    "border=1"),
  print.results = getOption("xtable.print.results", TRUE),
  format.args = getOption("xtable.format.args", NULL),
  rotate.rownames = getOption("xtable.rotate.rownames", FALSE),
  rotate.colnames = getOption("xtable.rotate.colnames", FALSE),
  booktabs = getOption("xtable.booktabs", FALSE),
  scalebox = getOption("xtable.scalebox", NULL),
  width = getOption("xtable.width", NULL),
  comment = getOption("xtable.comment", TRUE),
  timestamp = getOption("xtable.timestamp", date()),
  ...)}
\arguments{
  \item{x}{An object of class \code{"xtable"}.}
  \item{type}{Type of table to produce.  Possible values for \code{type}
    are \code{"latex"} or \code{"html"}.
    Default value is \code{"latex"}.}
  \item{file}{Name of file where the resulting code should be saved.  If
    \code{file=""}, output is displayed on screen.  Note that the
    function also (invisibly) returns a character vector of the results
    (which can be helpful for post-processing).
    Default value is \code{""}.}
  \item{append}{If \code{TRUE} and \code{file!=""}, code will be
    appended to \code{file} instead of overwriting \code{file}.
    Default value is \code{FALSE}.}
  \item{floating}{If \code{TRUE} and \code{type="latex"}, the resulting
    table will be a floating table (using, for example,
    \code{\\begin\{table\}} and \code{\\end\{table\}}).  See
    \code{floating.environment} below.
    Default value is \code{TRUE}. }
  \item{floating.environment}{If \code{floating=TRUE} and
    \code{type="latex"}, the resulting table uses the specified floating
    environment. Possible values include \code{"table"}, \code{"table*"}, 
    and other floating environments defined in LaTeX packages.
    Default value is \code{"table"}.}
  \item{table.placement}{If \code{floating=TRUE} and
    \code{type="latex"}, the floating table will have placement given by
    \code{table.placement} where \code{table.placement} must be
    \code{NULL} or contain only elements of
    \{"h","t","b","p","!","H"\}.
    Default value is \code{"ht"}.}
  \item{caption.placement}{The caption will be have placed at the bottom
    of the table if \code{caption.placement} is \code{"bottom"} and at
    the top of the table if it equals \code{"top"}.
    Default value is \code{"bottom"}.}
  \item{caption.width}{The caption will be placed in a \code{"parbox"}
    of the specified width if \code{caption.width} is not \code{NULL} and
	\code{type="latex"}. Default value is \code{NULL}.}
  \item{latex.environments}{If \code{floating=TRUE} and
    \code{type="latex"}, the specificed latex environments (provided as
    a character vector) will enclose the tabular environment.
    Default value is \code{"center"}. }
  \item{tabular.environment}{When \code{type="latex"}, the tabular
    environment that will be used.
    Defaults to \code{"tabular"}.
    When working with tables that extend more than one page, using
    \code{tabular.environment="longtable"} and the LaTeX package
    \code{"longtable"} (see Fairbairns, 2005) allows one to typeset them
    uniformly. Note that \code{"floating"} should be set to
    \code{"FALSE"} when using the \code{"longtable"} environment.}
  \item{size}{An arbitrary character vector intended to be used to set
    the font size in a LaTeX table.  The supplied value (if not
    \code{NULL}) is inserted just before the tabular environment
    starts.
    Default value is \code{NULL}. }
  \item{hline.after}{When \code{type="latex"}, a vector of numbers
    between -1 and \code{"nrow(x)"}, inclusive, indicating the rows
    after which a horizontal line should appear.  If \code{NULL} is used
    no lines are produced.
    Default value is \code{c(-1,0,nrow(x))} which means draw a line
    before and after the columns names and at the end of the
    table. Repeated values are allowed.}
  \item{NA.string}{String to be used for missing values in table
    entries.
    Default value is \code{""}.}
  \item{include.rownames}{logical. If \code{TRUE} the rows names is
    printed.
    Default value is \code{TRUE}.}
  \item{include.colnames}{logical. If \code{TRUE} the columns names is
    printed.
    Default value is \code{TRUE}.}
  \item{only.contents}{logical. If \code{TRUE} only the rows of the
    table is printed.
    Default value is \code{FALSE}. }
  \item{add.to.row}{a list of two components. The first component (which
    should be called 'pos') is a list contains the position of rows on
    which extra commands should be added at the end, The second
    component (which should be called 'command') is a character vector
    of the same length of the first component which contains the command
    that should be added at the end of the specified rows.
    Default value is \code{NULL}, i.e. do not add commands.}
  \item{sanitize.text.function}{All non-numeric enteries (except row and
    column names) are sanitised in an attempt to remove characters which
    have special meaning for the output format. If
    \code{sanitize.text.function} is not NULL (the default), it should
    be a function taking a character vector and returning one, and will
    be used for the sanitization instead of the default internal
    function.}
  \item{sanitize.rownames.function}{Like the
    \code{sanitize.text.function}, but applicable to row names.
    The default uses the \code{sanitize.text.function}. }
  \item{sanitize.colnames.function}{Like the
    \code{sanitize.text.function}, but applicable to column names.
    The default uses the \code{sanitize.text.function}. }
  \item{math.style.negative}{In a LaTeX table, if \code{TRUE}, then use
    $-$ for the negative sign (as was the behavior prior to version 1.5-3).
    Default value is \code{FALSE}.}
  \item{html.table.attributes}{In an HTML table, attributes associated
    with the \code{<TABLE>}tag.
    Default value is \code{border=1}.}
  \item{print.results}{If \code{TRUE}, the generated table is printed to
    standard output.  Set this to \code{FALSE} if you will just be using
    the character vector that is returned invisibly.}
  \item{format.args}{List of arguments for the \code{formatC} function.
    For example, standard German number separators can be specified as
    \code{format.args=list(big.mark = "'", decimal.mark =
      ","))}. \code{digits} and \code{format} arguments should not be
    included in this list. See details. }
  \item{rotate.rownames}{If \code{TRUE}, the row names are displayed
    vertically in LaTeX. }
  \item{rotate.colnames}{If \code{TRUE}, the column names are displayed
    vertically in LaTeX. }
  \item{booktabs}{If \code{TRUE}, the \code{toprule}, \code{midrule} and
    \code{bottomrule} tags from the LaTex "booktabs" package are used
    rather than \code{hline} for the horizontal line tags. }
  \item{scalebox}{If not \code{NULL}, a \code{scalebox} clause will be
    added around the tabular environment with the specified value used
    as the scaling factor. }
  \item{width}{If not \code{NULL}, the specified value is included in
    parenthesis between the tabular environment \code{begin} tag and the
    alignment specification.  This allows specification of the table
    width when using tabular environments such as \code{tabular*} and
    \code{tabularx}.  Note that table width specification is not
    supported with the \code{tabular} or \code{longtable} environments. }
  \item{comment}{If \code{TRUE}, the version and timestamp comment is
    included.  Default value is \code{TRUE}. }  
  \item{timestamp}{Timestamp to include in LaTeX comment.  Set this
    to \code{NULL} to exclude the timestamp. Default value is \code{date()}. }  
  \item{...}{Additional arguments.  (Currently ignored.) }
}
\details{
  This function displays or writes to disk the code to produce a table
  associated with an object \code{x} of class \code{"xtable"}. 
  The resulting code is either a LaTeX or HTML table, depending on the
  value of \code{type}.  The function also (invisibly) returns a
  character vector of the results (which can be helpful for
  post-processing).

  Since version 1.4 the non default behavior of \code{hline.after} is
  changed. To obtain the same results as the previous versions add to
  the \code{hline.after} vector the vector \code{c(-1, 0, nrow(x))}
  where \code{nrow(x)} is the numbers of rows of the object. 

  From version 1.4-3, all non-numeric columns are sanitized, and all
  LaTeX special characters are sanitised for LaTeX output.  See Section
  3 of the \code{xtableGallery} vignette for an example of customising
  the sanitization. From version 1.4-4, the sanitization also applies to
  column names.  To remove any text sanitization, specify
  \code{sanitize.text.function=function(x){x}}.  
  
  From version 1.6-1 the default values for the arguments other than
  \code{x} are obtainined using \code{getOption()}.  Hence the user can
  set the values once with \code{options()} rather than setting them in
  every call to \code{print.xtable()}.

  The argument \code{format.args} is used to supply arguments to the
  \code{formatC} function, but will throw an error if values for
  \code{digits} or \code{format} are included in the list of
  arguments. The recommended approach is to specify \code{digits} supply
  the argument \code{digits} to \code{xtable}, and to specify
  \code{format} supply the argument \code{display} to \code{xtable}. See
  the examples.  
}
\author{
  David Dahl \email{dahl@stat.tamu.edu} with contributions and
  suggestions from many others (see source code).
}
\references{
  Fairbairns, Robin (2005) \emph{Tables longer than a single page} The
  UK List of TeX Frequently Asked Questions on the
  Web. \url{http://www.tex.ac.uk/cgi-bin/texfaq2html?label=longtab}
}
\seealso{
  \code{\link{xtable}}, \code{\link{caption}}, \code{\link{label}}, 
  \code{\link{align}}, \code{\link{digits}}, \code{\link{display}},
  \code{\link{formatC}} 
}

\examples{
df <- data.frame(A = c(1.00123, 33.1, 6),
                 B = c(111111, 3333333, 3123.233))
## The following code gives the error
## formal argument "digits" matched by multiple actual arguments
## print(xtable(df, display = c("s","e","e")),
##       format.args = list(digits = 3, big.mark = " ", decimal.mark = ","))
## specify digits as argument to xtable instead
print(xtable(df, display = c("s","f","f"), digits = 4),
      format.args = list(big.mark = " ", decimal.mark = ","))
## The following code gives the error
## formal argument "format" matched by multiple actual arguments
## print(xtable(df, digits = 4),
##       format.args = list(format = c("s","e","e"),
##                          big.mark = " ", decimal.mark = ","))
## specify format using display argument in xtable
print(xtable(df, display = c("s","e","e"), digits = 4),
      format.args = list(big.mark = " ", decimal.mark = ","))

}

\keyword{print}
