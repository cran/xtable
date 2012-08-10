\name{xtable}
\alias{xtable}
\alias{xtable.anova}
\alias{xtable.aov}
\alias{xtable.aovlist}
\alias{xtable.data.frame}
\alias{xtable.glm}
\alias{xtable.lm}
\alias{xtable.matrix}
\alias{xtable.prcomp}
\alias{xtable.coxph}
\alias{xtable.summary.aov}
\alias{xtable.summary.aovlist}
\alias{xtable.summary.glm}
\alias{xtable.summary.lm}
\alias{xtable.summary.prcomp}
\alias{xtable.ts}
\alias{xtable.table}
\alias{xtable.zoo}
\title{Create Export Tables}
\description{
  Function converting an R object to an \code{xtable} object, which can
  then be printed as a LaTeX or HTML table.
}
\usage{
xtable(x, caption = NULL, label = NULL, align = NULL, digits = NULL,
       display = NULL, ...)
}
\arguments{
  \item{x}{An R object of class found among \code{methods(xtable)}.  See
    below on how to write additional method functions for \code{xtable}.}
  \item{caption}{Character vector of length 1 or 2 containing the
    table's caption or title.  If length 2, the second item is the
    "short caption" used when LaTeX generates a "List of Tables". Set to
    \code{NULL} to suppress the caption.  Default value is \code{NULL}. }
  \item{label}{Character vector of length 1 containing the LaTeX label
    or HTML anchor. Set to \code{NULL} to suppress the label.  Default
    value is \code{NULL}. }
  \item{align}{Character vector of length equal to the number of columns
    of the resulting table indicating the alignment of the corresponding
    columns.  Also, \code{"|"} may be used to produce vertical lines
    between columns in LaTeX tables, but these are effectively ignored
    when considering the required length of the supplied vector.  If a
    character vector of length one is supplied, it is split as
    \code{strsplit(align, "")[[1]]} before processing. Since the row
    names are printed in the first column, the length of \code{align} is
    one greater than \code{ncol(x)} if \code{x} is a
    \code{data.frame}. Use \code{"l"}, \code{"r"}, and \code{"c"} to
    denote left, right, and center alignment, respectively.  Use
    \code{"p\{3cm\}"} etc for a LaTeX column of the specified width. For
    HTML output the \code{"p"} alignment is interpreted as \code{"l"},
    ignoring the width request. Default depends on the class of
    \code{x}. }
  \item{digits}{
    Numeric vector of length equal to one (in which case it will be
    replicated as necessary) or to the number of columns of the
    resulting table \bold{or} matrix of the same size as the resulting
    table indicating the number of digits to display in the
    corresponding columns. Since the row names are printed in the first
    column, the length of the vector \code{digits} or the number of
    columns of the matrix \code{digits} is one greater than
    \code{ncol(x)} if \code{x} is a \code{data.frame}. Default depends
    of class of \code{x}. If values of \code{digits} are negative, the
    corresponding values of \code{x} are displayed in scientific format
    with \code{abs(digits)} digits.}
  \item{display}{
    Character vector of length equal to the number of columns of the
    resulting table indicating the format for the corresponding columns.
    Since the row names are printed in the first column, the length of
    \code{display} is one greater than \code{ncol(x)} if \code{x} is a
    \code{data.frame}.  These values are passed to the \code{formatC}
    function.  Use \code{"d"} (for integers), \code{"f"}, \code{"e"},
    \code{"E"}, \code{"g"}, \code{"G"}, \code{"fg"} (for reals), or
    \code{"s"} (for strings).  \code{"f"} gives numbers in the usual
    \code{xxx.xxx} format; \code{"e"} and \code{"E"} give
    \code{n.ddde+nn} or \code{n.dddE+nn} (scientific format); \code{"g"}
    and \code{"G"} put \code{x[i]} into scientific format only if it
    saves space to do so.  \code{"fg"} uses fixed format as \code{"f"},
    but \code{digits} as number of \emph{significant} digits.  Note that
    this can lead to quite long result strings.  Default depends on the
    class of \code{x}.}
  \item{...}{Additional arguments.  (Currently ignored.)}
}
\details{
  
  This function extracts tabular information from \code{x} and returns
  an object of class \code{"xtable"}.  The nature of the table generated
  depends on the class of \code{x}.  For example, \code{aov} objects
  produce ANOVA tables while \code{data.frame} objects produce a table
  of the entire data.frame.  One can optionally provide a caption
  (called a title in HTML) or label (called an anchor in HTML), as well
  as formatting specifications.  Default values for \code{align},
  \code{digits}, and \code{display} are class dependent.

  The available method functions for \code{xtable} are given by
  \code{methods(xtable)}.  Users can extend the list of available
  classes by writing methods for the generic function \code{xtable}.
  These methods functions should have \code{x} as their first argument
  with additional arguments to specify \code{caption}, \code{label},
  \code{align}, \code{digits}, and \code{display}.  Optionally, other
  arguments may be present to specify how the object \code{x} should be
  manipulated.  All method functions should return an object whose class
  if given by \code{c("xtable","data.frame")}.  The resulting object can
  have attributes \code{caption} and \code{label}, but must have
  attributes \code{align}, \code{digits}, and \code{display}.  It is
  strongly recommened that you set these attributes through the provided
  replacement functions as they perform validity checks.
  }
  \value{An object of class \code{"xtable"} which inherits the
  \code{data.frame} class and contains several additional attributes
  specifying the table formatting options.  
}
\author{David Dahl \email{dahl@stat.tamu.edu} with contributions and
  suggestions from many others (see source code).
}
\seealso{\code{\link{print.xtable}}, \code{\link{caption}},
  \code{\link{label}}, \code{\link{align}}, \code{\link{digits}},
  \code{\link{display}}, \code{\link{formatC}}, \code{\link{methods}}
}
\examples{

## Load example dataset
data(tli)

## Demonstrate data.frame
tli.table <- xtable(tli[1:20, ])
digits(tli.table)[c(2, 6)] <- 0
print(tli.table)
print(tli.table, type = "html")

## Demonstrate data.frame with different digits in cells
tli.table <- xtable(tli[1:20, ])
digits(tli.table) <- matrix( 0:4, nrow = 20, ncol = ncol(tli)+1 )
print(tli.table)
print(tli.table, type = "html")

## Demonstrate matrix
design.matrix <- model.matrix(~ sex*grade, data = tli[1:20, ])
design.table <- xtable(design.matrix)
print(design.table)
print(design.table, type = "html")

## Demonstrate aov
fm1 <- aov(tlimth ~ sex + ethnicty + grade + disadvg, data = tli)
fm1.table <- xtable(fm1)
print(fm1.table)
print(fm1.table, type = "html")

## Demonstrate lm
fm2 <- lm(tlimth ~ sex*ethnicty, data = tli)
fm2.table <- xtable(fm2)
print(fm2.table)
print(fm2.table, type = "html")
print(xtable(anova(fm2)))
print(xtable(anova(fm2)), type = "html")
fm2b <- lm(tlimth ~ ethnicty, data = tli)
print(xtable(anova(fm2b, fm2)))
print(xtable(anova(fm2b, fm2)), type = "html")

## Demonstrate glm
fm3 <- glm(disadvg ~ ethnicty*grade, data = tli, family = binomial())
fm3.table <- xtable(fm3)
print(fm3.table)
print(fm3.table, type = "html")
print(xtable(anova(fm3)))
print(xtable(anova(fm3)), type = "html")

## Demonstrate aov
## Taken from help(aov) in R 1.1.1
## From Venables and Ripley (1997) p.210.
N <- c(0,1,0,1,1,1,0,0,0,1,1,0,1,1,0,0,1,0,1,0,1,1,0,0)
P <- c(1,1,0,0,0,1,0,1,1,1,0,0,0,1,0,1,1,0,0,1,0,1,1,0)
K <- c(1,0,0,1,0,1,1,0,0,1,0,1,0,1,1,0,0,0,1,1,1,0,1,0)
yield <- c(49.5,62.8,46.8,57.0,59.8,58.5,55.5,56.0,62.8,55.8,69.5,55.0,
           62.0,48.8,45.5,44.2,52.0,51.5,49.8,48.8,57.2,59.0,53.2,56.0)
npk <- data.frame(block = gl(6,4), N = factor(N), P = factor(P),
                  K = factor(K), yield = yield)
npk.aov <- aov(yield ~ block + N*P*K, npk)
op <- options(contrasts = c("contr.helmert", "contr.treatment"))
npk.aovE <- aov(yield ~  N*P*K + Error(block), npk)
options(op)

summary(npk.aov)
print(xtable(npk.aov))
print(xtable(anova(npk.aov)))
print(xtable(summary(npk.aov)))

summary(npk.aovE)
print(xtable(npk.aovE), type = "html")
print(xtable(summary(npk.aovE)), type = "html")

## Demonstrate lm
## Taken from help(lm) in R 1.1.1
## Annette Dobson (1990) "An Introduction to Generalized Linear Models".
## Page 9: Plant Weight Data.
ctl <- c(4.17,5.58,5.18,6.11,4.50,4.61,5.17,4.53,5.33,5.14)
trt <- c(4.81,4.17,4.41,3.59,5.87,3.83,6.03,4.89,4.32,4.69)
group <- gl(2,10,20, labels = c("Ctl","Trt"))
weight <- c(ctl, trt)
lm.D9 <- lm(weight ~ group)
print(xtable(lm.D9))
print(xtable(anova(lm.D9)))

## Demonstrate glm
## Taken from help(glm) in R 1.1.1
## Annette Dobson (1990) "An Introduction to Generalized Linear Models".
## Page 93: Randomized Controlled Trial :
counts <- c(18,17,15,20,10,20,25,13,12)
outcome <- gl(3,1,9)
treatment <- gl(3,3)
d.AD <- data.frame(treatment, outcome, counts)
glm.D93 <- glm(counts ~ outcome + treatment, family = poisson())
print(xtable(glm.D93, align = "r|llrc"))
print(xtable(anova(glm.D93)), hline.after = c(1), size = "small")

## Demonstration of additional formatC() arguments.
print(fm1.table, format.args = list(big.mark = "'", decimal.mark = ","))

## Demonstration of "short caption" support.
fm1sc <- aov(tlimth ~ sex + ethnicty + grade, data = tli)
fm1sc.table <- xtable(fm1sc, 
  caption = c("ANOVA Model with Predictors Sex, Ethnicity, and Grade",
    "ANOVA: Sex, Ethnicity, Grade"))
print(fm1sc.table)

## Demonstration of longtable support.
## Remember to insert \usepackage{longtable} on your LaTeX preamble
x <- matrix(rnorm(1000), ncol = 10)
x.big <- xtable(x, label = 'tabbig',
                caption = 'Example of longtable spanning several pages')
print(x.big, tabular.environment = 'longtable', floating = FALSE) 
x <- x[1:30, ]
x.small <- xtable(x, label = 'tabsmall', caption = 'regular table env')
print(x.small)  # default, no longtable 

## Demonstration of sidewaystable support.
## Remember to insert \usepackage{rotating} on your LaTeX preamble
print(x.small, floating.environment = 'sidewaystable') 

if(require(stats, quietly = TRUE)) {
  ## Demonstrate prcomp
  ## Taken from help(prcomp) in mva package of R 1.1.1
  data(USArrests)
  pr1 <- prcomp(USArrests)
  print(xtable(pr1))
  print(xtable(summary(pr1)))

#  ## Demonstrate princomp
#  ## Taken from help(princomp) in mva package of R 1.1.1
#  pr2 <- princomp(USArrests)
#  print(xtable(pr2))
}

## Demonstrate include.rownames, include.colnames, 
## only.contents and add.to.row arguments
set.seed(2345)
res <- matrix(sample(0:9, size = 6*9, replace = TRUE), ncol = 6, nrow = 9)
xres <- xtable(res)
digits(xres) <- rep(0, 7)
addtorow <- list()
addtorow$pos <- list()
addtorow$pos[[1]] <- c(0, 2)
addtorow$pos[[2]] <- 4
addtorow$command <- c('\\vspace{2mm} \n', '\\vspace{10mm} \n')
print(xres, add.to.row = addtorow, include.rownames = FALSE,
      include.colnames = TRUE, only.contents = TRUE,
      hline.after = c(0, 0, 9, 9))

## Demonstrate include.rownames, include.colnames, 
## only.contents and add.to.row arguments in Rweave files

\dontrun{
 \begin{small}
 \setlongtables % For longtable version 3.x or less
 \begin{longtable}{
 <<results = tex, fig = FALSE>>=
 cat(paste(c('c', rep('cc', 34/2-1), 'c'), collapse = '@{\\hspace{2pt}}'))
 @ 
 }
 \hline
 \endhead
 \hline
 \endfoot
 <<results = tex, fig = FALSE>>=
 library(xtable)
 set.seed(2345)
 res <- matrix(sample(0:9, size = 34*90, replace = TRUE), ncol = 34, nrow = 90)
 xres <- xtable(res)
 digits(xres) <- rep(0, 35)
 addtorow <- list()
 addtorow$pos <- list()
 addtorow$pos[[1]] <- c(seq(4, 40, 5), seq(49, 85, 5))
 addtorow$pos[[2]] <- 45
 addtorow$command <- c('\\vspace{2mm} \n', '\\newpage \n')
 print(xres, add.to.row = addtorow, include.rownames = FALSE,
       include.colnames = FALSE, only.contents = TRUE, hline.after = NULL)
 @
 \end{longtable}
 \end{small}
}

## Demonstrate sanitization
mat <- round(matrix(c(0.9, 0.89, 200, 0.045, 2.0), c(1, 5)), 4)
rownames(mat) <- "$y_{t-1}$"
colnames(mat) <- c("$R^2$", "$\\\bar{R}^2$", "F-stat", "S.E.E", "DW")
print(xtable(mat), type = "latex", sanitize.text.function = function(x){x})

## Demonstrate booktabs
print(tli.table)
print(tli.table, hline.after = c(-1,0))
print(tli.table, hline.after = NULL)
print(tli.table,
      add.to.row = list(pos = list(2), command = c("\\vspace{2mm} \n")))

print(tli.table, booktabs = TRUE)
print(tli.table, booktabs = TRUE, hline.after = c(-1,0))
print(tli.table, booktabs = TRUE, hline.after = NULL)
print(tli.table, booktabs = TRUE, 
  add.to.row = list(pos = list(2), command = c("\\vspace{2mm} \n")))
print(tli.table, booktabs = TRUE, add.to.row = list(pos = list(2), 
  command = c("youhou\n")), tabular.environment = "longtable")

\testonly{
  for(i in c("latex","html")) {
    outFileName <- paste("xtable.", ifelse(i=="latex", "tex", i), sep = "")
    print(tli.table, type = i, file = outFileName, append = FALSE)
    print(design.table, type = i, file = outFileName, append = TRUE)
    print(fm1.table, type = i, file = outFileName, append = TRUE)
    print(fm2.table, type = i, file = outFileName, append = TRUE)
    print(fm2.table, type = i, file = outFileName, append = TRUE,
          math.style.negative = TRUE)
    print(xtable(anova(fm2)), type = i, file = outFileName, append = TRUE)
    print(xtable(anova(fm2b, fm2)), type = i, file = outFileName, append = TRUE)
    print(fm3.table, type = i, file = outFileName, append = TRUE)
    print(xtable(anova(fm3)), type = i, file = outFileName, append = TRUE)
    print(xtable(npk.aov), type = i, file = outFileName, append = TRUE)
    print(xtable(anova(npk.aov)), type = i, file = outFileName, append = TRUE)
    print(xtable(summary(npk.aov)), type = i, file = outFileName, append = TRUE)
    print(xtable(npk.aovE), type = i, file = outFileName, append = TRUE)
    print(xtable(summary(npk.aovE)),
          type = i, file = outFileName, append = TRUE)
    if(i=="latex") cat("\\\clearpage\n", file = outFileName, append = TRUE)
    print(xtable(lm.D9),
          type = i, file = outFileName, append = TRUE, latex.environment = NULL)
    print(xtable(lm.D9),
          type = i, file = outFileName, append = TRUE, latex.environment = "")
    print(xtable(lm.D9),
          type = i, file = outFileName, append = TRUE,
          latex.environment = "center")
    print(xtable(anova(lm.D9)), type = i, file = outFileName, append = TRUE)
    print(xtable(glm.D93), type = i, file = outFileName, append = TRUE)
    print(xtable(anova(glm.D93, test = "Chisq")),
          type = i, file = outFileName, append = TRUE)
    print(xtable(glm.D93, align = "r|llrc"),
          include.rownames = FALSE, include.colnames = TRUE,
          type = i, file = outFileName, append = TRUE)
    print(xtable(glm.D93, align = "r||llrc"),
          include.rownames = TRUE, include.colnames = FALSE,
          type = i, file = outFileName, append = TRUE)
    print(xtable(glm.D93, align = "|r||llrc"),
          include.rownames = FALSE, include.colnames = FALSE,
          type = i, file = outFileName, append = TRUE)
    print(xtable(glm.D93, align = "|r||llrc|"),
          type = i, file = outFileName, append = TRUE)
    print(xtable(anova(glm.D93)),
          hline.after = c(1), size = "small",
          type = i, file = outFileName, append = TRUE)
    if(require(stats, quietly = TRUE)) {
      print(xtable(pr1), type = i, file = outFileName, append = TRUE)
      print(xtable(summary(pr1)), type = i, file = outFileName, append = TRUE)
      # print(xtable(pr2), type = i, file = outFileName, append = TRUE)
    }
    temp.table <- xtable(ts(cumsum(1+round(rnorm(100), 2)),
                            start = c(1954, 7), frequency = 12))
    caption(temp.table) <- "Time series example"
    print(temp.table, type = i, file = outFileName,
          append = TRUE, caption.placement = "top", table.placement = "h")
    print(temp.table, type = i, file = outFileName,
          append = TRUE, caption.placement = "bottom", table.placement = "htb")
  }
} 

}
\keyword{file}
