### xtable 1.4-2  (2006/10/23)
###
### Produce LaTeX and HTML tables from R objects.
###
### Copyright 2000-2006 David B. Dahl <dahl@stat.tamu.edu>
###
### This file is part of the `xtable' library for R and related languages.
### It is made available under the terms of the GNU General Public
### License, version 2, or at your option, any later version,
### incorporated herein by reference.
###
### This program is distributed in the hope that it will be
### useful, but WITHOUT ANY WARRANTY; without even the implied
### warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
### PURPOSE.  See the GNU General Public License for more
### details.
###
### You should have received a copy of the GNU General Public
### License along with this program; if not, write to the Free
### Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
### MA 02111-1307, USA

"caption<-" <- function(x,value) UseMethod("caption<-")
"caption<-.xtable" <- function(x,value) {
  if (length(value)>1)
    stop("\"caption\" must have length 1")
  attr(x,"caption") <- value
  return(x)
}

caption <- function(x,...) UseMethod("caption")
caption.xtable <- function(x,...) {
  return(attr(x,"caption"))
}

"label<-" <- function(x,value) UseMethod("label<-")
"label<-.xtable" <- function(x,value) {
  if (length(value)>1)
    stop("\"label\" must have length 1")
  attr(x,"label") <- value
  return(x)
}

label <- function(x,...) UseMethod("label")
label.xtable <- function(x,...) {
  return(attr(x,"label"))
}

"align<-" <- function(x,value) UseMethod("align<-")
"align<-.xtable" <- function(x,value) {
# Based on contribution from Benno <puetz@mpipsykl.mpg.de> in e-mail dated Wednesday, December 01, 2004
  # cat("%",value,"\n")
  if ( (!is.null(value)) && ( is.character(value) ) && ( length(value) == 1 ) && ( nchar(value) > 1 ) ) {
    value <- strsplit(value,"")[[1]]
  }
  if (!all(!is.na(match(value,c("r","l","c","|"))))){
    stop("\"align\" must be containing elements of {\"r\",\"l\",\"c\",\"|\"}")
  }
  c.value <- if (any(!is.na(match(value,"|")))) {
                value[-which(value=='|')]
             } else {
                value
             }
  if (length(c.value)!=ncol(x)+1)
      stop(paste("\"align\" must have length equal to",ncol(x)+1,"( ncol(x) + 1 )"))

  attr(x,"align") <- value
  return(x)
}

align <- function(x,...) UseMethod("align")
align.xtable <- function(x,...) {
  return(attr(x,"align"))
}

"digits<-" <- function(x,value) UseMethod("digits<-")
"digits<-.xtable" <- function(x,value) {
  if( is.matrix( value ) ) {
    if( ncol( value ) != ncol(x)+1 || nrow( value ) != nrow(x) ) {
      stop( "if argument 'digits' is a matrix, it must have columns equal",
        " to ", ncol(x)+1, " ( ncol(x) + 1 ) and rows equal to ", nrow(x),
        " ( nrow( x )" )
    }
  } else {
    if( length( value ) != ncol(x)+1 ) {
      stop( "if argument 'digits' is a vector, it must have length equal",
        " to", ncol(x)+1, " ( ncol(x) + 1 )" )
    }
  }
  if (!is.numeric(value))
    stop("\"digits\" must be numeric")
  attr(x,"digits") <- value
  return(x)
}

digits <- function(x,...) UseMethod("digits")
digits.xtable <- function(x,...) {
  return(attr(x,"digits"))
}

"display<-" <- function(x,value) UseMethod("display<-")
"display<-.xtable" <- function(x,value) {
  if (length(value)!=ncol(x)+1)
    stop(paste("\"display\" must have length equal to",ncol(x)+1,"( ncol(x) + 1 )"))
  if (!all(!is.na(match(value,c("d","f","e","E","g","G","fg","s")))))
    stop("\"display\" must be in {\"d\",\"f\",\"e\",\"E\",\"g\",\"G\", \"fg\", \"s\"}")
  attr(x,"display") <- value
  return(x)
}

display <- function(x,...) UseMethod("display")
display.xtable <- function(x,...) {
  return(attr(x,"display"))
}

