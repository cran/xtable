### xtable 1.0-8  (2001/12/15)
###
### Produce LaTeX and HTML tables from R objects.
###
### Copyright 2000-2001 David B. Dahl <dbdahl@stat.wisc.edu>
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

xtable <- function(x,...) {
  if(is.null(class(x))) class(x) <- data.class(x)
  UseMethod("xtable",x,...)
}


## data.frame and matrix objects

xtable.data.frame <- function(x,caption=NULL,label=NULL,align=NULL,digits=NULL,display=NULL) {
  characters <- unlist(lapply(x,is.character))
  factors <- unlist(lapply(x,is.factor))

  class(x) <- c("xtable","data.frame")
  caption(x) <- caption
  label(x) <- label
  align(x) <- switch(1+is.null(align),align,c("r",c("r","l")[(characters|factors)+1]))
  digits(x) <- switch(1+is.null(digits),digits,c(0,rep(2,ncol(x))))
  display(x) <- switch(1+is.null(display),display,c("s",c("f","s")[(characters|factors)+1]))
  return(x)
}
 
xtable.matrix <- function(x,caption=NULL,label=NULL,align=NULL,digits=NULL,display=NULL) {
  return(xtable.data.frame(data.frame(x,check.names=FALSE),caption=caption,label=label,align=align,digits=digits,display=display))
}


## anova objects

xtable.anova <- function(x,caption=NULL,label=NULL,align=NULL,digits=NULL,display=NULL) {
  suggested.digits <- c(0,rep(2,ncol(x)))
  suggested.digits[grep("Pr\\(>",names(x))+1] <- 4
  suggested.digits[grep("P\\(>",names(x))+1] <- 4
  suggested.digits[grep("Df",names(x))+1] <- 0

  class(x) <- c("xtable","data.frame")
  caption(x) <- caption
  label(x) <- label
  align(x) <- switch(1+is.null(align),align,c("l",rep("r",ncol(x))))
  digits(x) <- switch(1+is.null(digits),digits,suggested.digits)
  display(x) <- switch(1+is.null(display),display,c("s",rep("f",ncol(x))))
  return(x)
}


## aov objects

xtable.aov <- function(x,caption=NULL,label=NULL,align=NULL,digits=NULL,display=NULL,...) {
  return(xtable.anova(anova(x,...),caption=caption,label=label,align=align,digits=digits,display=display))
}

xtable.summary.aov <- function(x,caption=NULL,label=NULL,align=NULL,digits=NULL,display=NULL) {
  return(xtable.anova(x[[1]],caption=caption,label=label,align=align,digits=digits,display=display))
}

xtable.aovlist <- function(x,caption=NULL,label=NULL,align=NULL,digits=NULL,display=NULL) {
  return(xtable.summary.aovlist(summary(x),caption=caption,label=label,align=align,digits=digits,display=display))
}

xtable.summary.aovlist <- function(x,caption=NULL,label=NULL,align=NULL,digits=NULL,display=NULL) {
  for(i in 1:length(x)) {
    if (i==1) result <- xtable.summary.aov(x[[i]],caption=caption,label=label,align=align,digits=digits,display=display)
    else result <- rbind(result,xtable.anova(x[[i]][[1]],caption=caption,label=label,align=align,digits=digits,display=display))
  }
  return(result)
}


## lm objects

xtable.lm <- function(x,caption=NULL,label=NULL,align=NULL,digits=NULL,display=NULL) {
  return(xtable.summary.lm(summary(x),caption=caption,label=label,align=align,digits=digits,display=display))
}

xtable.summary.lm <- function(x,caption=NULL,label=NULL,align=NULL,digits=NULL,display=NULL) {
  x <- data.frame(x$coef,check.names=FALSE)

  class(x) <- c("xtable","data.frame")
  caption(x) <- caption
  label(x) <- label
  align(x) <- switch(1+is.null(align),align,c("r","r","r","r","r"))
  digits(x) <- switch(1+is.null(digits),digits,c(0,4,4,2,4))
  display(x) <- switch(1+is.null(display),display,c("s","f","f","f","f"))
  return(x)
}


## glm objects

xtable.glm <- function(x,caption=NULL,label=NULL,align=NULL,digits=NULL,display=NULL) {
  return(xtable.summary.glm(summary(x),caption=caption,label=label,align=align,digits=digits,display=display))
}

xtable.summary.glm <- function(x,caption=NULL,label=NULL,align=NULL,digits=NULL,display=NULL) {
  return(xtable.summary.lm(x,caption=caption,label=label,align=align,digits=digits,display=display))
}


## prcomp objects

xtable.prcomp <- function(x,caption=NULL,label=NULL,align=NULL,digits=NULL,display=NULL) {
  x <- data.frame(x$rotation,check.names=FALSE)

  class(x) <- c("xtable","data.frame")
  caption(x) <- caption
  label(x) <- label
  align(x) <- switch(1+is.null(align),align,c("r",rep("r",ncol(x))))
  digits(x) <- switch(1+is.null(digits),digits,c(0,rep(4,ncol(x))))
  display(x) <- switch(1+is.null(display),display,c("s",rep("f",ncol(x))))
  return(x)
}

xtable.summary.prcomp <- function(x,caption=NULL,label=NULL,align=NULL,digits=NULL,display=NULL) {
  x <- data.frame(x$importance,check.names=FALSE)

  class(x) <- c("xtable","data.frame")
  caption(x) <- caption
  label(x) <- label
  align(x) <- switch(1+is.null(align),align,c("r",rep("r",ncol(x))))
  digits(x) <- switch(1+is.null(digits),digits,c(0,rep(4,ncol(x))))
  display(x) <- switch(1+is.null(display),display,c("s",rep("f",ncol(x))))
  return(x)
}


# Broken in R 1.4.0:  x$loadings cannot be coerce "loadings" into a "data.frame".
# ## princomp objects
# 
# xtable.princomp <- function(x,caption=NULL,label=NULL,align=NULL,digits=NULL,display=NULL) {
#   x <- data.frame(x$loadings,check.names=FALSE)
# 
#   class(x) <- c("xtable","data.frame")
#   caption(x) <- caption
#   label(x) <- label
#   align(x) <- switch(1+is.null(align),align,c("r",rep("r",ncol(x))))
#   digits(x) <- switch(1+is.null(digits),digits,c(0,rep(4,ncol(x))))
#   display(x) <- switch(1+is.null(display),display,c("s",rep("f",ncol(x))))
#   return(x)
# }

