### xtable 1.3-0  (2005/11/02)
###
### Produce LaTeX and HTML tables from R objects.
###
### Copyright 2000-2005 David B. Dahl <dahl@stat.tamu.edu>
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
print.xtable <- function(x,type="latex",file="",append=FALSE,floating=TRUE,table.placement="ht",caption.placement="bottom",latex.environments=c("center"),tabular.environment="tabular",size=NULL,hline.after=NULL,NA.string="",...) {

  if (length(type)>1)
    stop("\"type\" must have length 1")
  type <- tolower(type)
  if (!all(!is.na(match(type,c("latex","html")))))
    stop("\"type\" must be in {\"latex\", \"html\"}")
  if (!all(!is.na(match(unlist(strsplit(table.placement, split="")),c("H","h","t","b","p","!")))))
    stop("\"table.placement\" must contain only elements of {\"h\",\"t\",\"b\",\"p\",\"!\"}")
  if (!all(!is.na(match(caption.placement,c("bottom","top")))))
    stop("\"caption.placement\" must be either {\"bottom\",\"top\"}")

  if (type=="latex") {
    BCOMMENT <- "% "
    ECOMMENT <- "\n"
# See e-mail from "John S. Walker <jsw9c@uic.edu>" dated 5-19-2003 regarding "texfloat"
# See e-mail form "Fernando Henrique Ferraz P. da Rosa" <academic@feferraz.net>" dated 10-28-2005 regarding "longtable"
    if ( tabular.environment == "longtable" & floating == TRUE ) {
      warning("Attempt to use \"longtable\" with floating=TRUE. Changing to FALSE.")
      floating <- FALSE
    }
    if ( tabular.environment == "longtable" & caption.placement == "top" ) {
      warning("Attempt to use \"longtable\" with caption.placement=\"top\". Changing to \"bottom\".")
      caption.placement <- "bottom"
    }
    if ( floating == TRUE ) {
      # See e-mail from "Pfaff, Bernhard <Bernhard.Pfaff@drkw.com>" dated 7-09-2003 regarding "suggestion for an amendment of the source"
      # See e-mail from "Mitchell, David" <David.Mitchell@dotars.gov.au>" dated 2003-07-09 regarding "Additions to R xtable package"
      BTABLE <- paste("\\begin{table}",ifelse(!is.null(table.placement),
        paste("[",table.placement,"]",sep=""),""),"\n",sep="")
      if ( is.null(latex.environments) || (length(latex.environments)==0) ) {
        BENVIRONMENT <- ""
        EENVIRONMENT <- ""
      }
      else {
        BENVIRONMENT <- ""
        EENVIRONMENT <- ""
        for ( i in 1:length(latex.environments) ) {
          if ( latex.environments[i] == "" ) next
          BENVIRONMENT <- paste(BENVIRONMENT, "\\begin{",latex.environments[i],"}\n",sep="")
          EENVIRONMENT <- paste("\\end{",latex.environments[i],"}\n",EENVIRONMENT,sep="")
        }
      }
      ETABLE <- "\\end{table}\n"
    }
    else {
      BTABLE <- ""
      ETABLE <- ""
      BENVIRONMENT <- ""
      EENVIRONMENT <- ""
    }
#    BTABULAR <- string("\\begin{tabular}{|") + paste(attr(x,"align"),collapse="|") + "|}\n\\hline\n"
#    See e-mail from "BXC (Bendix Carstensen)" <bxc@novonordisk.com> dated Mon, 27 Aug 2001 10:11:54 +0200
    BTABULAR <- paste("\\begin{",tabular.environment,"}{",
                      paste(attr(x, "vsep"),
                            c(attr(x, "align"), "}\n\\hline\n"),
                            sep="", collapse=""),
                      sep="")
    ETABULAR <- paste("\\hline\n\\end{",tabular.environment,"}\n",sep="")
# BSIZE contributed by Benno PÃ¼tz <puetz@mpipsykl.mpg.de> in e-mail dated Wednesday, December 01, 2004
    if (is.null(size) || !is.character(size)){
      BSIZE <- ""
      ESIZE <- ""
    } else {
      if(length(grep("^\\\\",size))==0){
        size <- paste("\\",size,sep="")
      }
      BSIZE <- paste("{",size,"\n",sep="")
      ESIZE <- "}\n"
    }
    BLABEL <- "\\label{"
    ELABEL <- "}\n"
    BCAPTION <- "\\caption{"
    ECAPTION <- "}\n"
    BROW <- ""
    EROW <- " \\\\\n"
    BTH <- ""
    ETH <- ""
    STH <- " & "
    PHEADER <- "\\hline\n"
    BTD1 <- " & "
    BTD2 <- ""
    BTD3 <- ""
    ETD  <- ""
    sanitize <- function(str) {
      result <- str
      result <- gsub(">","$>$",result)
      result <- gsub("<","$<$",result)
      result <- gsub("\\|","$\|$",result)
      return(result)
    }
    sanitize.numbers <- function(x) {
      result <- x
      for(i in 1:length(x)) {
        result[i] <- gsub("-","$-$",result[i])
      }
      return(result)
    }
    sanitize.final <- function(result) {
      return(result)
    }
 } else {
    BCOMMENT <- "<!-- "
    ECOMMENT <- " -->\n"
    BTABLE <- "<TABLE border=1>\n"
    ETABLE <- "</TABLE>\n"
    BENVIRONMENT <- ""
    EENVIRONMENT <- ""
    BTABULAR <- ""
    ETABULAR <- ""
    BSIZE <- ""
    ESIZE <- ""
    BLABEL <- "<A NAME="
    ELABEL <- "></A>\n"
    BCAPTION <- paste("<CAPTION ALIGN=\"",caption.placement,"\"> ",sep="")
    ECAPTION <- " </CAPTION>\n"
    BROW <- "<TR>"
    EROW <- " </TR>\n"
    BTH <- " <TH> "
    ETH <- " </TH> "
    STH <- " </TH> <TH> "
    PHEADER <- ""
    BTD1 <- " <TD align=\""
    align.tmp <- attr(x,"align")
    align.tmp <- align.tmp[align.tmp!="|"]
    BTD2 <- matrix(align.tmp,nrow=nrow(x),ncol=ncol(x)+1,byrow=TRUE)
    BTD2[BTD2=="r"] <- "right"
    BTD2[BTD2=="l"] <- "left"
    BTD2[BTD2=="c"] <- "center"
    BTD3 <- "\"> "
    ETD  <- " </TD>"
    sanitize <- function(str) {
      result <- str
      result <- gsub("&","&amp ",result)
      result <- gsub(">","&gt ",result)
      result <- gsub("<","&lt ",result)
      result <- gsub("_", "\\_", result, fixed=TRUE)
      return(result)
    }
    sanitize.numbers <- function(x) {
      return(x)
    }
    sanitize.final <- function(result) {
      # Suggested by Uwe Ligges <ligges@statistik.uni-dortmund.de> in e-mail dated 2005-07-30.
      result$text <- gsub("  *"," ", result$text)
      result$text <- gsub(' align="left"', "", result$text)
      return(result)
    }
  }

  result <- string("",file=file,append=append)
  info <- R.Version()
  result <- result + BCOMMENT + type + " table generated in " +
            info$language + " " + info$major + "." + info$minor + " by xtable 1.3-0 package" + ECOMMENT
  result <- result + BCOMMENT + date() + ECOMMENT
  result <- result + BTABLE
  result <- result + BENVIRONMENT
  if ( floating == TRUE ) {
    if ((!is.null(attr(x,"caption"))) && (type=="html" || caption.placement=="top")) result <- result + BCAPTION + attr(x,"caption") + ECAPTION
    if (!is.null(attr(x,"label")) && (type=="latex" && caption.placement=="top")) result <- result + BLABEL + attr(x,"label") + ELABEL  
  }
  result <- result + BSIZE
  result <- result + BTABULAR
  result <- result + BROW + BTH + STH + paste(sanitize(names(x)),collapse=STH) + ETH + EROW
  result <- result + PHEADER

  cols <- matrix("",nrow=nrow(x),ncol=ncol(x)+1)
  cols[,1] <- row.names(x)
  disp <- function(y) {
    if (is.factor(y)) {
      y <- levels(y)[y]
    }
    if (is.list(y)) {
      y <- unlist(y)
    }
    return(y)
  }
  # Code for letting "digits" be a matrix was provided by Arne Henningsen <ahenningsen@agric-econ.uni-kiel.de> in e-mail dated 2005-06-04.
  if( !is.matrix( attr( x, "digits" ) ) ) {
    attr(x,"digits") <- matrix( attr( x, "digits" ),
      nrow = nrow( cols ), ncol = ncol( cols ), byrow = TRUE )
  }
  for(i in 1:ncol(x)) {
    ina <- is.na(x[,i])
    is.numeric.column <- is.numeric(x[,i])
    for( j in 1:nrow( cols ) ) {
      cols[j,i+1] <-
        formatC( disp( x[j,i] ),
          format = ifelse( attr( x, "digits" )[j,i+1] < 0, "E",
            attr( x, "display" )[i+1] ),
          digits = abs( attr( x, "digits" )[j,i+1] ) )
    }
    if (any(ina)) cols[ina,i+1] <- NA.string
    if ( is.numeric.column ) {
      cols[,i+1] <- sanitize.numbers(cols[,i+1])
    }
  }

  multiplier <- 5
  full <- matrix("",nrow=nrow(x),ncol=multiplier*(ncol(x)+1)+2)
  full[,1] <- BROW
  full[,multiplier*(0:ncol(x))+2] <- BTD1
  full[,multiplier*(0:ncol(x))+3] <- BTD2
  full[,multiplier*(0:ncol(x))+4] <- BTD3
  full[,multiplier*(0:ncol(x))+5] <- cols
  full[,multiplier*(0:ncol(x))+6] <- ETD
# hline.after contributed by Benno PÃ¼tz <puetz@mpipsykl.mpg.de> in e-mail dated Wednesday, December 01, 2004
  full[,multiplier*(ncol(x)+1)+2] <- ifelse(1:nrow(x) %in% hline.after,paste(EROW,PHEADER,sep=""),EROW)
  if (type=="latex") full[,2] <- ""

  result <- result + paste(t(full),collapse="")

  if ( tabular.environment == "longtable") { 
    result <- result + "\\hline\n"
    if ((!is.null(attr(x,"caption"))) && (type=="latex")) result <- result + BCAPTION + attr(x,"caption") + ECAPTION
    if (!is.null(attr(x,"label"))) result <- result + BLABEL + attr(x,"label") + ELABEL
    ETABULAR <- "\\end{longtable}\n"
  }
  result <- result + ETABULAR
  result <- result + ESIZE
  if ( floating == TRUE ) {
    if ((!is.null(attr(x,"caption"))) && (type=="latex" && caption.placement=="bottom")) result <- result + BCAPTION + attr(x,"caption") + ECAPTION
    if (!is.null(attr(x,"label")) && caption.placement=="bottom") result <- result + BLABEL + attr(x,"label") + ELABEL  
  }
  result <- result + EENVIRONMENT
  result <- result + ETABLE
  result <- sanitize.final(result)
  print(result)

  return(invisible(result$text))
}

"+.string" <- function(x,y) {
  x$text <- paste(x$text,as.string(y)$text,sep="")
  return(x)
}

print.string <- function(x,...) {
  cat(x$text,file=x$file,append=x$append)
  return(invisible())
}

string <- function(text,file="",append=FALSE) {
  x <- list(text=text,file=file,append=append)
  class(x) <- "string"
  return(x)
}

as.string <- function(x,file="",append=FALSE) {
  if (is.null(attr(x,"class")))
  switch(data.class(x),
      character=return(string(x,file,append)),
      numeric=return(string(as.character(x),file,append)),
      stop("Cannot coerse argument to a string"))
  if (class(x)=="string")
    return(x)
  stop("Cannot coerse argument to a string")
}

is.string <- function(x) {
  return(class(x)=="string")
}

