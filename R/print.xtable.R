### xtable package
###
### Produce LaTeX and HTML tables from R objects.
###
### Copyright 2000-2007 David B. Dahl <dahl@stat.tamu.edu>
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
print.xtable <- function(
  x,
  type="latex",
  file="",
  append=FALSE,
  floating=TRUE,
  floating.environment="table",
  table.placement="ht",
  caption.placement="bottom",
  latex.environments=c("center"),
  tabular.environment="tabular",
  size=NULL,
  hline.after=c(-1,0,nrow(x)),
  NA.string="",
  include.rownames=TRUE,
  include.colnames=TRUE,
  only.contents=FALSE,
  add.to.row=NULL,
  sanitize.text.function=NULL,
  sanitize.rownames.function=sanitize.text.function,
  sanitize.colnames.function=sanitize.text.function,
  math.style.negative=FALSE,
  html.table.attributes="border=1",
  ...) {
  # Claudio Agostinelli <claudio@unive.it> dated 2006-07-28 hline.after
  # By default it print an \hline before and after the columns names independently they are printed or not and at the end of the table
  # Old code that set hline.after should include c(-1, 0, nrow(x)) in the hline.after vector
  # If you do not want any \hline inside the data, set hline.after to NULL 
  # PHEADER instead the string '\\hline\n' is used in the code
  # Now hline.after counts how many time a position appear  
  # I left an automatic PHEADER in the longtable is this correct?

  # Claudio Agostinelli <claudio@unive.it> dated 2006-07-28 include.rownames, include.colnames  
  pos <- 0
  if (include.rownames) pos <- 1
  
  # Claudio Agostinelli <claudio@unive.it> dated 2006-07-28 hline.after checks
  if (any(hline.after < -1) | any(hline.after > nrow(x))) stop("'hline.after' must be inside [-1, nrow(x)]")
  
  # Claudio Agostinelli <claudio@unive.it> dated 2006-07-28 add.to.row checks
  if (!is.null(add.to.row)) {
    if (is.list(add.to.row) && length(add.to.row)==2) {
      if (is.null(names(add.to.row))) {
        names(add.to.row) <- c('pos', 'command')
      } else if (any(sort(names(add.to.row))!=c('command', 'pos'))) {
        stop("the names of the elements of 'add.to.row' must be 'pos' and 'command'")
      }
      if (is.list(add.to.row$pos) && is.vector(add.to.row$command, mode='character')) {
        if ((npos <- length(add.to.row$pos)) != length(add.to.row$command)) {
          stop("the length of 'add.to.row$pos' must be equal to the length of 'add.to.row$command'")
        }
        if (any(unlist(add.to.row$pos) < -1) | any(unlist(add.to.row$pos) > nrow(x))) {
          stop("the values in add.to.row$pos must be inside the interval [-1, nrow(x)]")
        }
      } else {
        stop("the first argument ('pos') of 'add.to.row' must be a list, the second argument ('command') must be a vector of mode character")
      }
    } else {
      stop("'add.to.row' argument must be a list of length 2")
    }
  } else {
     add.to.row <- list(pos=list(), command=vector(length=0, mode="character"))
     npos <- 0
  }

  # Claudio Agostinelli <claudio@unive.it> dated 2006-07-28 add.to.row
  # Add further commands at the end of rows
  if (type=="latex") {
     PHEADER <- "\\hline\n"
  } else {
     PHEADER <- ""
  }
   
  lastcol <- rep(" ", nrow(x)+2)
  if (!is.null(hline.after)) {
     add.to.row$pos[[npos+1]] <- hline.after
     add.to.row$command <- c(add.to.row$command, PHEADER)
  }
  if ( length(add.to.row$command) > 0 ) {
    for (i in 1:length(add.to.row$command)) {
      addpos <- add.to.row$pos[[i]]
      freq <- table(addpos)
      addpos <- unique(addpos)
      for (j in 1:length(addpos)) {
        lastcol[addpos[j]+2] <- paste(lastcol[addpos[j]+2], paste(rep(add.to.row$command[i], freq[j]), sep="", collapse=""), sep=" ")
      }
    }
  }
  
  if (length(type)>1) stop("\"type\" must have length 1")
  type <- tolower(type)
  if (!all(!is.na(match(type,c("latex","html"))))) stop("\"type\" must be in {\"latex\", \"html\"}")
  if (!all(!is.na(match(floating.environment,c("table","table*","sidewaystable"))))) stop("\"type\" must be in {\"table\", \"table*\", \"sidewaystable\"}")
  if (!all(!is.na(match(unlist(strsplit(table.placement, split="")),c("H","h","t","b","p","!"))))) {
    stop("\"table.placement\" must contain only elements of {\"h\",\"t\",\"b\",\"p\",\"!\"}")
  }
  if (!all(!is.na(match(caption.placement,c("bottom","top"))))) stop("\"caption.placement\" must be either {\"bottom\",\"top\"}")

  if (type=="latex") {
    BCOMMENT <- "% "
    ECOMMENT <- "\n"
    # See e-mail from "John S. Walker <jsw9c@uic.edu>" dated 5-19-2003 regarding "texfloat"
    # See e-mail form "Fernando Henrique Ferraz P. da Rosa" <academic@feferraz.net>" dated 10-28-2005 regarding "longtable"
    if ( tabular.environment == "longtable" & floating == TRUE ) {
      warning("Attempt to use \"longtable\" with floating=TRUE. Changing to FALSE.")
      floating <- FALSE
    }
    if ( floating == TRUE ) {
      # See e-mail from "Pfaff, Bernhard <Bernhard.Pfaff@drkw.com>" dated 7-09-2003 regarding "suggestion for an amendment of the source"
      # See e-mail from "Mitchell, David" <David.Mitchell@dotars.gov.au>" dated 2003-07-09 regarding "Additions to R xtable package"
      # See e-mail from "Garbade, Sven" <Sven.Garbade@med.uni-heidelberg.de> dated 2006-05-22 regarding the floating environment.
      BTABLE <- paste("\\begin{", floating.environment, "}",ifelse(!is.null(table.placement),
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
      ETABLE <- paste("\\end{", floating.environment, "}\n", sep="")
    }
    else {
      BTABLE <- ""
      ETABLE <- ""
      BENVIRONMENT <- ""
      EENVIRONMENT <- ""
    }

    tmp.index.start <- 1
    if ( ! include.rownames ) {
      while ( attr(x,"align",exact=TRUE)[tmp.index.start] == '|' ) tmp.index.start <- tmp.index.start + 1
      tmp.index.start <- tmp.index.start + 1
    }
    BTABULAR <- paste("\\begin{",tabular.environment,"}{",
                      paste(c(attr(x, "align",exact=TRUE)[tmp.index.start:length(attr(x,"align",exact=TRUE))], "}\n"),
                            sep="", collapse=""),
                      sep="")
    
    ## fix 10-26-09 (robert.castelo@upf.edu) the following 'if' condition is added here to support
    ## a caption on the top of a longtable
    if (tabular.environment == "longtable" && caption.placement=="top") {
        BCAPTION <- "\\caption{"
        ECAPTION <- "} \\\\ \n"
        if ((!is.null(attr(x,"caption",exact=TRUE))) && (type=="latex")) BTABULAR <- paste(BTABULAR,  BCAPTION, attr(x,"caption",exact=TRUE), ECAPTION, sep="")
    }
    # Claudio Agostinelli <claudio@unive.it> dated 2006-07-28 add.to.row position -1
    BTABULAR <- paste(BTABULAR,lastcol[1], sep="")
    # the \hline at the end, if present, is set in full matrix    
    ETABULAR <- paste("\\end{",tabular.environment,"}\n",sep="")
    
    # BSIZE contributed by Benno <puetz@mpipsykl.mpg.de> in e-mail dated Wednesday, December 01, 2004
    if (is.null(size) || !is.character(size)) {
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
    EROW <- " \\\\ \n"
    BTH <- ""
    ETH <- ""
    STH <- " & "
    BTD1 <- " & "
    BTD2 <- ""
    BTD3 <- ""
    ETD  <- ""
    # Based on contribution from Jonathan Swinton <jonathan@swintons.net> in e-mail dated Wednesday, January 17, 2007
    sanitize <- function(str) {
      result <- str
      result <- gsub("\\\\","SANITIZE.BACKSLASH",result)
      result <- gsub("$","\\$",result,fixed=TRUE)
      result <- gsub(">","$>$",result,fixed=TRUE)
      result <- gsub("<","$<$",result,fixed=TRUE)
      result <- gsub("|","$|$",result,fixed=TRUE)
      result <- gsub("{","\\{",result,fixed=TRUE)
      result <- gsub("}","\\}",result,fixed=TRUE)
      result <- gsub("%","\\%",result,fixed=TRUE)
      result <- gsub("&","\\&",result,fixed=TRUE)
      result <- gsub("_","\\_",result,fixed=TRUE)
      result <- gsub("#","\\#",result,fixed=TRUE)
      result <- gsub("^","\\verb|^|",result,fixed=TRUE)
      result <- gsub("~","\\~{}",result,fixed=TRUE)
      result <- gsub("SANITIZE.BACKSLASH","$\\backslash$",result,fixed=TRUE)
      return(result)
    }
    sanitize.numbers <- function(x) {
      result <- x
      if ( math.style.negative ) {
        # Jake Bowers <jwbowers@illinois.edu> in e-mail from 2008-08-20 suggested
        # disabling this feature to avoid problems with LaTeX's dcolumn package.
        # by Florian Wickelmaier <florian.wickelmaier@uni-tuebingen.de> in e-mail
        # from 2008-10-03 requested the ability to use the old behavior.
        for(i in 1:length(x)) {
          result[i] <- gsub("-","$-$",result[i],fixed=TRUE)
        }
      }
      return(result)
    }
    sanitize.final <- function(result) {
      return(result)
    }
  } else {
    BCOMMENT <- "<!-- "
    ECOMMENT <- " -->\n"
    BTABLE <- paste("<TABLE ",html.table.attributes,">\n",sep="")
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
    BTD1 <- " <TD align=\""
    align.tmp <- attr(x,"align",exact=TRUE)
    align.tmp <- align.tmp[align.tmp!="|"]
    BTD2 <- matrix(align.tmp[(2-pos):(ncol(x)+1)],nrow=nrow(x),ncol=ncol(x)+pos,byrow=TRUE)
    # Based on contribution from Jonathan Swinton <jonathan@swintons.net> in e-mail dated Wednesday, January 17, 2007
    BTD2[regexpr("^p",BTD2)>0] <- "left"
    BTD2[BTD2=="r"] <- "right"
    BTD2[BTD2=="l"] <- "left"
    BTD2[BTD2=="c"] <- "center"
    BTD3 <- "\"> "
    ETD  <- " </TD>"
    sanitize <- function(str) {
      result <- str
      result <- gsub("&","&amp ",result,fixed=TRUE)
      result <- gsub(">","&gt ",result,fixed=TRUE)
      result <- gsub("<","&lt ",result,fixed=TRUE)
      # Kurt Hornik <Kurt.Hornik@wu-wien.ac.at> on 2006/10/05 recommended not escaping underscores.
      # result <- gsub("_", "\\_", result, fixed=TRUE)
      return(result)
    }
    sanitize.numbers <- function(x) {
      return(x)
    }
    sanitize.final <- function(result) {
      # Suggested by Uwe Ligges <ligges@statistik.uni-dortmund.de> in e-mail dated 2005-07-30.
      result$text <- gsub("  *"," ", result$text,fixed=TRUE)
      result$text <- gsub(' align="left"', "", result$text,fixed=TRUE)
      return(result)
    }
  }

  result <- string("",file=file,append=append)
  info <- R.Version()
  # modified Claudio Agostinelli <claudio@unive.it> dated 2006-07-28 to set automatically the package version
  result <- result + BCOMMENT + type + " table generated in " +
            info$language + " " + info$major + "." + info$minor + " by xtable " + packageDescription('xtable')$Version + " package" + ECOMMENT
  result <- result + BCOMMENT + date() + ECOMMENT
  # Claudio Agostinelli <claudio@unive.it> dated 2006-07-28 only.contents
  if (!only.contents) {
    result <- result + BTABLE
    result <- result + BENVIRONMENT
    if ( floating == TRUE ) {
      if ((!is.null(attr(x,"caption",exact=TRUE))) && (type=="html" || caption.placement=="top")) result <- result + BCAPTION + attr(x,"caption",exact=TRUE) + ECAPTION
      if (!is.null(attr(x,"label",exact=TRUE)) && (type=="latex" && caption.placement=="top")) result <- result + BLABEL + attr(x,"label",exact=TRUE) + ELABEL  
    }
    result <- result + BSIZE
    result <- result + BTABULAR
  }
  # Claudio Agostinelli <claudio@unive.it> dated 2006-07-28 include.colnames, include.rownames 
  if (include.colnames) {
    result <- result + BROW + BTH
    if (include.rownames) result <- result + STH
    if (is.null(sanitize.colnames.function)) {                                     # David G. Whiting in e-mail 2007-10-09
      result <- result + paste(sanitize(names(x)),collapse=STH)
    } else {
      result <- result + paste(sanitize.colnames.function(names(x)), collapse=STH) # David G. Whiting in e-mail 2007-10-09
    }
    result <- result + ETH + EROW
  }

  cols <- matrix("",nrow=nrow(x),ncol=ncol(x)+pos)
  if (include.rownames) {
    if (is.null(sanitize.rownames.function)) {                                     # David G. Whiting in e-mail 2007-10-09
      cols[,1] <- sanitize(row.names(x))
    } else {
      cols[,1] <- sanitize.rownames.function(row.names(x))                         # David G. Whiting in e-mail 2007-10-09
    }
  }

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
  if( !is.matrix( attr( x, "digits",exact=TRUE ) ) ) {
    # modified Claudio Agostinelli <claudio@unive.it> dated 2006-07-28
    attr(x,"digits") <- matrix( attr( x, "digits",exact=TRUE ), nrow = nrow(x), ncol = ncol(x)+1, byrow = TRUE )
  }
  for(i in 1:ncol(x)) {
    ina <- is.na(x[,i])
    is.numeric.column <- is.numeric(x[,i])
    for( j in 1:nrow( cols ) ) {
      ### modified Claudio Agostinelli <claudio@unive.it> dated 2009-09-14
      ### add decimal.mark=options()$OutDec
      cols[j,i+pos] <-
        formatC( disp( x[j,i] ),
          format = ifelse( attr( x, "digits",exact=TRUE )[j,i+1] < 0, "E", attr( x, "display",exact=TRUE )[i+1] ), digits = abs( attr( x, "digits",exact=TRUE )[j,i+1] ), decimal.mark=options()$OutDec)
    }
    if ( any(ina) ) cols[ina,i+pos] <- NA.string
    # Based on contribution from Jonathan Swinton <jonathan@swintons.net> in e-mail dated Wednesday, January 17, 2007
    if ( is.numeric.column ) {
      cols[,i+pos] <- sanitize.numbers(cols[,i+pos])
    } else {
      if (is.null(sanitize.text.function)) {
        cols[,i+pos] <- sanitize(cols[,i+pos])
      } else {
        cols[,i+pos] <- sanitize.text.function(cols[,i+pos])
      }
    }
  }

  multiplier <- 5
  full <- matrix("",nrow=nrow(x),ncol=multiplier*(ncol(x)+pos)+2)
  full[,1] <- BROW
  full[,multiplier*(0:(ncol(x)+pos-1))+2] <- BTD1
  full[,multiplier*(0:(ncol(x)+pos-1))+3] <- BTD2
  full[,multiplier*(0:(ncol(x)+pos-1))+4] <- BTD3
  full[,multiplier*(0:(ncol(x)+pos-1))+5] <- cols
  full[,multiplier*(0:(ncol(x)+pos-1))+6] <- ETD

  full[,multiplier*(ncol(x)+pos)+2] <- paste(EROW, lastcol[-(1:2)], sep=" ")
  if (type=="latex") full[,2] <- ""
  result <- result + lastcol[2] + paste(t(full),collapse="")
  if (!only.contents) {
    if (tabular.environment == "longtable") {
      result <- result + PHEADER
      ## fix 10-27-09 Liviu Andronic (landronimirc@gmail.com) the following 'if' condition is inserted in order to avoid
      ## that bottom caption interferes with a top caption of a longtable
      if(caption.placement=="bottom"){
        if ((!is.null(attr(x,"caption",exact=TRUE))) && (type=="latex")) result <- result + BCAPTION + attr(x,"caption",exact=TRUE) + ECAPTION
      }
      if (!is.null(attr(x,"label",exact=TRUE))) result <- result + BLABEL + attr(x,"label",exact=TRUE) + ELABEL
      ETABULAR <- "\\end{longtable}\n"
    }
    result <- result + ETABULAR
    result <- result + ESIZE
    if ( floating == TRUE ) {
      if ((!is.null(attr(x,"caption",exact=TRUE))) && (type=="latex" && caption.placement=="bottom")) result <- result + BCAPTION + attr(x,"caption",exact=TRUE) + ECAPTION
      if (!is.null(attr(x,"label",exact=TRUE)) && caption.placement=="bottom") result <- result + BLABEL + attr(x,"label",exact=TRUE) + ELABEL  
    }
    result <- result + EENVIRONMENT
    result <- result + ETABLE
  }   
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
  if (is.null(attr(x,"class",exact=TRUE)))
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

