gettext  <- function(...) base::gettext(..., domain = "Rz")
gettextf <- function(...) base::gettextf(..., domain = "Rz")

#if(grepl("darwin",R.Version()$os) formals(gettext)$domain <- NULL
#if(grepl("darwin",R.Version()$os) formals(gettextf)$domain <- NULL

fixTranslations <- function(w){
  if ("GtkLabel" %in% class(w))
    w$setLabel(gettext(w$getLabel()))
  else if ("GtkNotebook" %in% class(w))
    lapply(gtkChildren(w),
           function(wc)
             w$getTabLabel(wc)$setLabel(gettext(w$getTabLabelText(wc))))

  if ("GtkContainer" %in% class(w))
    lapply(gtkChildren(w), fixTranslations)
  
  return()
}

fileCheck <- function(filename, parent){
  if (file.exists(filename)){
    dialog <- gtkMessageDialogNew(parent, "destroy-with-parent",
                                   GtkMessageType["question"],
                                   GtkButtonsType["ok-cancel"],
                                   paste(gettext("File Path: "), filename, "\n",
                                         gettext("This file already exists. Overwrite it?"), sep=""))
    response <- dialog$run()
    dialog$hide()
    if (response==GtkResponseType["ok"]){
      return(TRUE)
    } else {
      return(FALSE)
    }
  } else {
    return(TRUE)
  }
}

#script.buffer <-
#setRefClass("RzScriptBuffer",
#  fields = c("script", "buffer")
#  )

#sb.obj <- script.buffer$new(script="test", buffer=gtkTextBufferNew())

localize <- function(char) iconv(char, "UTF-8", localeToCharset()[1])

gtkProgressBarStart <- function(progress.bar){
  progress.bar$Pulse()
  return(TRUE)
}

gtkFileChooserDialogFilteredNew <- function(title, parent=NULL,
                                            action=GtkFileChooserAction["open"],
                                            file.type.list){
  dialog <- gtkFileChooserDialogNew(title=title, parent=parent,
                                    action=action,
                                    "gtk-open", GtkResponseType["accept"],
                                    "gtk-cancel", GtkResponseType["cancel"], 
                                    show=FALSE)
  for (i in seq_along(file.type.list)) {
    filter <- gtkFileFilterNew()
    filter$setName(file.type.list[[i]]$name)
    filter$addPattern(file.type.list[[i]]$pattern)
    dialog$addFilter(filter)
  }
  class(dialog) <- c("GtkFileChooserDialogFiltered", class(dialog))
  return(dialog)
}

gtkFileChooserDialogFilteredActivate <- function(obj){
  if (obj$run() == GtkResponseType["accept"]) {
    filename <- localize(obj$getFilename())
    filetype <- localize(obj$getFilter()$getName())
    return(list(filename=filename, filetype=filetype))
  } else {
    return(NULL)
  }
}

gtkFileChooserDialogFilteredRun <- function(obj) gtkDialogRun(obj)

Rz <- function(...){
  main.obj <- new("RzMain")
}

gtkInfoBarRzNew <- function(show=TRUE){
    obj <- gtkInfoBarNew(show=TRUE)
    class(obj) <- c("gtkInfoBarRz", class(obj))
    return(obj)
}
gtkInfoBarRzNew()

gtkInfoBarRzSetText <- function(obj, txt){
  label <- obj$getContentArea()$getChildren()[[1]]
  label$setText(txt)
}

write.spss <-
function (df, datafile, codefile, varlabels, varnames = NULL) {
    dfn <- lapply(df, function(x) if (is.factor(x)) as.numeric(x) else x)
    write.table(dfn, file = datafile, row.names = FALSE, col.names = FALSE, 
        sep = ",", quote = FALSE, na = "", eol = ",\n")
    if (is.null(varnames)) {
        varnames <- names(df)
    }
    varnames <- gsub("[^[:alnum:]_\\$@#]", "\\.", varnames)
    dl.varnames <- varnames
    if (any(chv <- sapply(df, is.character))) {
        lengths <- sapply(df[chv], function(v) max(nchar(v)))
        if (any(lengths > 255L)) 
            stop("Cannot handle character variables longer than 255")
        lengths <- paste("(A", lengths, ")", sep = "")
        star <- ifelse(c(FALSE, diff(which(chv) > 1L)), " *", 
            " ")
        dl.varnames[chv] <- paste(star, dl.varnames[chv], lengths)
    }
    cat("DATA LIST FILE=", foreign:::adQuote(datafile), " free (\",\")\n", 
        file = codefile)
    cat("/", dl.varnames, " .\n\n", file = codefile, append = TRUE)
    cat("VARIABLE LABELS\n", file = codefile, append = TRUE)
    cat(paste(varnames, foreign:::adQuote(varlabels), "\n"), ".\n", file = codefile, 
        append = TRUE)
    factors <- sapply(df, is.factor)
    if (any(factors)) {
        cat("\nVALUE LABELS\n", file = codefile, append = TRUE)
        for (v in which(factors)) {
            cat("/\n", file = codefile, append = TRUE)
            cat(varnames[v], " \n", file = codefile, append = TRUE)
            levs <- levels(df[[v]])
            cat(paste(seq_along(levs), foreign:::adQuote(levs), "\n", sep = " "), 
                file = codefile, append = TRUE)
        }
        cat(".\n", file = codefile, append = TRUE)
    }
    cat("\nEXECUTE.\n", file = codefile, append = TRUE)
}

write.stata <- function (df, datafile, codefile, varlabels) 
{
    write.table(df, file = datafile, row.names = FALSE, col.names = FALSE, 
        sep = ",", quote = FALSE, na = ".")
    nms <- names(df)
    varlabels <- paste("label variable ", nms, " \"",varlabels, "\"", sep="", collapse="\n")
    factors <- sapply(df, is.factor) | sapply(df, is.character)
    formats <- paste(nms, "fmt", sep = "_")
    nms <- ifelse(factors, paste(nms, formats, sep = ":"), nms)
    cat("infile", nms, " using ", datafile, ", automatic\n", varlabels,
        file = codefile)
}
