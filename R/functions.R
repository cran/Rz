data.collec.obj <- new("RzDataCollection")

Rz <- function(...){
  data.collection <- data.collec.obj
  main.obj <- main$new(data.collection=data.collection)
  main.obj$constructActionGroup()
  main.obj$constructMenu()
  main.obj$constructDataHandler()
  main.obj$constructWindow()
}

localize <- function(char) iconv(char, "UTF-8", localeToCharset()[1])

gtkProgressBarStart <- function(progress.bar){
  progress.bar$Pulse()
  return(TRUE)
}

gtkFileChooserDialogFilteredNew <- function(title, parent=NULL, file.type.list){
  dialog <- gtkFileChooserDialogNew(title=title, parent=parent,
                                    action=GtkFileChooserAction["open"],
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

.onAttach <- function(lib, pkg){
	if(exists("winMenuAdd")){
		temp<-try(winMenuAdd("Rz"),silent=TRUE)
		if(class(temp)!="try-error"){
    winMenuAddItem("Rz", gettext("Start"), "Rz()")
    }
  }
  txt1 <- "################################ Rz ################################"
  txt2 <- gettext("Excute Rz() to start,")
  txt3 <- gettext("or you can start from menu bar if you use R on Windows.")
  txt4 <- "####################################################################"
  txt  <- format(c(txt1,txt2,txt3,txt4), justify="centre")
  cat("\n\n\n", txt, sep="\n", "\n", fill=TRUE)
}
