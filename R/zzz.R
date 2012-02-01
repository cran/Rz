data.collection.obj <- new("RzDataCollection")
rzSettings <- new("RzSettings")
#      .Rz.path <- "/home/masahiro/Documents/R/Rz/Rz/inst"  # for debug
#      .Rz.path <- "/media/sf_Dropbox/Documents/R/Rz/Rz/inst"  # for debug

.onAttach <- function(lib, pkg){
  #     if(exists("winMenuAdd")){
  if(grepl("mingw", R.Version()$os)){
    temp<-try(winMenuAdd("Rz"),silent=TRUE)
    if(class(temp)!="try-error"){
      winMenuAddItem("Rz", gettext("Start"), "Rz()")
    }
  }
  rzSettings$load()
  txt1 <- "################################ Rz ################################"
  txt2 <- gettext("Excute Rz() to start,")
  txt3 <- gettext("or you can start from menu bar if you use R on Windows.")
  txt4 <- "####################################################################"
  txt  <- format(c(txt1,txt2,txt3,txt4), justify="centre")
  txt <- paste(txt, collapse="\n")
  packageStartupMessage(txt)
}

