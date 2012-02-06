rzplot.save <- 
setRefClass("RzPlotSave",
  fields = c("win", "expander", "dialog", "file.types", "file.type.list",
             "entry1", "entry2"),
  methods = list(
    initialize  = function(...) {
      initFields(...)
      # save
      dialog <<- gtkFileChooserDialogNew(title=gettext("Save"), parent=win,
                                         action=GtkFileChooserAction["save"],
                                         "gtk-save", GtkResponseType["accept"],
                                         "gtk-cancel", GtkResponseType["cancel"], 
                                         show=FALSE)
      file.types <<- c(png  = gettext("png (*.png)"),
                       pdf  = gettext("pdf (*.pdf)"),
                       eps  = gettext("eps/ps (*.eps, *.ps)"),
                       wmf  = gettext("wmf(windows only) (*.wmf)"),
                       svg  = gettext("svg (*.svg)"),
                       jpeg = gettext("jpeg (*.jpeg)"),
                       bmp  = gettext("bmp (*.bmp)"),
                       tiff = gettext("tiff (*.tiff)"),
                       tex  = gettext("pictex (*.tex)"))
      file.type.list <<- list(png  = list(name=file.types[["png"]] , pattern="*.png"),
                              pdf  = list(name=file.types[["pdf"]] , pattern="*.pdf"),
                              eps  = list(name=file.types[["eps"]] , pattern=c("*.eps", "*.ps")),
                              wmf  = list(name=file.types[["wmf"]] , pattern="*.wmf"),
                              svg  = list(name=file.types[["svg"]] , pattern="*.svg"),
                              jpeg = list(name=file.types[["jpeg"]], pattern="*.jpeg"),
                              bmp  = list(name=file.types[["bmp"]] , pattern="*.bmp"),
                              tiff = list(name=file.types[["tiff"]], pattern="*.tiff"),
                              tex  = list(name=file.types[["tex"]] , pattern="*.tex")
                              )
      for (i in seq_along(file.type.list)) {
        filter <- gtkFileFilterNew()
        filter$setName(file.type.list[[i]]$name)
        for ( j in file.type.list[[i]]$pattern){ filter$addPattern(j) }
        dialog$addFilter(filter)
      }
      dialogRun <- function(...) dialog$run()
      save.button <- gtkButtonNewFromStock(GTK_STOCK_SAVE)
      gSignalConnect(save.button, "clicked", dialogRun)
      
      label1 <- gtkLabelNew(gettext("width (cm)"))
      entry1 <<- gtkEntryNew()
      entry1$setText("(auto)")
      label2 <- gtkLabelNew(gettext("height (cm)"))
      entry2 <<- gtkEntryNew()
      entry2$setText("(auto)")
      
      table  <- gtkTableNew(3, 2, FALSE)
      table["border-width"] <- 5
      table$attach(label1, 0, 1, 0, 1, "shrink", "shrink", 0, 0)
      table$attachDefaults(entry1, 1, 2, 0, 1)
      table$attach(label2, 0, 1, 1, 2, "shrink", "shrink", 0, 0)
      table$attachDefaults(entry2, 1, 2, 1, 2)
      table$attach(save.button, 0, 2, 2, 3, "fill", "fill", 0, 0)
      table$setColSpacings(5)
      table$setRowSpacings(2)
      
      expander <<- gtkExpanderNew("save options")
      expander["border-width"] <<- 3
      expander$setExpanded(FALSE)
      expander$add(table)
    },
    
    onSave = function(response.id, p.current, theme_Rz, legend.position){
      if (response.id == GtkResponseType["accept"]) {
        dialog$hide()
        
        filename <- localize(dialog$getFilename())
        filetype <- localize(dialog$getFilter()$getName())
        
        match <- NULL
        for (i in seq_along(file.type.list)) {
          match <- c(match, file.type.list[[i]]$name==filetype)
        }
        ind <- which(match)
        ext <- paste(strsplit(file.type.list[[ind]]$pattern[1], "")[[1]][-1], collapse="")
        filename <- ifelse(grepl(paste(file.type.list[[ind]]$pattern, collapse="|"), filename), filename, sprintf(paste("%s", ext, sep=""), filename))
        
        if (file.exists(filename)){
          dialog2 <- gtkMessageDialogNew(dialog, "destroy-with-parent",
                                         GtkMessageType["question"],
                                         GtkButtonsType["ok-cancel"],
                                         gettext("This file already exists. Overwrite it?"))
          response <- dialog2$run()
          dialog2$hide()
          if (response!=GtkResponseType["ok"]){
            dialog$run()
            return()
          }
        }
        
        width  <- localize(entry1$getText())
        width  <- suppressWarnings(as.numeric(width))
        width  <- ifelse(is.na(width), par("din")[1], width / 2.54)
        height <- localize(entry2$getText())
        height <- suppressWarnings(as.numeric(height))
        height <- ifelse(is.na(height), par("din")[2], height / 2.54)
        
        if(filetype==file.types[["eps"]]){
          p <- p.current + theme_Rz() + opts(legend.position=legend.position)
          ggsave(filename=filename, plot=p, width=width, height=height,
                 family=rzSettings$getPsFont())
        } else if(filetype==file.types[["pdf"]]){
          p <- p.current + theme_Rz() + opts(legend.position=legend.position)
          
          ggsave(filename=filename, plot=p, width=width, height=height,
                 family=rzSettings$getPdfFont())
        } else {
          p <- p.current
          if(grepl("mingw", R.Version()$os)){
            windowsFonts(F = windowsFont(rzSettings$getPlotFontFamily()))
            p <- p + theme_Rz(base_family="F") + opts(legend.position=legend.position)
          } else if(grepl("darwin", R.Version()$os)){
            X11Fonts(F=X11Font(sprintf("-*-%s-*-*-*-*-*-*-*-*-*-*-*-*", rzSettings$getPlotFontFamily())))
            quartzFonts(F = quartzFont(rep(rzSettings$getPlotFontFamily(), 4)))
            p <- p + theme_Rz(base_family="F") + opts(legend.position=legend.position)
          }
          
          ggsave(filename=filename, plot=p, width=width, height=height)
        }
      } else {
        dialog$hide()
      }
      
    }
  )
)
rzplot.save$accessors("expander", "dialog")
