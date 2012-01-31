rzplot.geom <- 
setRefClass("RzPlotGeom",
  fields = c("expander", "button",
             "entry.completion1",
             "combo", "combo.theme",
             "hist.label1", "hist.combo1",
             "y.label", "y.entry",
             "flip.togglebutton",
             "na.rm.togglebutton"),
  methods = list(
    initialize  = function(...) {
      initFields(...)
      entry.completion1 <<- gtkEntryCompletionNew()
      entry.completion1$setTextColumn(1)
      entry.completion1$setInlineCompletion(TRUE)
      entry.completion1$setInlineSelection(TRUE)
      entry.completion1$setPopupSetWidth(FALSE)
      
      # genaral & geom options
      label <- gtkLabelNew("geom")
      combo <<- gtkComboBoxNewText()
      geoms <- c("auto", "bar", "freqpoly", "histogram", "density", "boxplot",
                 "line", "area", "point", "jitter", "rug", "smooth", "quantile")
      for(i in geoms) combo$appendText(i)
      combo$setActive(0)
      
      image  <- gtkImageNewFromStock(GTK_STOCK_ADD, GtkIconSize["menu"])
      button <<- gtkButtonNew()
      button["tooltip-text"] <<- gettext("Add Layer and Redraw")
      button$setFocusOnClick(FALSE)
      button$setImage(image)
      button$setRelief(GtkReliefStyle["none"])
      
      
      # histogram
      hist.label1 <<- gtkLabelNew("binwidth", show=FALSE)
      hist.combo1 <<- gtkComboBoxEntryNewText()
      hist.combo1$hide()
      breaks <- c("default", "based on Sturges", "based on Scott", "based on Freedman-Diaconis", "(numeric)")
      for(i in breaks) hist.combo1$appendText(i)
      hist.combo1$setActive(0)
      
      # y
      y.label <<- gtkLabelNew(show=TRUE)
      y.label$setText(gettext("y"))
      y.entry <<- gtkEntryNew(show=TRUE)
      y.entry$setCompletion(entry.completion1)
      
      #flip
      flip.togglebutton <<- gtkToggleButtonNewWithLabel(gettext("flip"))
      
      #remove NA
      na.rm.togglebutton <<- gtkToggleButtonNewWithLabel(gettext("remove NA"))
      na.rm.togglebutton$setActive(TRUE)
      
      #theme
      label.theme <- gtkLabelNew("theme")
      combo.theme <<- gtkComboBoxNewText()
      themes <- c("gray", "bw")
      for(i in themes) combo.theme$appendText(i)
      combo.theme$setActive(0)
      
      
      table <- gtkTableNew(6, 3, FALSE)
      table["border-width"] <- 5
      table$attach        (label,       0, 1, 0, 1, "shrink", "shrink", 0, 0)
      table$attachDefaults(combo,       1, 2, 0, 1)
      table$attach        (button,      2, 3, 0, 1, "shrink", "shrink", 0, 0)
      table$attach        (hist.label1, 0, 1, 1, 2, "shrink", "shrink", 0, 0)
      table$attachDefaults(hist.combo1, 1, 3, 1, 2)
      table$attach        (y.label,     0, 1, 2, 3, "shrink", "shrink", 0, 0)
      table$attachDefaults(y.entry,     1, 3, 2, 3)
      table$attachDefaults(flip.togglebutton,   0, 3, 3, 4)
      table$attachDefaults(na.rm.togglebutton,  0, 3, 4, 5)
      table$attach        (label.theme, 0, 1, 5, 6, "shrink", "shrink", 0, 0)
      table$attachDefaults(combo.theme, 1, 3, 5, 6)
      
      table$setColSpacings(5)
      table$setRowSpacings(2)
            
      expander <<- gtkExpanderNew(gettext("general & geom options"))
      expander["border-width"] <<- 3
      expander$setExpanded(TRUE)
      expander$add(table)
            
      gSignalConnect(combo, "changed", .self$onGeomComboChanged)
    },
    
    
    onGeomComboChanged = function(combo=NULL){
      
      if(is.null(combo)){
        geom <- "auto"
      } else {
        geom <- localize(combo$getActiveText())
      }
      hist.label1$hide()
      hist.combo1$hide()
      
      if(geom == "histogram"){
        hist.label1$showAll()
        hist.combo1$showAll()
      }
    },
    
    getArgs = function(){
      geom <- combo$getActiveText()
      hist.break <- localize(hist.combo1$getActiveText())
      y       <- localize(y.entry$getText())
      if(!nzchar(y)) y <- NULL
      flip       <- flip.togglebutton$getActive()
      na.rm      <- na.rm.togglebutton$getActive()
      theme <- combo.theme$getActiveText()
      args <- list(geom=geom,
                   hist.break=hist.break,
                   y=y,
                   flip=flip,
                   na.rm=na.rm,
                   theme=theme)
      return(args)
    },
    
    completionSetModel = function(model){
      entry.completion1$setModel(model)
    }
  )
)
rzplot.geom$accessors("expander", "button")
