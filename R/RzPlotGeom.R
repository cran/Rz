rzplot.geom <- 
setRefClass("RzPlotGeom",
  fields = c("expander", "button",
             "combo", "combo.theme",
             "hist.label1", "hist.combo1",
             "combo.x", "combo.y",
             "flip.togglebutton",
             "na.rm.togglebutton"),
  methods = list(
    initialize  = function(...) {
      initFields(...)
      
      # x
      label.x <- gtkLabelNew(show=TRUE)
      label.x$setText(gettext("x"))
      combo.x <<- new("RzCompletionCombo")
      
      # y
      label.y <- gtkLabelNew(show=TRUE)
      label.y$setText(gettext("y"))
      combo.y <<- new("RzCompletionCombo")
      
      # geom
      label <- gtkLabelNew("geom")
      combo <<- gtkComboBoxNewText()
      geoms <- c("auto", "bar", "freqpoly", "histogram", "density", "boxplot", "violin",
                 "line", "area", "point", "jitter", "rug", "smooth", "quantile", "blank")
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
      
      table <- gtkTableNew(4, 3, FALSE)
      table["border-width"] <- 5
      table$attach        (label,              0, 1, 0, 1, "shrink", "shrink", 0, 0)
      table$attachDefaults(combo,              1, 2, 0, 1)
      table$attach        (button,             2, 3, 0, 1, "shrink", "shrink", 0, 0)
      table$attach        (label.x,            0, 1, 1, 2, "shrink", "shrink", 0, 0)
      table$attachDefaults(combo.x$getCombo(), 1, 3, 1, 2)
      table$attach        (label.y,            0, 1, 2, 3, "shrink", "shrink", 0, 0)
      table$attachDefaults(combo.y$getCombo(), 1, 3, 2, 3)
      table$attach        (hist.label1,        0, 1, 3, 4, "shrink", "shrink", 0, 0)
      table$attachDefaults(hist.combo1,        1, 3, 3, 4)
      
      table$setColSpacings(5)
      table$setRowSpacings(2)
      
      expander <<- gtkExpanderNew(gettext("geom options"))
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
    
    clear = function(){
      combo$setActive(0)
      hist.combo1$setActive(0)
      combo.x$clear()
      combo.y$clear()
    },
    
    setX = function(txt){
      combo.x$setText(txt)
    },
    
    getArgs = function(){
      geom <- combo$getActiveText()
      hist.break <- localize(hist.combo1$getActiveText())
      x       <- localize(combo.x$getActiveText())
      y       <- localize(combo.y$getActiveText())
      if(!nzchar(y)) y <- NULL
      args <- list(geom=geom,
                   hist.break=hist.break,
                   x=x,
                   y=y)
      return(args)
    },
    
    completionSetModel = function(model){
      combo.x$setModel(model)
      combo.y$setModel(model)
    }
  )
)
rzplot.geom$accessors("expander", "button")
