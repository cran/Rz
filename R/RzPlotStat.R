rzplot.stat <- 
setRefClass("RzPlotStat",
  fields = c("expander", "button",
             "combo", "combo.method", "combo.geom", "combo.geom2",
             "combo.fun", "combo.color", "entry.size"),
  methods = list(
    initialize  = function(...) {
      initFields(...)
      
      label <- gtkLabelNew("statistics")
      combo <<- gtkComboBoxNewText()
      stats <- c("none", "smooth", "quantile", "sum", "summary")
      for(i in stats) combo$appendText(i)

      image <- gtkImageNewFromStock(GTK_STOCK_ADD, GtkIconSize["menu"])
      button <<- gtkButtonNew()
      button$setFocusOnClick(FALSE)
      button$setImage(image)
      button$setRelief(GtkReliefStyle["none"])
      button["tooltip-text"] <<- gettext("Add Layer and Redraw")
      
      label.method <- gtkLabelNew("method")
      combo.method <<- gtkComboBoxNewText()
      methods <- c("auto", "lm", "glm", "loess")
      for(i in methods) combo.method$appendText(i)
      combo.method$setActive(0)
      
      label.geom <- gtkLabelNew("geom")
      combo.geom <<- gtkComboBoxNewText()
      geoms <- c("errorbar", "pointrange", "linerange", "crossbar", "smooth")
      for(i in geoms) combo.geom$appendText(i)
      combo.geom$setActive(0)
      
      label.color <- gtkLabelNew("color")
      combo.color <<- gtkComboBoxEntryNewText()
      for(i in c("default", colors())) combo.color$appendText(i)
      combo.color$setActive(0)
      
      label.size <- gtkLabelNew("size")
      entry.size <<- gtkEntryNew()
      entry.size$setText("0.5")
      
      label.geom2 <- gtkLabelNew("geom (additional)")
      combo.geom2 <<- gtkComboBoxNewText()
      geoms2 <- c("point", "line", "bar", "none")
      for(i in geoms2) combo.geom2$appendText(i)
      combo.geom2$setActive(0)
      
      label.fun <- gtkLabelNew("function")
      combo.fun <<- gtkComboBoxNewText()
      funs <- c("mean_se", "mean_sdl", "mean_cl_normal", "mean_cl_boot", "median_hilow")
      for(i in funs) combo.fun$appendText(i)
      combo.fun$setActive(0)

      table <- gtkTableNew(7, 3, FALSE)
      table["border-width"] <- 5
      table$attach        (label,        0, 1, 0, 1, "shrink", "shrink", 0, 0)
      table$attachDefaults(combo,        1, 2, 0, 1)
      table$attach        (button,       2, 3, 0, 1, "shrink", "shrink", 0, 0)
      table$attach        (label.method, 0, 1, 1, 2, "shrink", "shrink", 0, 0)
      table$attachDefaults(combo.method, 1, 3, 1, 2)
      table$attach        (label.geom,   0, 1, 2, 3, "shrink", "shrink", 0, 0)
      table$attachDefaults(combo.geom,   1, 3, 2, 3)
      table$attach        (label.color,  0, 1, 3, 4, "shrink", "shrink", 0, 0)
      table$attachDefaults(combo.color,  1, 3, 3, 4)
      table$attach        (label.size,   0, 1, 4, 5, "shrink", "shrink", 0, 0)
      table$attachDefaults(entry.size,   1, 3, 4, 5)
      table$attach        (label.geom2,  0, 1, 5, 6, "shrink", "shrink", 0, 0)
      table$attachDefaults(combo.geom2,  1, 3, 5, 6)
      table$attach        (label.fun,    0, 1, 6, 7, "shrink", "shrink", 0, 0)
      table$attachDefaults(combo.fun,    1, 3, 6, 7)
      table$setColSpacings(5)
      table$setRowSpacings(2)
            
      expander <<- gtkExpanderNew(gettext("statistics options"))
      expander["border-width"] <<- 3
      expander$setExpanded(FALSE)
      expander$add(table)
      
      gSignalConnect(combo, "changed", function(combo){
        
        stat <- localize(combo$getActiveText())
        
        label.geom$hide()
        combo.geom$hide()
        label.color$hide()
        combo.color$hide()
        label.size$hide()
        entry.size$hide()
        label.geom2$hide()
        combo.geom2$hide()
        label.fun$hide()
        combo.fun$hide()
        label.method$hide()
        combo.method$hide()
        
        if(stat == "summary"){
          label.geom$showAll()
          combo.geom$showAll()
          label.color$showAll()
          combo.color$showAll()
          label.size$showAll()
          entry.size$showAll()
          label.geom2$showAll()
          combo.geom2$showAll()
          label.fun$showAll()
          combo.fun$showAll()
        } else if(stat == "smooth"){
          label.method$showAll()
          combo.method$showAll()
        }
      })
      combo$setActive(0)
    },
    
    clear = function(){
      combo$setActive(0)
      combo.method$setActive(0)
      combo.geom$setActive(0)
      combo.geom2$setActive(0)
      combo.fun$setActive(0)
      combo.color$setActive(0)
      entry.size$setText("0.5")
    },
    
    getArgs = function(){
      stat   <- localize(combo$getActiveText())
      method <- localize(combo.method$getActiveText())
      geom   <- localize(combo.geom$getActiveText())
      color  <- localize(combo.color$getActiveText())
      size   <- as.numeric(localize(entry.size$getText()))
      geom2  <- localize(combo.geom2$getActiveText())
      fun    <- localize(combo.fun$getActiveText())
      if(any(color==c("", "default"))) color <- NULL
      args   <- list(stat=stat,
                     method=method,
                     geom=geom,
                     color=color,
                     size=size,
                     geom2=geom2,
                     fun=fun)
      return(args)
    }
  )
)
rzplot.stat$accessors("expander", "button")
