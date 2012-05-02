rzplot.position <- 
setRefClass("RzPlotPosition",
  fields = c("expander", "combo", "entry.width", "entry.height"),
  methods = list(
    initialize  = function(...) {
      initFields(...)

      label <- gtkLabelNew(gettext("position"))
      combo <<- gtkComboBoxNewText()
      combo$show()
      positions <- c("default", "identity", "dodge", "fill", "stack", "jitter")
      for(i in positions) combo$appendText(i)
      combo$setActive(0)

      label.width  <-  gtkLabelNew("width")
      entry.width  <<- gtkEntryNew()
      label.height <-  gtkLabelNew("height")
      entry.height <<- gtkEntryNew()

      table  <- gtkTableNew(3, 2, FALSE)
      table["border-width"] <- 5
      table$attach        (label       , 0, 1, 0, 1, "shrink", "shrink", 0, 0)
      table$attachDefaults(combo       , 1, 2, 0, 1)
      table$attach        (label.width , 0, 1, 1, 2, "shrink", "shrink", 0, 0)
      table$attachDefaults(entry.width , 1, 2, 1, 2)
      table$attach        (label.height, 0, 1, 2, 3, "shrink", "shrink", 0, 0)
      table$attachDefaults(entry.height, 1, 2, 2, 3)

      expander <<- gtkExpanderNew(gettext("position options"))
      expander["border-width"] <<- 3
      expander$setExpanded(FALSE)
      expander$add(table)
    },
    
    clear = function(){
      combo$setActive(0)
      entry.width$setText("")
      entry.height$setText("")
    },
    
    getArgs = function(){
      position  <- localize(combo$getActiveText())
      width     <- localize(entry.width$getText())
      height    <- localize(entry.height$getText())
      suppressWarnings(width  <- as.numeric(width))
      suppressWarnings(height <- as.numeric(height))
      if(is.na(width))  width  <- NULL
      if(is.na(height)) height <- NULL
      args <- list(position=position, width=width, height=height)
      return(args)
    }
))
rzplot.position$accessors("expander")
