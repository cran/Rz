rzplot.label <-
setRefClass("RzPlotLabel",
  fields = c("expander", "title.entry", "xlab.combo", "ylab.combo"),
  methods = list(
    initialize  = function(...) {
      initFields(...)
      # label options
      title.label <- gtkLabelNew(gettext("title"))
      title.entry <<- gtkEntryNew()
      
      xlab.label <- gtkLabelNew(gettext("x axis"))
      xlab.combo <<- gtkComboBoxEntryNewText()
      xlab.combo$show()
      labels <- c(gettext("variable label"), gettext("variable name"), gettext("(free text)"))
      for(i in labels) xlab.combo$appendText(i)
      xlab.combo$setActive(0)
      
      ylab.label <- gtkLabelNew(gettext("y axis"))
      ylab.combo <<- gtkComboBoxEntryNewText()
      ylab.combo$show()
      for(i in labels) ylab.combo$appendText(i)
      ylab.combo$setActive(0)
      
      table  <- gtkTableNew(3, 2, FALSE)
      table["border-width"] <- 5
      table$attach(title.label, 0, 1, 0, 1, "shrink", "shrink", 0, 0)
      table$attachDefaults(title.entry, 1, 2, 0, 1)
      table$attach(xlab.label, 0, 1, 1, 2, "shrink", "shrink", 0, 0)
      table$attachDefaults(xlab.combo, 1, 2, 1, 2)
      table$attach(ylab.label, 0, 1, 2, 3, "shrink", "shrink", 0, 0)
      table$attachDefaults(ylab.combo, 1, 2, 2, 3)
      table$setColSpacings(5)
      table$setRowSpacings(2)
      
      expander <<- gtkExpanderNew("label options")
      expander["border-width"] <<- 3
      expander$setExpanded(FALSE)
      expander$add(table)                  
    },
    getArgs = function(){
      title <- localize(title.entry$getText())
      xlab  <- localize(xlab.combo$getActiveText())
      ylab  <- localize(ylab.combo$getActiveText())
      args  <- list(title=title, xlab=xlab, ylab=ylab)
      return(args)
    }
  )
)
rzplot.label$accessors("expander")
