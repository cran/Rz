rzplot.stratum <- 
setRefClass("RzPlotStratum",
  fields = c("entry.group", "combo.group.label",
             "entry.fill", "combo.fill.label",
             "entry.color", "combo.color.label",
             "entry.shape", "combo.shape.label",
             "entry.size","combo.size.label",
             "combo.position", "expander",
             "entry.completion1", "entry.completion2", "entry.completion3",
             "entry.completion4", "entry.completion5"),
  methods = list(
    initialize  = function(...) {
      initFields(...)
      entry.completion1 <<- gtkEntryCompletionNew()
      entry.completion1$setTextColumn(1)
      entry.completion1$setInlineCompletion(TRUE)
      entry.completion1$setInlineSelection(TRUE)
      entry.completion1$setPopupSetWidth(FALSE)
      entry.completion2 <<- gtkEntryCompletionNew()
      entry.completion2$setTextColumn(1)
      entry.completion2$setInlineCompletion(TRUE)
      entry.completion2$setInlineSelection(TRUE)
      entry.completion2$setPopupSetWidth(FALSE)
      entry.completion3 <<- gtkEntryCompletionNew()
      entry.completion3$setTextColumn(1)
      entry.completion3$setInlineCompletion(TRUE)
      entry.completion3$setInlineSelection(TRUE)
      entry.completion3$setPopupSetWidth(FALSE)
      entry.completion4 <<- gtkEntryCompletionNew()
      entry.completion4$setTextColumn(1)
      entry.completion4$setInlineCompletion(TRUE)
      entry.completion4$setInlineSelection(TRUE)
      entry.completion4$setPopupSetWidth(FALSE)
      entry.completion5 <<- gtkEntryCompletionNew()
      entry.completion5$setTextColumn(1)
      entry.completion5$setInlineCompletion(TRUE)
      entry.completion5$setInlineSelection(TRUE)
      entry.completion5$setPopupSetWidth(FALSE)
      
      label.group  <-  gtkLabelNew("group")
      entry.group  <<- gtkEntryNew()
      entry.group$setWidthChars(12)
      label.fill  <-  gtkLabelNew("fill")
      entry.fill  <<- gtkEntryNew()
      entry.fill$setWidthChars(12)
      label.color <-  gtkLabelNew("color")
      entry.color <<- gtkEntryNew()
      entry.color$setWidthChars(12)
      label.shape <-  gtkLabelNew("shape")
      entry.shape <<- gtkEntryNew()
      entry.shape$setWidthChars(12)
      label.size  <-  gtkLabelNew("size")
      entry.size  <<- gtkEntryNew()
      entry.size$setWidthChars(12)
      entry.group$setCompletion(entry.completion1)
      entry.fill$setCompletion(entry.completion2)
      entry.color$setCompletion(entry.completion3)
      entry.shape$setCompletion(entry.completion4)
      entry.size$setCompletion(entry.completion5)

      # group
      combo.group.label <<- gtkComboBoxEntryNewText()
      combo.group.label["width-request"] <<- 1
      combo.group.label$show()
      labels <- c(gettext("variable label"), gettext("variable name"), gettext("(free text)"))
      for(i in labels) combo.group.label$appendText(i)
      combo.group.label$setActive(0)

      # fill
      combo.fill.label <<- gtkComboBoxEntryNewText()
      combo.fill.label["width-request"] <<- 1
      combo.fill.label$show()
      for(i in labels) combo.fill.label$appendText(i)
      combo.fill.label$setActive(0)

      # color
      combo.color.label <<- gtkComboBoxEntryNewText()
      combo.color.label["width-request"] <<- 1
      combo.color.label$show()
      for(i in labels) combo.color.label$appendText(i)
      combo.color.label$setActive(0)

      # shape
      combo.shape.label <<- gtkComboBoxEntryNewText()
      combo.shape.label["width-request"] <<- 1
      combo.shape.label$show()
      for(i in labels) combo.shape.label$appendText(i)
      combo.shape.label$setActive(0)

      # size
      combo.size.label <<- gtkComboBoxEntryNewText()
      combo.size.label["width-request"] <<- 1
      combo.size.label$show()
      for(i in labels) combo.size.label$appendText(i)
      combo.size.label$setActive(0)

      # position
      label.position <- gtkLabelNew(gettext("legend position"))
      combo.position <<- gtkComboBoxNewText()
      combo.position$show()
      labels <- c("top", "right", "bottom", "left", "none")
      for(i in labels) combo.position$appendText(i)
      combo.position$setActive(1)

      table  <- gtkTableNew(6, 3, FALSE)
      table["border-width"] <- 5
      table$attach        (label.group      , 0, 1, 0, 1, "shrink", "shrink", 0, 0)
      table$attach        (entry.group      , 1, 2, 0, 1, "shrink", "shrink", 0, 0)
      table$attachDefaults(combo.group.label, 2, 3, 0, 1)
      table$attach        (label.fill       , 0, 1, 1, 2, "shrink", "shrink", 0, 0)
      table$attach        (entry.fill       , 1, 2, 1, 2, "shrink", "shrink", 0, 0)
      table$attachDefaults(combo.fill.label , 2, 3, 1, 2)
      table$attach        (label.color      , 0, 1, 2, 3, "shrink", "shrink", 0, 0)
      table$attach        (entry.color      , 1, 2, 2, 3, "shrink", "shrink", 0, 0)
      table$attachDefaults(combo.color.label, 2, 3, 2, 3)
      table$attach        (label.shape      , 0, 1, 3, 4, "shrink", "shrink", 0, 0)
      table$attach        (entry.shape      , 1, 2, 3, 4, "shrink", "shrink", 0, 0)
      table$attachDefaults(combo.shape.label, 2, 3, 3, 4)
      table$attach        (label.size       , 0, 1, 4, 5, "shrink", "shrink", 0, 0)
      table$attach        (entry.size       , 1, 2, 4, 5, "shrink", "shrink", 0, 0)
      table$attachDefaults(combo.size.label , 2, 3, 4, 5)
      table$attach        (label.position   , 0, 1, 5, 6, "shrink", "shrink", 0, 0)
      table$attachDefaults(combo.position   , 1, 3, 5, 6)
      table$setColSpacings(5)
      table$setRowSpacings(2)

      expander <<- gtkExpanderNew("stratum options")
      expander["border-width"] <<- 3
      expander$setExpanded(FALSE)
      expander$add(table)


    },
    
    completionSetModel = function(model){
      entry.completion1$setModel(model)
      entry.completion2$setModel(model)
      entry.completion3$setModel(model)
      entry.completion4$setModel(model)
      entry.completion5$setModel(model)
    },
    
    getArgs = function(){
      group <- localize(entry.group$getText())
      fill  <- localize(entry.fill$getText())
      color <- localize(entry.color$getText())
      shape <- localize(entry.shape$getText())
      size  <- localize(entry.size$getText())
      group.label     <- localize(combo.group.label$getActiveText())
      fill.label      <- localize(combo.fill.label$getActiveText())
      color.label     <- localize(combo.color.label$getActiveText())
      shape.label     <- localize(combo.shape.label$getActiveText())
      size.label      <- localize(combo.size.label$getActiveText())
      legend.position <- localize(combo.position$getActiveText())
      if(!nzchar(group)) group <- NULL
      if(!nzchar(fill))  fill  <- NULL
      if(!nzchar(color)) color <- NULL
      if(!nzchar(shape)) shape <- NULL
      if(!nzchar(size))  size  <- NULL
      args <- list(group=group, fill=fill, color=color, shape=shape, size=size,
                   group.label=group.label, fill.label=fill.label, color.label=color.label,
                   shape.label=shape.label, size.label=size.label,
                   legend.position=legend.position)
      return(args)
    }
    )
)
rzplot.stratum$accessors("expander")
