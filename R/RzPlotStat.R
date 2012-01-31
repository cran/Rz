rzplot.stat <- 
setRefClass("RzPlotStat",
  fields = c("expander", "button",
#                         "entry.completion1",
             "combo"),
  methods = list(
    initialize  = function(...) {
      initFields(...)
      #                   entry.completion1 <<- gtkEntryCompletionNew()
      #                   entry.completion1$setTextColumn(1)
      #                   entry.completion1$setInlineCompletion(TRUE)
      #                   entry.completion1$setInlineSelection(TRUE)
      #                   entry.completion1$setPopupSetWidth(FALSE)
      
      label <- gtkLabelNew("statistics")
      combo <<- gtkComboBoxNewText()
      stats <- c("none", "smooth", "quantile", "sum")
      for(i in stats) combo$appendText(i)
      combo$setActive(0)

      image <- gtkImageNewFromStock(GTK_STOCK_ADD, GtkIconSize["menu"])
      button <<- gtkButtonNew()
      button$setFocusOnClick(FALSE)
      button$setImage(image)
      button$setRelief(GtkReliefStyle["none"])
      button["tooltip-text"] <<- gettext("Add Layer and Redraw")
      
      table <- gtkTableNew(1, 3, FALSE)
      table["border-width"] <- 5
      table$attach        (label,       0, 1, 0, 1, "shrink", "shrink", 0, 0)
      table$attachDefaults(combo,       1, 2, 0, 1)
      table$attach        (button,      2, 3, 0, 1, "shrink", "shrink", 0, 0)
      table$setColSpacings(5)
      table$setRowSpacings(2)
            
      expander <<- gtkExpanderNew(gettext("statistics options"))
      expander["border-width"] <<- 3
      expander$setExpanded(FALSE)
      expander$add(table)
      
      #                  gSignalConnect(combo, "changed", .self$onGeomComboChanged)
    },
    
    
    #                 onGeomComboChanged = function(combo=NULL){
    #                   
    #                   if(is.null(combo)){
    #                     geom <- "auto"
    #                   } else {
    #                     geom <- localize(combo$getActiveText())
    #                   }
    #                   hist.label1$hide()
    #                   hist.combo1$hide()
    #                   var2.label$hide()
    #                   var2.entry$hide()
    #                   
    #                   if(geom == "histogram"){
    #                     hist.label1$showAll()
    #                     hist.combo1$showAll()
    #                   } else if(geom == "boxplot"){
    #                     var2.label$setText(gettext("category"))
    #                     var2.label$showAll()
    #                     var2.entry$showAll()
    #                   } else if (geom == "point" | geom == "jitter"){
    #                     var2.label$setText(gettext("variable 2"))
    #                     var2.label$showAll()
    #                     var2.entry$showAll()
    #                   }
    #                 },
    
    getArgs = function(){
      stat <- localize(combo$getActiveText())
      args <- list(stat=stat)
      return(args)
    }#,
    
    #                completionSetModel = function(model){
    #                  entry.completion1$setModel(model)
    #                }
  )
)
rzplot.stat$accessors("expander", "button")
