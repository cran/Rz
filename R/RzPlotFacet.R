rzplot.facet <- 
setRefClass("RzPlotFacet",
  fields = c("combo", "combo2", "combo.x",
    "combo.y", "entry3", "entry4", "entry5",
    "label.y", "label3", "label4", "label5", "expander"),
  methods = list(
    initialize  = function(...) {
      initFields(...)

      label <- gtkLabelNew("facet")
      combo <<- gtkComboBoxNewText()
      combo$appendText("grid")
      combo$appendText("wrap")
      combo$setActive(0)
      label.x <-  gtkLabelNew("x")
      combo.x <<- new("RzCompletionCombo")
      label.y <<- gtkLabelNew("y")
      combo.y <<- new("RzCompletionCombo")
      label3 <<- gtkLabelNew("nrow", show=FALSE)
      entry3 <<- gtkEntryNew(show=FALSE)
      label4 <<- gtkLabelNew("ncol", show=FALSE)
      entry4 <<- gtkEntryNew(show=FALSE)
      label5 <<- gtkLabelNew("scale", show=FALSE)
      combo2 <<- gtkComboBoxNewText(show=FALSE)
      combo2$appendText("fixed")
      combo2$appendText("free")
      combo2$appendText("free_x")
      combo2$appendText("free_y")
      combo2$setActive(0)
      gSignalConnect(combo, "changed", function(combo){
        facet <- localize(combo$getActiveText())
        if(facet == "grid"){
          label.y$show()
          combo.y$getCombo()$show()
          label3$hide()
          entry3$hide()
          label4$hide()
          entry4$hide()
          label5$hide()
          combo2$hide()
        } else {
          label.y$hide()
          combo.y$getCombo()$hide()
          label3$show()
          entry3$show()
          label4$show()
          entry4$show()
          label5$show()
          combo2$show()
        }
      })
      gSignalConnect(entry3, "changed", function(entry){
        if(entry$getText() != ""){
          entry4$setSensitive(FALSE)
        } else {
          entry4$setSensitive(TRUE)
        }
      })
      gSignalConnect(entry4, "changed", function(entry){
        if(entry$getText() != ""){
          entry3$setSensitive(FALSE)
        } else {
          entry3$setSensitive(TRUE)
        }
      })

      table  <- gtkTableNew(6, 2, FALSE)
      table["border-width"] <- 5
      table$attach(label,  0, 1, 0, 1, "shrink", "shrink", 0, 0)
      table$attachDefaults(combo,  1, 2, 0, 1)
      table$attach(label.x, 0, 1, 1, 2, "shrink", "shrink", 0, 0)
      table$attachDefaults(combo.x$getCombo(), 1, 2, 1, 2)
      table$attach(label.y, 0, 1, 2, 3, "shrink", "shrink", 0, 0)
      table$attachDefaults(combo.y$getCombo(), 1, 2, 2, 3)
      table$attach(label3, 0, 1, 3, 4, "shrink", "shrink", 0, 0)
      table$attachDefaults(entry3, 1, 2, 3, 4)
      table$attach(label4, 0, 1, 4, 5, "shrink", "shrink", 0, 0)
      table$attachDefaults(entry4, 1, 2, 4, 5)
      table$attach(label5, 0, 1, 5, 6, "shrink", "shrink", 0, 0)
      table$attachDefaults(combo2, 1, 2, 5, 6)
      table$setColSpacings(5)
      table$setRowSpacings(2)

      expander <<- gtkExpanderNew(gettext("facet options"))
      expander["border-width"] <<- 3
      expander$setExpanded(FALSE)
      expander$add(table)


    },
    
    completionSetModel = function(model){
      combo.x$setModel(model)
      combo.y$setModel(model)
    },
    
    clear = function(){
      combo$setActive(0)
      combo.x$clear()
      combo.y$clear()
      entry3$setText("")
      entry4$setText("")
      combo2$setActive(0)
    },
    
    getArgs = function(){
      facet <- localize(combo$getActiveText())
      x <- localize(combo.x$getActiveText())
      y <- localize(combo.y$getActiveText())
      on <- FALSE
      if(x!="" || y!=""){
        on <- TRUE
      }
      if(facet=="wrap" & x=="") {
        on <- FALSE
      }
      nrow <- localize(entry3$getText())
      ncol <- localize(entry4$getText())
      nrow <- suppressWarnings(as.numeric(nrow))
      ncol <- suppressWarnings(as.numeric(ncol))
      if(is.na(nrow)) nrow <- NULL
      if(is.na(ncol)) ncol <- NULL

      args <- list(facet   = facet,
                   x       = x,
                   y       = y,
                   on      = on,
                   nrow    = nrow,
                   ncol    = ncol,
                   scale   = localize(combo2$getActiveText()))
      return(args)
    }
    )
)
rzplot.facet$accessors("expander")
