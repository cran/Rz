analysisVariableEditor <-
setRefClass("RzAnalysisVariableEditor",
  fields = c("main"),
  methods = list(
    initialize            = function(...) {
      initFields(...)
      templates <- c(
        'measurement(var) <- "interval"',
        'description(var) <- "Variable Label"',
        'labels(var) <- c(\n  "a"=1,\n  "b"=2,\n  "c"=3\n  )',
        'missing.values(var) <- c(8, 9)',
        'var <- -var'
      )
      names(templates) <- c(gettext("Change Measurement"),
                            gettext("Edit Variable Labels"),
                            gettext("Edit Value Labels"),
                            gettext("Apply Missing Values"),
                            gettext("Reverse"))
      
      label.template <- gtkLabelNew(gettext("Templates"))
      combo.template <- gtkComboBoxNewText()
      for(i in names(templates)) combo.template$appendText(i)
      hbox.template  <- gtkHBoxNew(spacing=5)
      hbox.template$packStart(label.template, expand=FALSE)
      hbox.template$packStart(combo.template)
      
      textview <- gtkTextViewNew()
      textview$modifyFont(pangoFontDescriptionFromString(rzSettings$getMonospaceFont()))
      textview$setLeftMargin(5)
      textview$setRightMargin(5)
      scrolledWindow <- gtkScrolledWindowNew()
      scrolledWindow["shadow-type"] <- GtkShadowType["in"]
      scrolledWindow$setPolicy(GtkPolicyType["automatic"], GtkPolicyType["automatic"])
      scrolledWindow$add(textview)
      
      button.execute <- gtkButtonNewFromStock(GTK_STOCK_EXECUTE)
      button.clear   <-  gtkButtonNewFromStock(GTK_STOCK_CLEAR)
      button.box     <-  gtkHButtonBoxNew()
      button.box$packStart(button.clear)
      button.box$packStart(button.execute)      
      
      main <<- gtkVBoxNew(spacing=0)
      main$packStart(hbox.template, expand=FALSE, padding=2)
      main$packStart(scrolledWindow, padding=2)
      main$packStart(button.box, expand=FALSE)
      
      gSignalConnect(button.clear, "clicked", function(button){
        buffer <- textview$getBuffer()
        buffer$setText("")
      })
      
      gSignalConnect(combo.template, "changed", function(combo){
        text   <- localize(combo$getActiveText())
        buffer <- textview$getBuffer()
        buffer$setText(templates[text])
      })
      
      gSignalConnect(button.execute, "clicked", function(button){
        variable.view <- rzTools$getVariableView()
        if(is.null(variable.view)) return()
        
        buffer <- textview$getBuffer()
        iter   <- buffer$getBounds()
        script <- localize(buffer$getText(iter$start, iter$end))
        script <- sub("^([[:space:]]+)([^[:space:]]+)([[:space:]]+)$", "\\2", script)
        if(!nzchar(script)) return()
        
        data          <- variable.view$getData()
        data.set.name <- data$getData.set.name()
        data.set      <- data$getData.set()
        data.frame    <- data$getData.frame()
        
        vars <- names(data.frame[variable.view$getSelectedRows()])
        if(length(vars) == 0) return()
        
        vars <- paste(vars, collapse=", ")
        script <- sprintf("foreach(var=c(%s), {\n%s\n})", vars, script)
        data.set <- try(within(data.set, eval(parse(text=script))), silent=TRUE)
        info.bar <- rzTools$getInfoBar()
        if (is.data.set(data.set)){
          data$setData.set(data.set)
          data$setData.frame(as.data.frame(data.set))
          variable.view$reload()
          data$linkDataFrame()
          info.bar$hide()
        } else {
          info.bar$setMessageType(GtkMessageType["error"])
          info.bar$setText(data.set[1])
          info.bar$show()
        }
      })
      
    }
  )
)
analysisVariableEditor$accessors("main")


