analysisEditor <-
setRefClass("RzAnalysisEditor",
  fields = c("main"),
  methods = list(
    initialize            = function(...) {
      initFields(...)
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
      main$packStart(scrolledWindow, padding=2)
      main$packStart(button.box, expand=FALSE)
      
      gSignalConnect(button.clear, "clicked", function(button){
        buffer <- textview$getBuffer()
        buffer$setText("")
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
        
        env <- new.env()
        rz.tmp.path <- tempfile()
        env$df.orig <- data.frame
        env$df      <- data.frame[vars]
        cat(gettext("\n=============== Output from Quick Editor ===============\n"), fill=TRUE)
        e <- try(eval(parse(text=script), envir=env), silent=TRUE)
        info.bar <- rzTools$getInfoBar()
        if (any(class(e)=="try-error")) {
          info.bar$setMessageType(GtkMessageType["error"])
          info.bar$setText(e[1])
          info.bar$show()
        } else {
          print(e)
          info.bar$hide()
        }
      })
    }
  )
)
analysisEditor$accessors("main")

