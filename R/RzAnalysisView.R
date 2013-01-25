analysisView <-
setRefClass("RzAnalysisView",
  fields = c("main", "rzAnalysisStat", "parent"),
  methods = list(
    initialize            = function(...) {
      initFields(...)
      
      parent <<- NULL
      button.detach <- gtkButtonNewWithLabel(gettext("Detach"))
      main <<- gtkNotebookNew()
      if(is.null(gtkCheckVersion(2, 20, 0))) { 
        main$setActionWidget(button.detach, GtkPackType["end"])
      }
      main$setTabPos(GtkPositionType["top"])
      rzAnalysisStat <<- new("RzAnalysisStat")
      main$appendPage(rzAnalysisStat$getMain()                 , gtkLabelNew(gettext("Descriptive statistics")))
      main$appendPage(new("RzAnalysisEditor")$getMain()        , gtkLabelNew(gettext("Editor")))
      main$appendPage(new("RzAnalysisVariableEditor")$getMain(), gtkLabelNew(gettext("Variable Editor")))
      
      gSignalConnect(button.detach, "clicked", .self$onDetach)
    },
    
    toggled = function(){
      rzAnalysisStat$toggled()
    },
        
    setAccel = function(accel.group){
#      button.execute$setAccelPath("<Rz-Menu>/View/Quick Editor View/Execute", accel.group)
    },
    
    onDetach = function(button){
      if(is.null(parent)) {
        button["label"] <- gettext("Attach")
        win <- gtkWindowNew(show=FALSE)
        win$setDefaultSize(500, 300)
        win$setTitle(gettext("Analysis"))
        parent <<- main$getParent()
        main$reparent(win)
        win$showAll()
        rzAnalysisStat$rebuild()
        
        gSignalConnect(win, "destroy", function(...){
          if (any(class(parent)=="GtkWidget")) {
            button["label"] <- gettext("Detach")
            main$reparent(parent)
            parent <<- NULL            
          }
        })
        
      } else {
        main$getParent()$destroy()
      }
    }
  )
)
analysisView$accessors("main")
