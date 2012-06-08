analysisView <-
setRefClass("RzAnalysisView",
  fields = c("main", "rzAnalysisStat"),
  methods = list(
    initialize            = function(...) {
      initFields(...)
            
      main <<- gtkNotebookNew()
      main$setTabPos(GtkPositionType["top"])
      rzAnalysisStat <<- new("RzAnalysisStat")
      main$appendPage(rzAnalysisStat$getMain()                 , gtkLabelNew(gettext("Descriptive statistics")))
      main$appendPage(new("RzAnalysisEditor")$getMain()        , gtkLabelNew(gettext("Editor")))
      main$appendPage(new("RzAnalysisVariableEditor")$getMain(), gtkLabelNew(gettext("Variable Editor")))
      
    },
    
    toggled = function(){
      rzAnalysisStat$toggled()
    },
        
    setAccel = function(accel.group){
#      button.execute$setAccelPath("<Rz-Menu>/View/Quick Editor View/Execute", accel.group)
    }
  )
)
analysisView$accessors("main")
