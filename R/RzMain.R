main <-
setRefClass("RzMain",
  fields = c("recode.id", "win",
             "main.hpaned", "main.vpaned", "main.view", "plot.view", "variable.view", "entry.search",
             "info.bar", "message.label", "status.bar", "progress.bar", "actions", "variable.view.list",
             "rzActionGroup", "rzMenu", "rzDataHandler", "rzSearchEntry", "rzDataSetIO", "rzPlot",
             "rzAnalysisView", "view.box"),
  methods = list(
    initialize            = function(...) {
      initFields(...)
      if (! exists("theme_rz", envir=.GlobalEnv)) {
        rzTools$sync("theme_rz", theme_grey)        
      }

      settings <- gtkSettingsGetDefault()
      settings$setStringProperty("gtk-font-name", rzSettings$getGlobalFont(), NULL)
      gtkRcReparseAll()
      rzDataSetIO    <<- new("RzDataSetIO")
      recode.id      <<- NULL
      win            <<- gtkWindowNew(show=FALSE)
      rzTools$setWindow(win)
      info.bar      <<- gtkInfoBarRzNew()
      message.label <<- gtkLabelNew()
      info.bar$addButton("gtk-ok", GtkResponseType["ok"])
      info.bar$setMessageType(GtkMessageType["info"])
      info.bar$getContentArea()$packStart(message.label, expand=FALSE, fill=FALSE)
      info.bar$hide()
      info.bar$setNoShowAll(TRUE)
      # must before new("Rzplot")
      rzTools$setInfoBar(info.bar)
      status.bar    <<- gtkStatusbarNew()
      progress.bar  <<- gtkProgressBarNew()
      
      main.hpaned    <<- gtkHPanedNew()
      main.vpaned    <<- gtkVPanedNew()
      main.view      <<- gtkVBoxNew()
      rzAnalysisView <<- new("RzAnalysisView")
      rzPlot         <<- new("RzPlot")
      
      main.vpaned$pack1(main.view, resize=TRUE)
      main.vpaned$pack2(rzAnalysisView$getMain(), resize=TRUE)
      main.vpaned$setPosition(280)
      
      main.hpaned$pack1(main.vpaned, resize=TRUE)
      main.hpaned$pack2(rzPlot$getMain(), resize=TRUE)
      main.hpaned$setPosition(450)
            
      status.bar$packEnd(progress.bar, expand=FALSE)
      
      rzSearchEntry <<- new("RzSearchEntry")
      variable.view <<- NULL
      variable.view.list <<- list()
      
      win["title"] <<- "Rz"
      win$setDefaultSize(800, 700)
      win["window-position"] <<- GtkWindowPosition["center"]
      progress.bar["width-request"] <<- 150
      
      gSignalConnect(info.bar, "response", .self$onInfoBarResponsed)
      
      rzActionGroup <<- new("RzActionGroup")
      rzActionGroup$load()
      
      rzMenu <<- new("RzMenu", action.group=rzActionGroup$getAction.group())
      gSignalConnect(main.view, "button-release-event" , rzMenu$popupmenu)
      accel.group <- rzMenu$getUimanager()$getAccelGroup()
      
      win$addAccelGroup(accel.group)
      rzAnalysisView$setAccel(accel.group)
      
      rzActionGroup$getA.plot.view()$setActive(rzSettings$getPlotViewEnabled())
      rzActionGroup$getA.analysis.view()$setActive(rzSettings$getAnalysisViewEnabled())
      gSignalConnect(rzActionGroup$getA.open(),      "activate", .self$onOpen)
      gSignalConnect(rzActionGroup$getA.save(),      "activate", .self$onSave)
      gSignalConnect(rzActionGroup$getA.ds(),        "activate", .self$onImportFromGlobalEnv)
      gSignalConnect(rzActionGroup$getA.ch.name(),   "activate", .self$onChangeDataSetName)
      gSignalConnect(rzActionGroup$getA.remove(),    "activate", .self$onRemove)
      gSignalConnect(rzActionGroup$getA.revert(),    "activate", .self$onRevert)
      gSignalConnect(rzActionGroup$getA.reload(),    "activate", .self$onReload)
      gSignalConnect(rzActionGroup$getA.delete(),    "activate", .self$onDelete)
      gSignalConnect(rzActionGroup$getA.duplicate(), "activate", .self$onDuplicate)
      gSignalConnect(rzActionGroup$getA.quit(),      "activate", win$destroy)
      gSignalConnect(rzActionGroup$getA.settings(),  "activate", .self$onSetting)
      gSignalConnect(rzActionGroup$getA.data.view(), "activate", .self$onDataView)
      gSignalConnect(rzActionGroup$getA.plot.view(), "toggled" , .self$onPlotViewToggled)
      gSignalConnect(rzActionGroup$getA.analysis.view(), "toggled" , .self$onAnalysisViewToggled)
      gSignalConnect(rzActionGroup$getA.tutorial(),  "activate", function(...) browseURL(gettext("http://m884.jp/RzTutorial.html")))
      gSignalConnect(rzActionGroup$getA.load.sample(), "activate", .self$onLoadSample)
      gSignalConnect(rzActionGroup$getA.value.lab(), "activate", .self$onEditValueLabels)
      
      rzDataHandler <<- new("RzDataHandler", data.collection=data.collection.obj)
      gSignalConnect(rzDataHandler$getData.set.list.combo(), "changed", .self$onDatasetChanged)
      
      vbox       <- gtkVBoxNew()
      data.handler.box  <- gtkHBoxNew(spacing=1)
      data.handler.box$packStart(rzDataHandler$getData.set.list.combo(), expand=TRUE, fill=TRUE)
      data.handler.box$packStart(rzSearchEntry$getEntry.search(), expand=FALSE)
      
      vbox$packStart(rzMenu$getMenu.bar(),     expand=FALSE, fill=FALSE)
      vbox$packStart(rzMenu$getTool.bar(),     expand=FALSE, fill=FALSE)
      vbox$packStart(info.bar,     expand=FALSE, fill=FALSE)
      vbox$packStart(data.handler.box, expand=FALSE, fill=FALSE, padding=2)
      vbox$packStart(main.hpaned,    expand=TRUE , fill=TRUE )
      vbox$packEnd  (status.bar,   expand=FALSE, fill=FALSE)
      rzMenu$getTool.bar()$showAll()
      win$add(vbox)
      win$show()
      win$present()
      if(!rzSettings$getAnalysisViewEnabled()) { rzAnalysisView$getMain()$hide() }
      if(!rzSettings$getPlotViewEnabled()) rzPlot$getMain()$hide()
      else                                 rzPlot$construct()
      
      gSignalConnect(win, "destroy", function(...){
        rzTools$clean()
        if(!is.null(rzSettings$getEmbededDeviceOn()) && rzSettings$getEmbededDeviceOn()){
          try(dev.off(), silent=TRUE)
        }
      })
      
    },
    
    show = function(){
      win$show()
      win$present()
    },
    
    # actions
    onEditValueLabels = function(action){
      if(is.null(variable.view)) return()
      variable.view$onEditValueLabels()
    },
    
    onDelete = function(action){
      if(is.null(variable.view)) return()
      dialog <- gtkMessageDialogNew(win, "destroy-with-parent",
                                    GtkMessageType["question"], GtkButtonsType["ok-cancel"],
                                    gettext("Are you sure you want to do that?"))
      response <- dialog$run()
      dialog$hide()
      
      if(response==GtkResponseType["ok"]){
        variable.view$onDelete()
      }
    },

    onDuplicate = function(action){
      if(is.null(variable.view)) return()
      variable.view$onDuplicate()
    },
    
    onDataView = function(action){
      if(!is.null(variable.view)){
        rzDataView <- new("RzDataView", RzData=rzDataHandler$getCurrentData())        
      }
    },
    
    onPlotViewToggled = function(action){
      if (!rzPlot$getConstructed()) {
        rzPlot$construct()
      }
      view <- rzPlot$getMain()
      parent <- view$getParent()
      if(action$getActive()) {
        if(class(parent)[1] == "GtkWindow") parent$show()    # if detached
        view$showAll()
        rzSettings$setPlotViewEnabled(TRUE)
      } else {
        if(class(parent)[1] == "GtkWindow") parent$hide()    # if detached
        view$hide()
        rzSettings$setPlotViewEnabled(FALSE)
      }
    },
    
    onAnalysisViewToggled = function(action){
      view <- rzAnalysisView$getMain()
      parent <- view$getParent()
      if(action$getActive()) {
        if(class(parent)[1] == "GtkWindow") parent$show()    # if detached
        view$showAll()
        rzAnalysisView$toggled()
        rzSettings$setAnalysisViewEnabled(TRUE)
        if(!is.null(variable.view)) variable.view$selectMode(TRUE)
      } else {
        if(class(parent)[1] == "GtkWindow") parent$hide()    # if detached
        view$hide()
        rzSettings$setAnalysisViewEnabled(FALSE)
        if(!is.null(variable.view)) variable.view$selectMode(FALSE)
      }
    },
    
    onSetting = function(action){
      rzSettings$runDialog(win)
      if(!is.null(variable.view))variable.view$changeFont()
      rzActionGroup$getA.reload()$setSensitive(rzSettings$getUseDataSetObject())
      gtkRcReparseAll()
    },
    
    onOpen                = function(action){
      timeoutid <- gTimeoutAdd(80, progress.bar$start)
      data <- rzDataSetIO$open(win, info.bar)
      gSourceRemove(timeoutid)
      progress.bar["activity-mode"] <<- FALSE
      if(!is.null(data)) {
        rzDataHandler$addData(data)
      }
    },
    
    onSave                = function(action){
      timeoutid <- gTimeoutAdd(80, progress.bar$start)
      data <- rzDataHandler$getCurrentData()
      rzDataSetIO$save(win, data)
      gSourceRemove(timeoutid)
      progress.bar["activity-mode"] <<- FALSE
    },
    
    onImportFromGlobalEnv = function(action){
      timeoutid <- gTimeoutAdd(80, progress.bar$start)
      data <- rzDataSetIO$importFromGlobalEnv(win)
      if(!is.null(data)) {
        rzDataHandler$addData(data)
      }
      gSourceRemove(timeoutid)
      progress.bar["activity-mode"] <<- FALSE
    },
    
    onLoadSample = function(action){
      timeoutid <- gTimeoutAdd(80, progress.bar$start)
      nes1948.por <- UnZip("anes/NES1948.ZIP","NES1948.POR",package="memisc")
      nes1948 <- spss.portable.file(nes1948.por)
      sample.data.set <- as.data.set(nes1948)
      data <- new("RzData", file.path=NULL, data.set=sample.data.set,
                  original.name=gettext("NES1948 [Sample Dataset in memisc]"))
      rzDataHandler$addData(data)
      gSourceRemove(timeoutid)
      progress.bar["activity-mode"] <<- FALSE
    },
    
    onChangeDataSetName   = function(action){
      onActivate <- function(entry, dialog){
        dialog$response(GtkResponseType["ok"])
      }
      onResponse <- function (dialog, response.id, entry) {
        if (response.id==GtkResponseType["ok"]) {
          new.name <- localize(entry$getText())
          result <- rzDataHandler$changeDataSetName(data.set.name, new.name)
          if(result$result){
            rm(list=data.set.name, envir=.GlobalEnv)
            rm(list=sprintf("%s.ds", data.set.name), envir=.GlobalEnv)
            match <- which(names(variable.view.list)==data.set.name)
            names(variable.view.list)[match] <<- new.name
            dialog$hide()            
          } else {
            dialog2 <- gtkMessageDialogNew(dialog, "destroy-with-parent",
                                           GtkMessageType["error"],
                                           GtkButtonsType["close"],
                                           result$message)
            dialog2$run()
            dialog2$hide()
            dialog$run()
            return()
            
          }
        } else {
          dialog$hide()
        }        
      }
      data.set.name <- rzDataHandler$getCurrentDataSetName()
      if ( length(data.set.name) == 0 ) return()
      txt <- paste(gettextf("Old Name"),":", sprintf(" <b>%s</b>", data.set.name), sep="")
      label1 <- gtkLabelNew()
      label1$setMarkup(txt)
      label1$show()
      label2 <- gtkLabelNew(gettext("New Name"))
      entry1 <- gtkEntryNew()
      hbox1 <- gtkHBoxNew()
      hbox2 <- gtkHBoxNew()
      hbox1$packStart(label1, expand = FALSE, padding = 5)
      hbox2$packStart(label2, expand = FALSE, padding = 5)
      hbox2$packStart(entry1, expand = TRUE, fill = TRUE, padding = 5)
      
      vbox1 <- gtkVBoxNew()
      vbox1$packStart(hbox1, expand = FALSE, padding = 2)
      vbox1$packStart(hbox2, expand = FALSE, padding = 2)
      
      dialog <- gtkDialogNewWithButtons(title=gettext("Change the Name of Current Dataset"), parent=win, flags=c("modal", "destroy-with-parent"),
                                        "gtk-ok", GtkResponseType["ok"], 
                                        "gtk-cancel", GtkResponseType["cancel"],
                                        show=FALSE)
      dialog$setResizable(FALSE)
      dialog$setDefaultSize(200, 130)
      dialog[["vbox"]]$packStart(vbox1, expand = FALSE, padding = 2)
      gSignalConnect(entry1, "activate", onActivate, dialog)
      gSignalConnect(dialog, "response", onResponse, entry1)
      response <- dialog$run()
    },
    
    onRemove              = function(action){
      data.set.name <- rzDataHandler$getCurrentDataSetName()
      if ( length(data.set.name) == 0 ) return()
      dialog <- gtkMessageDialogNew(win, "destroy-with-parent",
                                    GtkMessageType["question"], GtkButtonsType["ok-cancel"],
                                    gettext("Are you sure you want to do that?"))
      response <- dialog$run()
      dialog$hide()
      
      if(response==GtkResponseType["ok"]){
        rzDataHandler$removeCurrentData()
        variable.view$toggleView()
        variable.view$getView()$destroy()
        variable.view <<- NULL
        variable.view.list[data.set.name] <<- NULL
        rzTools$setVariableView(variable.view)
        rzAnalysisView$toggled()
      }
    },
    
    onRevert = function(action){
      if(!is.null(variable.view)){
        timeoutid <- gTimeoutAdd(80, progress.bar$start)
        
        dialog <- gtkMessageDialogNew(win, "destroy-with-parent",
                                      GtkMessageType["question"], GtkButtonsType["ok-cancel"],
                                      gettext("Are you sure you want to do that?"))
        response <- dialog$run()
        dialog$hide()
        
        if(response==GtkResponseType["ok"]){
          data <- rzDataHandler$getCurrentData()
          data$revert()
          variable.view$reload()
        }
        gSourceRemove(timeoutid)
        progress.bar["activity-mode"] <<- FALSE
      }
    },
    
    onReload = function(action){
      on.exit(gSourceRemove(timeoutid))
      on.exit(progress.bar["activity-mode"] <<- FALSE, add = TRUE)
      timeoutid <- gTimeoutAdd(80, progress.bar$start)
      if(!is.null(variable.view)){
        
        dialog <- gtkMessageDialogNew(win, "destroy-with-parent",
                                      GtkMessageType["question"], GtkButtonsType["ok-cancel"],
                                      gettext("Are you sure you want to do that?"))
        response <- dialog$run()
        dialog$hide()
        
        if(response==GtkResponseType["ok"]){
            variable.view$reload()
        }
      }
      
    },
    
    onDatasetChanged      = function(combo){
      data.set.name <- rzDataHandler$getCurrentDataSetName()
      if ( length(data.set.name) == 0 ) return()
      if ( !is.null(variable.view) ){
        variable.view$toggleView()
      }
      variable.view <<- variable.view.list[[data.set.name]]
      if ( is.null(variable.view) ) {
        timeoutid <- gTimeoutAdd(80, progress.bar$start)
        variable.view <<- new("RzVariableView", data=rzDataHandler$getData(data.set.name),
                              win=win, rzPlot=rzPlot)
        variable.view$construct()
        variable.view.list[[data.set.name]] <<- variable.view
        main.view$packStart(variable.view$getView())
        gSourceRemove(timeoutid)
        progress.bar["activity-mode"] <<- FALSE
      }
      rzTools$setVariableView(variable.view)
      rzAnalysisView$toggled()
      rzDataHandler$sync(data.set.name)
      variable.view$toggleView(rzSearchEntry)
      
      win["title"] <<- paste("Rz -", data.set.name)
      if (!is.null(recode.id)) gSignalHandlerDisconnect(rzActionGroup$getA.recode(), recode.id)
      recode.id <<- gSignalConnect(rzActionGroup$getA.recode(),  "activate", variable.view$onRecode, win)
    },
    
    onInfoBarResponsed    = function(widget, response.id){
      info.bar$hide()
      
    },
    
    # scripting interface
    addData = function(data){
      rzDataHandler$addData(data)
    },
    
    reloadData = function(data.set.name=NULL, ask = TRUE){
      vv.tmp <- NULL
      if (is.null(data.set.name)) {
        vv.tmp <- variable.view
      } else {
        vv.tmp <- variable.view.list[[data.set.name]]
      }
      if (! is.null(vv.tmp)) {
        response <- 1
        if (ask) {
          response <- menu(c(gettext("yes"), gettext("no")),
                           title = gettext("Are you sure you want to do that?"))
        }
        
        if (response == 1) {
          vv.tmp$reload()
        }
        
      } else {
        if (is.null(data.set.name)) {
          stop("Please select a dataset on Rz or specify \"data.set.name\".")
        } else {
          stop("The dataset named \"", data.set.name, "\" doesn't exist.")
        }
      }
    }
  )
)
main$accessors("variable.view")
