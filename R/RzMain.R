main <-
setRefClass("RzMain",
  fields = c("recode.id", "win",
             "main.hpaned", "main.view", "plot.view", "variable.view", "entry.search",
             "info.bar", "message.label", "status.bar", "progress.bar", "actions", "variable.view.list",
             "rzActionGroup", "rzMenu", "rzDataHandler", "rzSearchEntry", "rzDataSetIO", "rzPlot",
             "rzVariableEditorView", "view.box"),
  methods = list(
    initialize            = function(...) {
      initFields(...)
      settings <- gtkSettingsGetDefault()
      settings$setStringProperty("gtk-font-name", rzSettings$getGlobalFont(), NULL)
      gtkRcReparseAll()
      rzDataSetIO   <<- new("RzDataSetIO")
      recode.id     <<- NULL
      win           <<- gtkWindowNew(show=FALSE)
      main.hpaned   <<- gtkHPanedNew()
      main.view     <<- gtkVBoxNew()
      plot.view     <<- gtkVPanedNew()
      rzVariableEditorView <<- new("RzVariableEditorView")
      view.box      <<- gtkHBoxNew()
      view.box$packStart(plot.view)
      view.box$packStart(rzVariableEditorView$getMain())
      
      main.hpaned$pack1(main.view)
      main.hpaned$pack2(view.box)
      main.hpaned$setPosition(300)
      
      info.bar      <<- gtkInfoBarRzNew()
      message.label <<- gtkLabelNew()
      info.bar$addButton("gtk-ok", GtkResponseType["ok"])
      info.bar$setMessageType(GtkMessageType["info"])
      info.bar$getContentArea()$packStart(message.label, expand=FALSE, fill=FALSE)
      info.bar$hide()
      info.bar$setNoShowAll(TRUE)
      status.bar    <<- gtkStatusbarNew()
      progress.bar  <<- gtkProgressBarNew()
      
      status.bar$packEnd(progress.bar, expand=FALSE)
      
      rzSearchEntry <<- new("RzSearchEntry")
      variable.view <<- NULL
      variable.view.list <<- list()
      
      rzPlot <<- new("RzPlot", win=win)
      rzPlot$setInfo.bar(info.bar)
      rzVariableEditorView$setInfo.bar(info.bar)
      plot.view$pack2(rzPlot$getMain(), resize=FALSE)
      if(rzSettings$getUseEmbededDevice()){
        plot.area <- gtkDrawingArea()
        plot.view$pack1(plot.area, resize=TRUE)
        plot.view$setPosition(300)
        asCairoDevice(plot.area)
        rzSettings$setEmbededDeviceOn(TRUE)
      } else {
        rzSettings$setEmbededDeviceOn(FALSE)
      }
      
      win["title"] <<- "Rz"
      win$setDefaultSize(650, 600)
      progress.bar["width-request"] <<- 150
      
      gSignalConnect(info.bar, "response", .self$onInfoBarResponsed)
      
      rzActionGroup <<- new("RzActionGroup")
      rzActionGroup$load()
      
      rzMenu <<- new("RzMenu", action.group=rzActionGroup$getAction.group())
      gSignalConnect(main.view, "button-release-event" , rzMenu$popupmenu)
      accel.group <- rzMenu$getUimanager()$getAccelGroup()
      
      win$addAccelGroup(accel.group)
      rzVariableEditorView$setAccel(accel.group)
      
      rzActionGroup$getA.plot.view()$setActive(rzSettings$getPlotViewEnabled())
      rzActionGroup$getA.reload()$setSensitive(rzSettings$getUseDataSetObject())
      gSignalConnect(rzActionGroup$getA.open(),      "activate", .self$onOpen)
      gSignalConnect(rzActionGroup$getA.save(),      "activate", .self$onSave)
      gSignalConnect(rzActionGroup$getA.ds(),        "activate", .self$onImportFromGlobalEnv)
      gSignalConnect(rzActionGroup$getA.ch.name(),   "activate", .self$onChangeDataSetName)
      gSignalConnect(rzActionGroup$getA.remove(),    "activate", .self$onRemove)
      gSignalConnect(rzActionGroup$getA.revert(),    "activate", .self$onRevert)
      gSignalConnect(rzActionGroup$getA.reload(),    "activate", .self$onReload)
      gSignalConnect(rzActionGroup$getA.quit(),      "activate", win$destroy)
      gSignalConnect(rzActionGroup$getA.settings(),  "activate", .self$onSetting)
      gSignalConnect(rzActionGroup$getA.data.view(), "activate", .self$onDataView)
      gSignalConnect(rzActionGroup$getA.plot.view(), "toggled" , .self$onPlotViewToggled)
      gSignalConnect(rzActionGroup$getA.variable.editor.view(), "toggled" , .self$onVariableEditorViewToggled)
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
      if(!rzSettings$getPlotViewEnabled()) { plot.view$hide() }
      if(!rzSettings$getVariableEditorViewEnabled()) { rzVariableEditorView$getMain()$hide() }
      if(!rzSettings$getPlotViewEnabled() && !rzSettings$getVariableEditorViewEnabled()) {
        view.box$hide() 
      }
      
      gSignalConnect(win, "destroy", function(...){
        if(rzSettings$getEmbededDeviceOn()){
          try(dev.off(), silent=TRUE)
        }
      })
      
    },
    
    # actions
    onEditValueLabels = function(action){
      variable.view$onEditValueLabels(win=win)
    },
    
    onDataView = function(action){
      if(!is.null(variable.view)){
        rzDataView <- new("RzDataView", RzData=rzDataHandler$getCurrentData())        
      }
    },
    
    onPlotViewToggled = function(action){
      if(action$getActive()) {
        view.box$show()
        plot.view$show()
        rzSettings$setPlotViewEnabled(TRUE)
        if(rzSettings$getVariableEditorViewEnabled()){
          action <- rzActionGroup$getA.variable.editor.view()
          action["active"] <- FALSE
#          action$toggled()
        }
      } else {
        plot.view$hide()
        rzSettings$setPlotViewEnabled(FALSE)
        if(!rzSettings$getVariableEditorViewEnabled()){
          view.box$hide()
        }
      }
    },
    
    onVariableEditorViewToggled = function(action){
      if(action$getActive()) {
        view.box$show()
        rzVariableEditorView$getMain()$show()
        rzSettings$setVariableEditorViewEnabled(TRUE)
        if(!is.null(variable.view)) variable.view$selectMode(TRUE)
        if(rzSettings$getVariableEditorViewEnabled()){
          action <- rzActionGroup$getA.plot.view()
          action["active"] <- FALSE
#          action$toggled()
        }
      } else {
        rzVariableEditorView$getMain()$hide()
        rzSettings$setVariableEditorViewEnabled(FALSE)
        if(!is.null(variable.view)) variable.view$selectMode(FALSE)
        if(!rzSettings$getPlotViewEnabled()){
          view.box$hide()
        }
      }
    },
    
    onSetting = function(action){
      rzSettings$runDialog(win)
      rzDataHandler$syncAll()
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
      gSourceRemove(timeoutid)
      data <- rzDataSetIO$importFromGlobalEnv(win)
      progress.bar["activity-mode"] <<- FALSE
      if(!is.null(data)) {
        rzDataHandler$addData(data)
      }
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
        rm(list=data.set.name, envir=.GlobalEnv)
        variable.view$toggleView()
        variable.view$getSw()$destroy()
        variable.view <<- NULL
        variable.view.list[data.set.name] <<- NULL
        rzVariableEditorView$setVariableView(variable.view)
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
      if(!is.null(variable.view)){
        timeoutid <- gTimeoutAdd(80, progress.bar$start)
        
        dialog <- gtkMessageDialogNew(win, "destroy-with-parent",
                                      GtkMessageType["question"], GtkButtonsType["ok-cancel"],
                                      gettext("Are you sure you want to do that?"))
        response <- dialog$run()
        dialog$hide()
        
        if(response==GtkResponseType["ok"]){
          data   <- rzDataHandler$getCurrentData()
          result <- data$reloadFromGlobalEnv()
          if(result==TRUE){
            variable.view$reload()
          } else {
            dialog2 <- gtkMessageDialogNew(win, "destroy-with-parent",
                                           GtkMessageType["error"], GtkButtonsType["close"],
                                           gettextf("\"%s\" isn't a data.set or don't exist.", result))
            dialog2$run()
            dialog2$hide()
          }
        }
        gSourceRemove(timeoutid)
        progress.bar["activity-mode"] <<- FALSE
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
                              rzPlot=rzPlot)
        variable.view$construct()
        variable.view.list[[data.set.name]] <<- variable.view
        main.view$packStart(variable.view$getSw())
        gSourceRemove(timeoutid)
        progress.bar["activity-mode"] <<- FALSE
      }
      rzDataHandler$sync(data.set.name)
      variable.view$toggleView(rzSearchEntry)
      rzVariableEditorView$setVariableView(variable.view)
      
      win["title"] <<- paste("Rz -", data.set.name)
      if (!is.null(recode.id)) gSignalHandlerDisconnect(rzActionGroup$getA.recode(), recode.id)
      recode.id <<- gSignalConnect(rzActionGroup$getA.recode(),  "activate", variable.view$onRecode, win)
    },
    
    onInfoBarResponsed    = function(widget, response.id){
      info.bar$hide()
    }
  )
)

