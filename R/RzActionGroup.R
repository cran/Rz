ag <-
setRefClass("RzActionGroup",
  fields = c("action.group", "a.file", "a.open",
             "a.save", "a.ds",
             "a.quit", "a.edit","a.ch.name", "a.revert", "a.reload",
             "a.remove", "a.vlabs", "a.missing",
             "a.recode", "a.value.lab", "a.settings",
             "a.view", "a.data.view", "a.plot.view", "a.variable.editor.view"),
  methods = list(
    initialize            = function(...) {
      initFields(...)

      a.file      <<- gtkActionNew("MenuFile", gettext("_File"))
      a.open      <<- gtkActionNew("Open", gettext("_Open"), gettext("Open"), stock.id=GTK_STOCK_OPEN)
      a.save      <<- gtkActionNew("Save As", gettext("Save _As"), gettext("Save As"), stock.id=GTK_STOCK_SAVE_AS)
      a.ds        <<- gtkActionNew("ImportFromGlobalEnv", gettext("Import from _Grobal Environment"), gettext("Import from Grobal Environment"), stock.id=GTK_STOCK_ADD)
      a.quit      <<- gtkActionNew("Close", gettext("_Close"), gettext("Close"), stock.id=GTK_STOCK_CLOSE)
      a.edit      <<- gtkActionNew("MenuEdit", gettext("_Edit"))
      a.ch.name   <<- gtkActionNew("ChageDataSetName", gettext("_Change the Name of Current Dataset"), gettext("Change the Name of Current Dataset"), stock.id=GTK_STOCK_BOLD)
      a.remove    <<- gtkActionNew("RemoveDataSet", gettext("_Remove Current Dataset"), gettext("Remove Current Dataset"), stock.id=GTK_STOCK_DELETE)
      a.revert    <<- gtkActionNew("RevertToOriginal", gettext("Revert to Original DataSet"), gettext("Revert to Original DataSet"), stock.id=GTK_STOCK_REVERT_TO_SAVED)
      a.reload    <<- gtkActionNew("ReloadFromGlobalEnv", gettext("Reload from Grobal Environment"), gettext("Reload from Grobal Environment"), stock.id=GTK_STOCK_REFRESH)
      a.vlabs     <<- gtkActionNew("ValueLabels", gettext("Value Labels"))
      a.missing   <<- gtkActionNew("Missing", gettext("Missing Values"))
      a.recode    <<- gtkActionNew("Recode", gettext("Recode"))
      a.value.lab   <<- gtkActionNew("EditValueLabels", gettext("Edit Value Labels"))
      a.settings  <<- gtkActionNew("Settings", gettext("_Preferences"), gettext("Preferences"), stock.id=GTK_STOCK_PREFERENCES)
      a.view      <<- gtkActionNew("MenuView", gettext("_View"))
      a.data.view <<- gtkActionNew("DataView", gettext("Data View"), gettext("Data View"))
      a.plot.view <<- gtkToggleActionNew("PlotView", gettext("Plot View"), gettext("Plot View"))
      a.variable.editor.view <<- gtkToggleActionNew("QuickEditorView", gettext("Quick Editor View"), gettext("Quick Editor View"))
      icon.data.view <- gFileIconNew(gFileNewForPath(file.path(rzSettings$getRzPath(), "images", "table.png")))
      a.data.view$setGicon(icon.data.view)
      icon.plot.view <- gFileIconNew(gFileNewForPath(file.path(rzSettings$getRzPath(), "images", "order.png")))
      a.plot.view$setGicon(icon.plot.view)
      icon.variable.editor.view <- gFileIconNew(gFileNewForPath(file.path(rzSettings$getRzPath(), "images", "table_edit.png")))
      a.variable.editor.view$setGicon(icon.variable.editor.view)
      a.open$setAccelPath("<Rz-Menu>/File/Open")
      a.save$setAccelPath("<Rz-Menu>/File/Save As")
      a.ds$setAccelPath("<Rz-Menu>/File/Import")
      a.quit$setAccelPath("<Rz-Menu>/File/Close")
      a.reload$setAccelPath("<Rz-Menu>/Edit/Reload")
      a.data.view$setAccelPath("<Rz-Menu>/View/Data View")
      a.variable.editor.view$setAccelPath("<Rz-Menu>/View/Quick Editor View")
      a.plot.view$setAccelPath("<Rz-Menu>/View/Plot View")
      
      
      action.group  <<- gtkActionGroupNew()
      action.group$setTranslationDomain("pkg-RGtk2")
    },
    
    load = function(){
      action.group$addAction(a.file)
      action.group$addAction(a.open)
      action.group$addAction(a.save)
      action.group$addAction(a.ds)
      action.group$addAction(a.quit)

      action.group$addAction(a.edit)
      action.group$addAction(a.ch.name)
      action.group$addAction(a.remove)
      action.group$addAction(a.revert)
      action.group$addAction(a.reload)
      action.group$addAction(a.settings)

      action.group$addAction(a.view)
      action.group$addAction(a.data.view)
      action.group$addAction(a.plot.view)
      action.group$addAction(a.variable.editor.view)
      
      action.group$addAction(a.recode)
      action.group$addAction(a.value.lab)
    }
  )
)
ag$accessors(c("action.group", "a.file", "a.open",
               "a.save", "a.ds", "a.quit",
               "a.edit", "a.ch.name", "a.remove",
               "a.revert", "a.reload",
               "a.vlabs",
               "a.missing", "a.recode", "a.value.lab", "a.settings",
               "a.data.view", "a.plot.view", "a.variable.editor.view"))

