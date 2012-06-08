ag <-
setRefClass("RzActionGroup",
  fields = c("action.group", "a.file", "a.open",
             "a.save", "a.ds", "a.duplicate", "a.delete", "a.selectall", "a.unselect",
             "a.quit", "a.edit","a.ch.name", "a.revert", "a.reload",
             "a.remove", "a.vlabs", "a.missing", "a.selectall", "a.unselect",
             "a.recode", "a.value.lab", "a.settings",
             "a.view", "a.data.view", "a.plot.view", "a.analysis.view",
             "a.help", "a.tutorial", "a.load.sample"),
  methods = list(
    initialize            = function(...) {
      initFields(...)

      a.file      <<- gtkActionNew("MenuFile", gettext("_File"))
      a.open      <<- gtkActionNew("Open", gettext("_Open"), gettext("Open"), stock.id=GTK_STOCK_OPEN)
      a.save      <<- gtkActionNew("Save As", gettext("Save _As"), gettext("Save As"), stock.id=GTK_STOCK_SAVE_AS)
      a.ds        <<- gtkActionNew("ImportFromGlobalEnv", gettext("Import from _Grobal Environment"), gettext("Import from Grobal Environment"))
      a.quit      <<- gtkActionNew("Close", gettext("_Close"), gettext("Close"), stock.id=GTK_STOCK_CLOSE)
      a.edit      <<- gtkActionNew("MenuEdit", gettext("_Edit"))
      a.ch.name   <<- gtkActionNew("ChageDataSetName", gettext("_Change the Name of Current Dataset"), gettext("Change the Name of Current Dataset"), stock.id=GTK_STOCK_BOLD)
      a.remove    <<- gtkActionNew("RemoveDataSet", gettext("_Remove Current Dataset"), gettext("Remove Current Dataset"))
      a.revert    <<- gtkActionNew("RevertToOriginal", gettext("Revert to Original DataSet"), gettext("Revert to Original DataSet"), stock.id=GTK_STOCK_REVERT_TO_SAVED)
      a.reload    <<- gtkActionNew("ReloadFromGlobalEnv", gettext("Reload from Grobal Environment"), gettext("Reload from Grobal Environment"))
      a.vlabs     <<- gtkActionNew("ValueLabels", gettext("Value Labels"))
      a.missing   <<- gtkActionNew("Missing", gettext("Missing Values"))
      a.recode    <<- gtkActionNew("Recode", gettext("Recode"))
#      a.selectall <<- gtkActionNew("SelectAll", gettext("Select All Variables"), gettext("Select All Variables"))
#      a.unselect  <<- gtkActionNew("Unselect", gettext("Unselect All Variables"), gettext("Unselect All Variables"))
      a.delete    <<- gtkActionNew("Delete", gettext("Delete Selected Variables"), gettext("Delete Selected Variables"))
      a.duplicate <<- gtkActionNew("Duplicate", gettext("Duplicate Selected Variables"), gettext("Duplicate Selected Variables"))
      a.value.lab <<- gtkActionNew("EditValueLabels", gettext("Edit Value Labels"))
      a.settings  <<- gtkActionNew("Settings", gettext("_Preferences"), gettext("Preferences"), stock.id=GTK_STOCK_PREFERENCES)
      a.view      <<- gtkActionNew("MenuView", gettext("_View"))
      a.data.view <<- gtkActionNew("DataView", gettext("Data"), gettext("Data"))
      a.plot.view <<- gtkToggleActionNew("PlotView", gettext("Plot"), gettext("Plot"))
      a.analysis.view <<- gtkToggleActionNew("AnalysisView", gettext("Analysis"), gettext("Analysis"))
      a.help      <<- gtkActionNew("MenuHelp", gettext("_Help"))
      a.tutorial  <<- gtkActionNew("Tutorial", gettext("Tutorial on Web"), gettext("Tutorial on Web"))
      a.load.sample <<- gtkActionNew("LoadSample", gettext("Load Sample Dataset"), gettext("Load Sample Dataset"))
#      image <- gFileIconNew(gFileNewForPath(file.path(rzSettings$getRzPath(), "images", "tick.png")))
#      a.selectall$setGicon(image)
#      image <- gFileIconNew(gFileNewForPath(file.path(rzSettings$getRzPath(), "images", "cross.png")))
#      a.unselect$setGicon(image)
      image <- gFileIconNew(gFileNewForPath(file.path(rzSettings$getRzPath(), "images", "table_add.png")))
      a.ds$setGicon(image)
      image <- gFileIconNew(gFileNewForPath(file.path(rzSettings$getRzPath(), "images", "table_delete.png")))
      a.remove$setGicon(image)
      image <- gFileIconNew(gFileNewForPath(file.path(rzSettings$getRzPath(), "images", "table_refresh.png")))
      a.reload$setGicon(image)
      image <- gFileIconNew(gFileNewForPath(file.path(rzSettings$getRzPath(), "images", "table_row_delete.png")))
      a.delete$setGicon(image)
      image <- gFileIconNew(gFileNewForPath(file.path(rzSettings$getRzPath(), "images", "table_row_insert.png")))
      a.duplicate$setGicon(image)
      image <- gFileIconNew(gFileNewForPath(file.path(rzSettings$getRzPath(), "images", "table.png")))
      a.data.view$setGicon(image)
      image <- gFileIconNew(gFileNewForPath(file.path(rzSettings$getRzPath(), "images", "order.png")))
      a.plot.view$setGicon(image)
      image <- gFileIconNew(gFileNewForPath(file.path(rzSettings$getRzPath(), "images", "application_form.png")))
      a.analysis.view$setGicon(image)
      a.open$setAccelPath("<Rz-Menu>/File/Open")
      a.save$setAccelPath("<Rz-Menu>/File/Save As")
      a.ds$setAccelPath("<Rz-Menu>/File/Import")
      a.quit$setAccelPath("<Rz-Menu>/File/Close")
      a.reload$setAccelPath("<Rz-Menu>/Edit/Reload")
      a.data.view$setAccelPath("<Rz-Menu>/View/Data View")
      a.analysis.view$setAccelPath("<Rz-Menu>/View/Analysis View")
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
#      action.group$addAction(a.selectall)
#      action.group$addAction(a.unselect)
      action.group$addAction(a.delete)
      action.group$addAction(a.duplicate)
      action.group$addAction(a.settings)

      action.group$addAction(a.view)
      action.group$addAction(a.data.view)
      action.group$addAction(a.plot.view)
      action.group$addAction(a.analysis.view)
      
      action.group$addAction(a.help)
      action.group$addAction(a.tutorial)
      action.group$addAction(a.load.sample)

      action.group$addAction(a.recode)
      action.group$addAction(a.value.lab)
      
      
    }
  )
)
ag$accessors(c("action.group", "a.file", "a.open",
               "a.save", "a.ds", "a.quit",
               "a.edit", "a.ch.name", "a.remove",
               "a.revert", "a.reload",
               "a.vlabs", "a.duplicate", "a.delete", "a.selectall", "a.unselect",
               "a.missing", "a.recode", "a.value.lab", "a.settings",
               "a.data.view", "a.plot.view", "a.analysis.view",
               "a.tutorial", "a.load.sample"))

