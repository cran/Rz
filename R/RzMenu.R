menu <- 
setRefClass("RzMenu",
  fields = c("action.group", "uimanager", "menu.bar", "tool.bar", "context.menu"),
  methods = list(
    initialize            = function(...) {
      initFields(...)

      uimanager     <<- gtkUIManagerNew()
      uimanager$insertActionGroup(action.group, 0)
      uimanager$addUiFromFile(file.path(rzSettings$getRzPath(), "ui", "menu.ui"))
      menu.bar     <<- uimanager$getWidget("/MenuBar")
      tool.bar     <<- uimanager$getWidget("/ToolBar")
      context.menu <<- uimanager$getWidget("/PopupVV") 
      tool.bar["toolbar-style"] <<- GtkToolbarStyle["icons"]
      tool.bar$setIconSize(GtkIconSize["small-toolbar"])
      nitems <- tool.bar$getNItems()
      for(i in seq_len(nitems)){
        item <- tool.bar$getNthItem(i-1)
        child <- item$getChild()
        child["can-focus"] <- FALSE
      }
    },
    popupmenu = function(obj, event) {
        button <- event$GetButton()
        if (button==3) context.menu$popup(button=button,activate.time=event$getTime())
        return(TRUE)
    }
))
menu$accessors(c("uimanager", "menu.bar", "tool.bar", "context.menu"))
