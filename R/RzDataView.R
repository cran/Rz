dataView <-
setRefClass("RzDataView",
  fields = c("RzData", "model"),
  methods = list(
    initialize        = function(...) {
      initFields(...)
      df <- RzData$getData.frame()
      df2 <- data.frame(index=seq_len(nrow(df)))
      
      model <<- rGtkDataFrameNew(df)
      treeView <- gtkTreeViewNewWithModel(model)
      treeView["enable-grid-lines"] <- GtkTreeViewGridLines["both"]
      treeView["fixed-height-mode"] <- TRUE
      treeView["rules-hint"] <- TRUE

      model2 <- rGtkDataFrameNew(df2)
      treeView2 <- gtkTreeViewNewWithModel(model2)
      treeView2["enable-grid-lines"] <- GtkTreeViewGridLines["both"]

      rt.index    <- gtkCellRendererText()
      color       <- rt.index["cell-background-gdk"]
      color$red   <- 45000L
      color$green <- 45000L
      color$blue  <- 45000L
      rt.index["cell-background-gdk"] <- color
      rt.index["xalign"] <- 0.5
      renderer <- gtkCellRendererText()
      
      columns <- lapply(seq_len(dim(model)[2]),function(x){
        gtkTreeViewColumn(title=dimnames(model)[[2]][x], cell=renderer, "text"= x-1)})
      lapply(columns, gtkTreeViewColumnSetSizing, "fixed")
      lapply(columns, gtkTreeViewColumnSetResizable, TRUE)
      lapply(columns, gtkTreeViewColumnSetFixedWidth, 50)
      lapply(columns, treeView$AppendColumn)
      
      column1 <- gtkTreeViewColumn(" ", cell=rt.index, "text"=0)
      column1$setSizing("automatic")
      column1$setResizable(FALSE)
      treeView2$appendColumn(column1)
      
      scrolledWindow <- gtkScrolledWindowNew()
      scrolledWindow$add(treeView)
      
      scrolledWindow2 <- gtkScrolledWindowNew(NULL, scrolledWindow$getVadjustment())
      scrolledWindow2["vscrollbar-policy"] <- GtkPolicyType["never"]
      scrolledWindow2$add(treeView2)
      
      hbox <- gtkHBoxNew()
      hbox$packStart(scrolledWindow2, expand=FALSE)
      hbox$packStart(scrolledWindow)
      
      win <- gtkWindowNew(show=FALSE)
      win$add(hbox)
      win["width-request"] <- 800
      win["height-request"] <- 600
      win["allow-shrink"] <- TRUE
      win["title"] <- gettext("Data View")
      win["window-position"] <- GtkWindowPosition["center"]
      win$showAll()
    }
  )
)
#dataView$accessors(c())
