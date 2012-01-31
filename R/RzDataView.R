dataView <-
setRefClass("RzDataView",
  fields = c("RzData", "model"),
  methods = list(
    initialize        = function(...) {
      initFields(...)
      df <- RzData$getData.frame()
      df <- cbind(index=seq_len(nrow(df)), df)
      model <<- rGtkDataFrameNew(df)
      treeView <- gtkTreeViewNewWithModel(model)
      treeView["enable-grid-lines"] <- GtkTreeViewGridLines["both"]
      treeView["fixed-height-mode"] <- TRUE
      treeView["rules-hint"] <- TRUE

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
      columns[[1]] <- gtkTreeViewColumn("", cell=rt.index, "text"=0)
      lapply(columns, gtkTreeViewColumnSetSizing, "fixed")
      lapply(columns, gtkTreeViewColumnSetResizable, TRUE)
      lapply(columns, gtkTreeViewColumnSetFixedWidth, 50)
      lapply(columns, treeView$AppendColumn)
      
      scrolledWindow <- gtkScrolledWindowNew()
      scrolledWindow$add(treeView)
      win <- gtkWindowNew(show=FALSE)
      win$add(scrolledWindow)
      win["width-request"] <- 800
      win["height-request"] <- 600
      win["title"] <- gettext("Data View")
      win["window-position"] <- GtkWindowPosition["center"]
      win$showAll()
    }
  )
)
#dataView$accessors(c())
