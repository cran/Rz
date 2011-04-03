formals(gettext)$domain <- "Rz"
formals(gettextf)$domain <- "Rz"

#script.buffer <-
#setRefClass("RzScriptBuffer",
#  fields = c("script", "buffer")
#  )

#sb.obj <- script.buffer$new(script="test", buffer=gtkTextBufferNew())

data <-
setRefClass("RzData",
  fields = c("file.path", "data.set.name", "original.name",
             "data.set", "original.data.set", "data.frame",
             "log",
             "system", "package.version", "encoding", "time"),
  methods = list(
    initialize        = function(...) {
      data.set.name     <<- NULL
      original.data.set <<- data.set
      data.frame        <<- NULL
      log               <<- NULL
      system            <<- R.Version()
      package.version   <<- list(Rz = packageVersion("Rz"),
                                 memisc=packageVersion("memisc"),
                                 RGtk2=packageVersion("RGtk2"))
      encoding          <<- localeToCharset()
      time              <<- Sys.time()
      initFields(...)
    },
    construct         = function() { data.frame <<- as.data.frame(data.set) },
    ncol              = function() length(description(data.set)),
    constructVariable = function(col){
      data.frame[[col]] <<- as.data.frame(data.set[[col]])[[1]]
      names(data.frame) <<- names(data.set)
    },
    linkDataFrame     = function(){
      assign(data.set.name, data.frame, envir=.GlobalEnv)
    },
    getVariableLabels = function() gsub("(^')|('$)", "", as.character(description(data.set))),
    getVariableNames  = function() names(description(data.set)),
    getMeasurement    = function() sapply(data.set, measurement),
    getMissingValues  = function() {
      miss.val  <- lapply(data.set, missing.values)
      miss.val  <- sapply(miss.val, function(x){ifelse(is.null(x), "", paste(x@filter, collapse=","))})
    },
    getValueLabels    = function() {
      val.labs  <- lapply(data.set, labels)
      val.labs  <- sapply(val.labs, function(x){ifelse(is.null(x), "", paste(x@values, "\"",x@.Data, "\"", collapse=", "))})
    },
    getSummaries      = function() {
      summaries   <- lapply(data.set, summary)
      val <- lapply(summaries, as.character)
      lab <- lapply(summaries, names)
      summaies <- sapply(seq_along(val),
                           function(x){
                            lnchar <- nchar(sub("", "", lab[[x]]), type="width") # "sub()" to remove invalid character
                            lnchar <- max(lnchar) - lnchar
                            vnchar <- nchar(val[[x]], type="width")
                            vnchar <- max(vnchar) - vnchar
                            paste(lab[[x]],
                                  sapply(sapply(lnchar, function(x) rep(" ", x)), paste, collapse=""),
                                  "\t:",
                                  sapply(sapply(vnchar, function(x) rep(" ", x)), paste, collapse=""),
                                  val[[x]], sep="", collapse="\n")
                           }
                         )
    },
    getSummary = function(row){
      summary <- summary(data.set[[row]])
      lab <- names(summary)
      val <- as.character(summary)
      lnchar <- nchar(sub("", "", lab), type="width") # "sub()" is for removing invalid character.
      lnchar <- max(lnchar) - lnchar
      vnchar <- nchar(val, type="width")
      vnchar <- max(vnchar) - vnchar
      summary <- paste(lab,
                       sapply(sapply(lnchar, function(x) rep(" ", x)), paste, collapse=""),
                       "\t:",
                       sapply(sapply(vnchar, function(x) rep(" ", x)), paste, collapse=""),
                       val, sep="", collapse="\n")
      return(summary)
    }
  )
)
data$accessors(c("file.path", "data.set.name", "original.name", "data.set"))

data.collection <-
setRefClass("RzDataCollection",
  fields = c("data.collection"),
  methods = list(
    initialize       = function(...) {
      data.collection <<- list()
      initFields(...)
    },
    addData          = function(data) {
      names <- .self$getDataSetNames()
      match <- grep("^dataset*", names, value=TRUE)
      match <- suppressWarnings(as.numeric(gsub("dataset", "", match)))
      match <- c(0, match)
      data$setData.set.name(sprintf("dataset%s", max(match, na.rm=TRUE)+1 ))
      data.collection[[ length(data.collection) + 1 ]] <<- data
    },
    removeData       = function(data.set.name){
      names <- .self$getDataSetNames()      
      data.collection[ which(names == data.set.name) ] <<- NULL
    },
    getDataSetNames  = function(){
      names <- lapply(data.collection, function(x) x$getData.set.name())
      return( unlist(names) )
    },
    getOriginalNames = function(){
      names <- lapply(data.collection, function(x) x$getOriginal.name())
      return( unlist(names) )
    },
    getData          = function(data.set.name){
      names <- .self$getDataSetNames()      
      data  <- data.collection[[ which(names == data.set.name) ]]
      return(data)
    },
    getLength        = function(){length(data.collection)}
  )
)

variable.view <- 
setRefClass("RzVariableView",
  fields = c("data","main", "liststore", "sw", "column.definition", "summaries",
             "rt.index", "rt.vars", "rt.var.labs", "rt.val.labs", "rc.msr", "rt.missing"),
  methods = list(
    initialize  = function(...) {
      column.definition <<- c(index=0, vars=1, var.labs=2, msr=3, val.labs=4, missing=5)
      liststore <<- gtkListStoreNew("character", "character", "character", "character", "character", "character")
      main <<- gtkTreeViewNewWithModel(liststore)
      sw   <<- gtkScrolledWindowNew()
      sw$add(main)
      sw$setPolicy(GtkPolicyType["automatic"], GtkPolicyType["automatic"])
      main["enable-grid-lines"] <<- GtkTreeViewGridLines["both"]
      main["has-tooltip"] <<- TRUE
      rt.index    <<- gtkCellRendererText()
      rt.vars     <<- gtkCellRendererText()
      rt.var.labs <<- gtkCellRendererText()
      rt.val.labs <<- gtkCellRendererText()
      rt.missing  <<- gtkCellRendererText()
      rt.vars    ["editable"] <<- TRUE
      rt.var.labs["editable"] <<- TRUE
      rt.missing ["editable"] <<- TRUE

     combo.list <- gtkListStoreNew("character")
     sapply(c("nominal","ordinal","interval","ratio"),
            function(x) {
              iter <- combo.list$append()$iter
              combo.list$set(iter, 0, x)
            })
      rc.msr <<- gtkCellRendererCombo()
      rc.msr["model"] <<- combo.list
      rc.msr["text-column"] <<- 0
      rc.msr["editable"] <<- TRUE
      rc.msr["has-entry"] <<- FALSE
      
      gSignalConnect(main, "row-activated", .self$onRowActivated)
      gSignalConnect(main, "query-tooltip", .self$onQueryTooltip)
      gSignalConnect(rt.vars    , "edited", .self$onCellEditedVars)
      gSignalConnect(rt.var.labs, "edited", .self$onCellEditedVarLabs)
      gSignalConnect(rc.msr     , "edited", .self$onCellEditedMsr)
      gSignalConnect(rt.missing , "edited", .self$onCellEditedMissing)

      initFields(...)
    },
    construct   = function() {
      vars      <-  data$getVariableNames()
      var.labs  <-  data$getVariableLabels()
      msr       <-  data$getMeasurement()
      val.labs  <-  data$getValueLabels()
      miss.val  <-  data$getMissingValues()
      summaries <<- data$getSummaries()
      for ( i in seq_len(data$ncol()) ) {
        iter <- liststore$append()$iter
        liststore$set(iter,
          column.definition[["index"]], i,
          column.definition[["vars"]], vars[i],
          column.definition[["var.labs"]], var.labs[i],
          column.definition[["msr"]], msr[i],
          column.definition[["val.labs"]], val.labs[i],
          column.definition[["missing"]], miss.val[i])
      }
      columns <- list(
        index   = gtkTreeViewColumnNewWithAttributes(""                     , rt.index   , "text"=column.definition[["index"]]   ),
        vars    = gtkTreeViewColumnNewWithAttributes(gettext("Names")       , rt.vars    , "text"=column.definition[["vars"]]    ),
        labs    = gtkTreeViewColumnNewWithAttributes(gettext("Labels")      , rt.var.labs, "text"=column.definition[["var.labs"]]),
        msr     = gtkTreeViewColumnNewWithAttributes(gettext("Measurement") , rc.msr     , "text"=column.definition[["msr"]]     ),
        val.labs= gtkTreeViewColumnNewWithAttributes(gettext("Value Labels"), rt.val.labs, "text"=column.definition[["val.labs"]]),
        missing = gtkTreeViewColumnNewWithAttributes(gettext("Missing")     , rt.missing , "text"=column.definition[["missing"]] )
      )
      lapply(columns, gtkTreeViewColumnSetSizing   , "fixed")
      lapply(columns, gtkTreeViewColumnSetResizable, TRUE)
      lapply(columns, gtkTreeViewColumnSetSpacing  , 1)
      columns$index$setSizing("automatic")
      columns$index$setResizable(FALSE)
      columns$vars$setFixedWidth(50)
      columns$labs$setFixedWidth(250)
      columns$val.labs$setFixedWidth(100)
      columns$msr$setSizing("automatic")
      columns$msr$setMinWidth(80)
      columns$msr$setResizable(FALSE)
      columns$missing$setSizing("automatic")
      columns$missing$setResizable(FALSE)
      lapply(columns, function(column) main$appendColumn(column))
    },
    setCell     = function(path, col, new.value){
      iter <- liststore$getIterFromString(path)$iter
      if (liststore$iterIsValid(iter)) {
        liststore$set(iter, col, new.value)
      } else {
        iter <- liststore$append()$iter
        liststore$set(iter, col, new.value) 
      }
    },
    getSelected = function(){
      iter  <- main$getSelection()$getSelected()$iter
      value <- liststore$get(iter, unlist(column.definition))
      value <- lapply(value, localize)
      value <- unlist(value)
      names(value) <- names(column.definition)
      return(value)
    },
    toggleView  = function(entry=NULL, f=NULL){
      if (is.null(entry)){
        main$setSearchEntry(NULL)
        sw$hideAll()
      } else {
        main$setSearchEntry(entry)
        main$setSearchEqualFunc(f)
        sw$showAll()
      }
    },
    # actions
    onCellEditedVars    = function(renderer, path, new.text){
      txt           <- localize(new.text)
      row           <- as.numeric(path) + 1
      data.set.name <- data$getData.set.name()
      var.name      <- data$getVariableNames()[row]
      data.set      <- data$getData.set()
      names(data.set)[row] <- txt
      data$setData.set(data.set)
      data$constructVariable(row)
      data$linkDataFrame()
      .self$setCell(path, column.definition["vars"], txt)
      summaries[row] <<- data$getSummary(row)
    },
    onCellEditedVarLabs = function(renderer, path, new.text){
      txt           <- localize(new.text)
      row           <- as.numeric(path) + 1
      data.set.name <- data$getData.set.name()
      var.name      <- data$getVariableNames()[row]
      data.set      <- data$getData.set()
      description(data.set[[row]]) <- txt
      data$setData.set(data.set)      
      .self$setCell(path, column.definition["var.labs"], txt)
    },
    onCellEditedMsr     = function(renderer, path, new.text){
      txt           <- localize(new.text)
      row           <- as.numeric(path) + 1
      data.set.name <- data$getData.set.name()
      var.name      <- data$getVariableNames()[row]
      data.set      <- data$getData.set()
      measurement(data.set[[row]]) <- txt
      data$setData.set(data.set)
      data$constructVariable(row)
      data$linkDataFrame()
      .self$setCell(path, column.definition["msr"], txt)
      summaries[row] <<- data$getSummary(row)
    },
    onCellEditedMissing = function(renderer, path, new.text){
      txt           <- localize(new.text)
      row           <- as.numeric(path) + 1
      data.set.name <- data$getData.set.name()
      var.name      <- data$getVariableNames()[row]
      data.set      <- data$getData.set()
      txt <- as.numeric(unlist(strsplit(txt, ",")))
      if (any(is.na(txt))) return()
      else missing.values(data.set[[row]]) <- txt
      txt <- paste(as.character(sort(txt)), collapse=",")
      data$setData.set(data.set)
      data$constructVariable(row)
      data$linkDataFrame()
      .self$setCell(path, column.definition["missing"], txt)
      summaries[row] <<- data$getSummary(row)
    },
    onRecode            = function(action, win){
      onActivatedGTButton <- function(obj, list){
        var.name   <- list$var.name
        liststore2 <- list$liststore2
        textbuffer <- list$textbuffer
        result     <- liststore2$getIterFirst()
        data       <- NULL
        while(result$retval) {
          data <- c(data, lapply(liststore2$get(result$iter, 0, 1), localize))
          result$retval <- liststore2$iterNext(result$iter)
        }
        if(is.null(data)) return()
        data <- matrix(data, byrow=TRUE, ncol=2)
        text <- paste("\"", data[,2], "\" = ", data[,1], " <- ", data[,1], sep="", collapse=",\n")
        text <- sprintf("%s,\n%s", text, "otherwise = \"copy\"")
        textbuffer$setText(text)
      }
      onResponse <- function(dialog, response){
        if (response==GtkResponseType["ok"]) {
          iter <- textbuffer$getBounds()
          text <- localize(textbuffer$getText(iter$start, iter$end))
          text <- paste("         ", strsplit(text, "\n")[[1]],sep="", collapse="\n")
          new.var <- localize(entry3$getText())
          new.var.lab <- localize(entry.var.lab$getText())
          if( any(new.var==data$getVariableNames()) ){
            dialog2 <- gtkMessageDialogNew(dialog, "destroy-with-parent",
                                           GtkMessageType["question"],
                                           GtkButtonsType["ok-cancel"],
                                           gettext("This Variable already exists. Overwrite it?"))
            response2 <- dialog2$run()
            dialog2$destroy()
            if (response2!=GtkResponseType["ok"]){
            dialog$run()
            return()
            }
          }
          dialog$destroy()
          text <- sprintf("%s <-\n  recode(%s,\n%s)\n", new.var, var.name, text)
          data.set <- within(data.set, eval(parse(text=text)))
          description(data.set[[new.var]]) <- new.var.lab
          data$setData.set(data.set)

          var <- data.set[[new.var]]
          msr <- measurement(var)
          miss.val  <- missing.values(var)
          miss.val  <- ifelse(is.null(miss.val), "", paste(miss.val@filter, collapse=","))
          val.labs  <- labels(var)
          val.labs  <- ifelse(is.null(val.labs), "", paste(val.labs@values, "\"",val.labs@.Data, "\"", collapse=", "))
          row <- which(names(data.set)==new.var)
          cell.row <- as.character(row - 1)
          data$constructVariable(row)
          data$linkDataFrame()
          .self$setCell(cell.row, column.definition["index"], as.character(row))
          .self$setCell(cell.row, column.definition["vars"], new.var)
          .self$setCell(cell.row, column.definition["var.labs"], new.var.lab)
          .self$setCell(cell.row, column.definition["msr"], msr)
          .self$setCell(cell.row, column.definition["val.labs"], val.labs)
          .self$setCell(cell.row, column.definition["missing"] , miss.val)
          summary <- data$getSummary(row)
          summaries[row] <<- summary
        } else {
          dialog$destroy()
        }
      }
      data.set.name <- data$getData.set.name()
      selec      <- .self$getSelected()
      var.name   <- selec["vars"]
      var.lab    <- selec["var.labs"]
      data.set   <- data$getData.set()
      var        <- data.set[[var.name]]
      msr        <- measurement(var)
      labels     <- labels(var)
      liststore2 <- gtkListStoreNew("character", "character")
      if(!is.null(labels)){
        labels <- rbind(labels@values, labels@.Data)
        apply(labels, 2,
              function(x) {
               iter <- liststore2$append()$iter
               liststore2$set(iter, 0, x[1], 1, x[2])
              })
      }
      tw <- gtkTreeViewNewWithModel(liststore2)

      rt.val <- gtkCellRendererText()
      rt.lab <- gtkCellRendererText()
      
      col.val <- gtkTreeViewColumnNewWithAttributes(gettext("Values"), rt.val, "text"=0)
      col.lab <- gtkTreeViewColumnNewWithAttributes(gettext("Value Labels"), rt.lab, "text"=1)
      tw$appendColumn(col.val)
      tw$appendColumn(col.lab)
      
      vbox1  <- gtkVBoxNew()
      hbox1  <- gtkHBoxNew()
      vbox2  <- gtkVBoxNew()
      hpaned <- gtkHPanedNew()
      table1  <- gtkTableNew(rows=3, columns=2)
      hpaned$setPosition(160)
      sw1 <- gtkScrolledWindowNew()
      sw2 <- gtkScrolledWindowNew()
      sw1$setPolicy("automatic", "automatic")
      sw2$setPolicy("automatic", "automatic")
      label1 <- gtkLabelNew(gettext("Existing Values"))
      entry1 <- gtkEntryNew()
      entry1["editable"] <- FALSE
      entry1$setText(paste(sort(unique(na.omit(as.numeric(var)))), collapse=","))
      label2 <- gtkLabelNew(gettext("Source"))
      label3 <- gtkLabelNew(gettext("Destination"))
      label.var.lab <- gtkLabelNew(gettext("Variable Label"))
      label2$setAlignment(0,0.5)
      label3$setAlignment(0,0.5)
      label.var.lab$setAlignment(0,0.5)
      entry2 <- gtkEntryNew()
      entry2$setText(var.name)
      entry2["editable"] <- FALSE
#      entry2["sensitive"] <- FALSE
      entry3 <- gtkEntryNew()
      entry3$setText(var.name)
      entry.var.lab <- gtkEntryNew()
      entry.var.lab$setText(var.lab)
      entry2$setWidthChars(10)
      entry3$setWidthChars(10)
      entry.var.lab$setWidthChars(10)
      textview <- gtkTextViewNew()
      font.desc <- pangoFontDescriptionFromString("monospace")
      textview$modifyFont(font.desc)

      textbuffer <- textview$getBuffer()
      gtbutton   <- gtkButtonNewWithLabel(gettext("Generate Template"))
      
      vbox1$packStart(hbox1, expand=FALSE)
      vbox1$packStart(hpaned)
      
      hbox1$packStart(label1, expand=FALSE)
      hbox1$packStart(entry1)

      sw1$add(tw)
      hpaned$add1(sw1)
      hpaned$add2(vbox2)

      sw2$add(textview)
      vbox2$packStart(table1, expand=FALSE)
      vbox2$packStart(gtbutton, expand=FALSE, fill=FALSE)
      vbox2$packStart(sw2)
      
      table1$attach(label2, 0, 1, 0, 1, xoptions=GtkAttachOptions["fill"])
      table1$attach(entry2, 1, 2, 0, 1, xpadding = 1, ypadding = 1)
      table1$attach(label3, 0, 1, 1, 2, xoptions=GtkAttachOptions["fill"])
      table1$attach(entry3, 1, 2, 1, 2, xpadding = 1, ypadding = 1)
      table1$attach(label.var.lab, 0, 1, 2, 3, xoptions=GtkAttachOptions["fill"])
      table1$attach(entry.var.lab, 1, 2, 2, 3, xpadding = 1, ypadding = 1)
      
      gSignalConnect(gtbutton, "clicked", onActivatedGTButton, list(var.name=var.name, liststore2=liststore2, textbuffer=textbuffer))
      
      dialog <- gtkDialogNewWithButtons(title=gettext("Recode"), parent=win, flags=c("modal", "destroy-with-parent"),
                                         "gtk-ok", GtkResponseType["ok"], 
                                         "gtk-cancel", GtkResponseType["cancel"],
                                         show=FALSE)
      dialog$setDefaultSize(430, 300)
      dialog[["vbox"]]$packStart(vbox1)
      gSignalConnect(dialog, "response", onResponse)
      response <- dialog$run()
    },
    onRowActivated      = function(tw, path, column){
      row      <- as.numeric(path$toString())
      data.set <- data$getData.set()
      print(codebook(data.set[ row+1 ]))
    },  
    onQueryTooltip      = function(tw, x, y, keyboard_mode, tooltip){
      path <- tw$getPathAtPos(x, y - 20)$path
      if(is.null(path)) return(FALSE)
      row <- as.numeric(path$toString())
      char  <- summaries[ row+1 ]
      tooltip$setMarkup(paste("<span font_family=\"monospace\">", char, "</span>", sep="", collapse=""))
      tw$setTooltipRow(tooltip, path)
      return(TRUE)
    }
  )
)
variable.view$accessors("sw")

main <-
setRefClass("RzMain",
  fields = c("recode.id", "win", "data.collection", "data.handler", "action.group", "uimanager", "a.recode",
             "menu.bar", "tool.bar", "context.menu",  "main.view", "variable.view", "entry.search",
             "info.bar", "message.label", "status.bar", "progress.bar", "actions", "variable.view.list", "data.set.list", "data.set.list.combo"),
  methods = list(
    initialize            = function(...) {
      recode.id     <<- NULL
      win           <<- gtkWindowNew(show=FALSE)
      action.group  <<- gtkActionGroupNew()
      action.group$setTranslationDomain("pkg-RGtk2")
      uimanager     <<- gtkUIManagerNew()
      main.view     <<- gtkVBoxNew()
      data.handler  <<- gtkHBoxNew(spacing=1)
      data.set.list <<- gtkListStoreNew("character", "character", "character", "character")
      data.set.list.combo <<- gtkComboBoxNewWithModel(data.set.list)
      info.bar      <<- gtkInfoBarNew(show=FALSE)
      message.label <<- gtkLabelNew()
      info.bar$addButton("gtk-ok", GtkResponseType["ok"])
      info.bar$setMessageType(GtkMessageType["info"])
      status.bar    <<- gtkStatusbarNew()
      progress.bar  <<- gtkProgressBarNew()
#      spinner <<- gtkSpinnerNew()
      entry.search  <<- gtkEntryNew()
      entry.search$setWidthChars(20)
      entry.search$setIconFromStock(GtkEntryIconPosition["primary"], "gtk-find")
      entry.search$setTooltipText(gettext("Prev: Up Arrow Key\nNext: Down Arrow Key"))
      variable.view <<- NULL
      variable.view.list <<- list()
      
      info.bar$setNoShowAll(TRUE)
      win["title"] <<- "Rz"
      win$setDefaultSize(650, 600)
      progress.bar["width-request"] <<- 150

      gSignalConnect(data.set.list.combo, "changed", .self$onDatasetChanged)
      gSignalConnect(info.bar, "response", .self$onInfoBarResponsed)

      initFields(...)
    },
    constructActionGroup  = function(){
      a.file    <- gtkActionNew("MenuFile", gettext("File"))
      a.open    <- gtkActionNew("Open", gettext("Open"), stock.id=GTK_STOCK_OPEN)
      a.save    <- gtkActionNew("Save As",gettext("Save as"),  stock.id=GTK_STOCK_SAVE_AS)
      a.import  <- gtkActionNew("Import",gettext("Import"), stock.id=GTK_STOCK_CONVERT)
      a.ds      <- gtkActionNew("ImportFromGlobalEnv",gettext("Import from _Grobal Environment"), stock.id=GTK_STOCK_ADD)
      a.quit    <- gtkActionNew("Quit", stock.id=GTK_STOCK_QUIT)
      a.edit    <- gtkActionNew("MenuEdit", gettext("Edit"))
      a.ch.name <- gtkActionNew("ChageDataSetName", gettext("Change the Name of Current Dataset"), stock.id=GTK_STOCK_BOLD)
      a.remove  <- gtkActionNew("RemoveDataSet", gettext("Remove Current Dataset"), stock.id=GTK_STOCK_DELETE)
      a.vlabs   <- gtkActionNew("ValueLabels", gettext("Value Labels"))
      a.missing <- gtkActionNew("Missing", gettext("Missing Values"))
      a.recode  <<- gtkActionNew("Recode", gettext("Recode"))

      action.group$addAction(a.open)
      action.group$addAction(a.save)
      action.group$addAction(a.file)
      action.group$addAction(a.import)
      action.group$addAction(a.ds)
      action.group$addAction(a.quit)

      action.group$addAction(a.edit)
      action.group$addAction(a.ch.name)
      action.group$addAction(a.remove)
    #  action.group$addAction(a.vlabs)
    #  action.group$addAction(a.missing)

      action.group$addAction(a.recode)
      
      gSignalConnect(a.open,    "activate", .self$onOpen)
      gSignalConnect(a.save,    "activate", .self$onSave)
      gSignalConnect(a.import,  "activate", .self$onImport)
      gSignalConnect(a.ds,      "activate", .self$onImportFromGlobalEnv)
      gSignalConnect(a.ch.name, "activate", .self$onChangeDataSetName)
      gSignalConnect(a.remove, "activate", .self$onRemove)
      gSignalConnect(a.quit,    "activate", win$destroy)
   },
    constructMenu         = function(){
      popupmenu <- function(obj, event, popup) {
        button <- event$GetButton()
        if (button==3) popup$popup(button=button,activate.time=event$getTime())
        return(TRUE)
      }
      .Rz.path <- system.file(package = "Rz")
#      .Rz.path <- "/home/masahiro/Documents/R/Rz/Rz/inst"  # for debug
      uimanager$insertActionGroup(action.group, 0)
      uimanager$addUiFromFile(file.path(.Rz.path, "ui", "menu.ui"))
      menu.bar     <<- uimanager$getWidget("/MenuBar")
      tool.bar     <<- uimanager$getWidget("/ToolBar")
      context.menu <<- uimanager$getWidget("/PopupVV") 
      toolitem1 <- tool.bar$getNthItem(0)
      toolitem2 <- tool.bar$getNthItem(1)
      toolitem3 <- tool.bar$getNthItem(3)
      toolitem4 <- tool.bar$getNthItem(4)
      toolitem5 <- tool.bar$getNthItem(6)
      toolitem6 <- tool.bar$getNthItem(7)
#      toolitem.spin <- gtkToolItemNew()
#      toolitem.spin$add(spinner)
#      tool.bar$insert(toolitem.spin, 0)
      toolitem1$setTooltipText(gettext("Open"))
      toolitem2$setTooltipText(gettext("Save as"))
      toolitem3$setTooltipText(gettext("Import"))
      toolitem4$setTooltipText(gettext("Import from Grobal Environment"))
      toolitem5$setTooltipText(gettext("Change the Name of Current Dataset"))
      toolitem6$setTooltipText(gettext("Remove Current Dataset"))
      tool.bar["toolbar-style"] <<- GtkToolbarStyle["icons"]
      tool.bar$setIconSize(GtkIconSize["small-toolbar"])
      gSignalConnect(main.view, "button-release-event" , popupmenu, context.menu)
    },
    constructDataHandler  = function(){
      dsnames        <- data.collection$getDataSetNames()
      original.names <- data.collection$getOriginalNames()
      for( i in seq_len(data.collection$getLength()) ){
        iter <- data.set.list$append()$iter
        data.set.list$set(iter, 0, dsnames[i], 1, sprintf("(%s)", original.names[i]), 2, "", 3, "")
      }
      renderer1 <- gtkCellRendererText()
      renderer2 <- gtkCellRendererText()
      data.set.list.combo$setFocusOnClick(FALSE)
      data.set.list.combo$packStart(renderer1)
      data.set.list.combo$packStart(renderer2, expand=FALSE)
      data.set.list.combo$addAttribute(renderer1, "text", 0)
      data.set.list.combo$addAttribute(renderer2, "text", 1)
      data.handler$packStart(data.set.list.combo, expand=TRUE, fill=TRUE)
      data.handler$packStart(entry.search, expand=FALSE)
    },
    constructWindow       = function(){
      info.bar$getContentArea()$packStart(message.label, expand=FALSE, fill=FALSE)
      status.bar$packEnd(progress.bar, expand=FALSE)
#      status.bar$packEnd(spinner, expand=FALSE)
      vbox       <- gtkVBoxNew()
      vbox$packStart(menu.bar,     expand=FALSE, fill=FALSE)
      vbox$packStart(tool.bar,     expand=FALSE, fill=FALSE)
      vbox$packStart(info.bar,     expand=FALSE, fill=FALSE)
      vbox$packStart(data.handler, expand=FALSE, fill=FALSE)
      vbox$packStart(main.view,    expand=TRUE , fill=TRUE )
      vbox$packEnd  (status.bar,   expand=FALSE, fill=FALSE)
      tool.bar$showAll()
      win$add(vbox)
      win$showAll()
    },
    addData               = function(data){
        data.collection$addData(data)
        iter <- data.set.list$append()$iter
        data.set.list$set(iter, 0, data$getData.set.name(), 1, sprintf("( %s )", data$getOriginal.name()), 2, "", 3, "")
        len <- data.set.list$iterNChildren()
        data.set.list.combo$setActive( len-1 )
    },
    getCurrentDataSetName = function(){localize(data.set.list.combo$getActiveText()) },
    getCurrentData        = function(){
      data.set.name <- .self$getCurrentDataSetName()
      if(length(data.set.name)==0) return(NULL)
      data <- data.collection$getData(data.set.name)
      return(data)
    },
    # actions
    onOpen                = function(action){
      dialog <- gtkFileChooserDialogNew(title=gettext("Open Data"), parent=win,
                                        action=GtkFileChooserAction["open"],
                                        "gtk-save", GtkResponseType["accept"],
                                        "gtk-cancel", GtkResponseType["cancel"], 
                                        show=FALSE)
      filter <- gtkFileFilterNew()
      filter$setName(gettext("RzData file (*.rzd)"))
      filter$addPattern("*.rzd")
      dialog$addFilter(filter)
      if (dialog$run() == GtkResponseType["accept"]) {
        filename <- localize(dialog$getFilename())
        dialog$destroy()
        if (length(file)==0) return()
        load(file=filename)
        base <- basename(filename)
        data$setFile.path(filename)
        data$setOriginal.name(base)        
        data.collection$addData(data)
        iter <- data.set.list$append()$iter
        data.set.list$set(iter, 0, data$getData.set.name(), 1, sprintf("( %s )", data$getOriginal.name()), 2, "", 3, "")
        len <- data.set.list$iterNChildren()
        data.set.list.combo$setActive( len-1 )        
      } else {
        dialog$destroy()
      }
    },
    onSave                = function(action){
      data <- .self$getCurrentData()
      if(is.null(data)) return()
      dialog <- gtkFileChooserDialogNew(title=gettext("Save Data"), parent=win,
                                        action=GtkFileChooserAction["save"],
                                        "gtk-save", GtkResponseType["accept"],
                                        "gtk-cancel", GtkResponseType["cancel"], 
                                        show=FALSE)
      filter <- gtkFileFilterNew()
      filter$setName(gettext("RzData file (*.rzd)"))
      filter$addPattern("*.rzd")
      dialog$addFilter(filter)
      if (dialog$run() == GtkResponseType["accept"]) {
        filename <- localize(dialog$getFilename())
        if (file.exists(filename)){
          dialog2 <- gtkMessageDialogNew(dialog, "destroy-with-parent",
                                         GtkMessageType["question"],
                                         GtkButtonsType["ok-cancel"],
                                         gettext("This file already exists. Overwrite it?"))
          response <- dialog2$run()
          dialog2$destroy()
          if (response!=GtkResponseType["ok"]){
            dialog$destroy()
            return()
          }
        }
        filename <- ifelse(grepl("*.rzd", filename), filename, sprintf("%s.rzd", filename))
        save(data, file=filename)
        dialog$destroy()
      } else {
        dialog$destroy()
      }
    },
    onImport              = function(action){
      label <- gtkLabel(gettext("Encoding"))
      combo <- gtkComboBoxNewText()
      for(i in iconvlist()) combo$appendText(i)
      index <- which(localeToCharset()[1]==iconvlist()) - 1
      if(length(index)==0) index <- -1
      combo$setActive(index)
      # File Filter
      file.type.list <- list(spss    = list(name=gettext("SPSS system file (*.sav)"), pattern="*.sav"),
                             spss.por= list(name=gettext("SPSS portable file (*.por)"), pattern="*.por"),
                             stata   = list(name=gettext("Stata file (*.dta)"), pattern="*.dta")
                            )
      # File Import Dialog
      dialog <- gtkFileChooserDialogFilteredNew(title=gettext("Import"), parent=win, file.type.list=file.type.list)
      hbox <- dialog$getContentArea()$getChildren()[[1]]$getChildren()[[1]]$getChildren()[[1]]$getChildren()[[3]]$getChildren()[[2]]$getChildren()[[2]]
      hbox$packEnd(combo, expand=FALSE)
      hbox$packEnd(label, expand=FALSE)
      file     <- dialog$activate()
      encoding <- localize(combo$getActiveText())
      dialog$destroy()
      if (is.null(file)){
        return()
      } else {
        timeoutid <- gTimeoutAdd(80, progress.bar$start)
#        spinner$start()

        dir  <- dirname(file$filename)
        base <- basename(file$filename)
        if (grepl("sav", file$filetype)) {
          importer <- spss.system.file(file$filename)
        } else if (grepl("por", file$filetype)) {
          importer <- spss.portable.file(file$filename)
        } else {
          importer <- Stata.file(file$filename)
        }
        data.set   <- NULL
        c.encoding <- localeToCharset()[1]
        if ( encoding != c.encoding ) {
          message.label$setText(gettext("Encoding may takes several or more minutes. Please wait..."))
          info.bar$show()
          data.set   <- as.data.set(importer)
          names(data.set) <- iconv(names(data.set), from=encoding, to=c.encoding)
          var.labs   <- iconv(description(data.set), from=encoding, to=c.encoding)
          var.labs   <- as.list(var.labs)
          var.labs   <- lapply(var.labs, function(x) gsub("(^')|('$)", "", x))
          labels     <- lapply(data.set, labels)
          labels     <- lapply(labels, function(x){
            x <- iconv(as.character(x), from=encoding, to=c.encoding)
            if(length(x) == 0) return(NULL)
            else return(x)
          })
          for(i in seq_along(var.labs)){
            description(data.set[[i]]) <- var.labs[[i]]
            if( !is.null(labels[[i]]))
              labels(data.set[[i]])@.Data <- labels[[i]]
          }
        } else {
          data.set <- as.data.set(importer)        
        }

        data <- new("RzData",
                    file.path=file$filename, original.name=base,
                    data.set=data.set)
        data$construct()
        gSourceRemove(timeoutid)
        progress.bar["activity-mode"] <<- FALSE
#        spinner$stop()

        .self$addData(data)
      }
    },
    onImportFromGlobalEnv = function(action){
      on.exit(gSourceRemove(timeoutid))
      on.exit(progress.bar["activity-mode"] <<- FALSE, add=TRUE)
#      on.exit(spinner$stop())
      combo <- gtkComboBoxNewText()
      dfnames <- unlist(eapply(.GlobalEnv, is.data.frame))
      dfnames <- names(dfnames)[dfnames]
      dsnames <- unlist(eapply(.GlobalEnv, is.data.set))
      dsnames <- names(dsnames)[dsnames]
      importlist <- c(dfnames, dsnames)
      sapply(importlist, combo$appendText)
      dialog <- gtkDialogNewWithButtons(title=gettext("Import from Grobal Environment"), parent=win, flags=c("modal", "destroy-with-parent"),
                                         "gtk-ok", GtkResponseType["ok"], 
                                         "gtk-cancel", GtkResponseType["cancel"],
                                         show=FALSE)
      dialog[["vbox"]]$packStart(combo, expand=FALSE)
      dialog$showAll()
      response <- dialog$run()
      import <- localize(combo$getActiveText())
      dialog$destroy()
      timeoutid <- gTimeoutAdd(80, progress.bar$start)
#      spinner$start()
      if (response==GtkResponseType["ok"] & length(import)!=0) {
        ds <- get(import, envir=.GlobalEnv)
        if (is.data.frame(ds)) {
          ds2 <- data.set(ds)
          names(ds2) <- colnames(ds)
          ds <- ds2
        }
        data <- new("RzData", file.path=NULL, data.set=ds,
                    original.name=sprintf("%s [from Global Environment]", import))
        data$construct()
        .self$addData(data)
      }
    },
    onChangeDataSetName   = function(action){
      data.set.name <- .self$getCurrentDataSetName()
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
      response <- dialog$run()
      if (response==GtkResponseType["ok"]) {
        new.name <- localize(entry1$getText())
        if ( length(new.name) == 0 ) return()
        data <- data.collection$getData(data.set.name)
        data$setData.set.name(new.name)
        index <- data.set.list.combo$getActive()
        path  <- gtkTreePathNewFromString(index)
        iter  <- data.set.list$getIter(path)$iter
        data.set.list$set(iter, 0, new.name)
        rm(list=data.set.name, envir=.GlobalEnv)
        data$linkDataFrame()
        match <- which(names(variable.view.list)==data.set.name)
        names(variable.view.list)[match] <<- new.name
        dialog$destroy()
      } else {
        dialog$destroy()
      }
    },
    onRemove              = function(action){
      data.set.name <- .self$getCurrentDataSetName()
      if ( length(data.set.name) == 0 ) return()
      data.collection$removeData(data.set.name)
      rm(list=data.set.name, envir=.GlobalEnv)
      iter <- data.set.list.combo$getActiveIter()$iter
      data.set.list$remove(iter)
      variable.view$toggleView()
      variable.view$getSw()$destroy()
      variable.view <<- NULL
      variable.view.list[data.set.name] <<- NULL
    },
    onDatasetChanged      = function(combo){
      searchFunc <- function(model, column, key, iter) {
        key <- localize(key)
        value <- model$get(iter, 1, 2)
        return(!any(
          grepl(key, c(localize(value[[1]][1]), localize(value[[2]][1])), perl=TRUE)
        ))
      }
      data.set.name <- .self$getCurrentDataSetName()
      if ( length(data.set.name) == 0 ) return()
      data <- data.collection$getData(data.set.name)
      if ( !is.null(variable.view) ){
        variable.view$toggleView()
      }
      variable.view <<- variable.view.list[[data.set.name]]
      if ( is.null(variable.view) ) {
        timeoutid <- gTimeoutAdd(80, progress.bar$start)
#        spinner$start()
        variable.view <<- new("RzVariableView", data=data)
        variable.view$construct()
        variable.view.list[[data.set.name]] <<- variable.view
        main.view$packStart(variable.view$getSw())
        gSourceRemove(timeoutid)
        progress.bar["activity-mode"] <<- FALSE
#        spinner$stop()
      }
      data$linkDataFrame()
      variable.view$toggleView(entry.search, searchFunc)
      
      win["title"] <<- paste("Rz -", data.set.name)
      if (!is.null(recode.id)) gSignalHandlerDisconnect(a.recode, recode.id)
      recode.id <<- gSignalConnect(a.recode,  "activate", variable.view$onRecode, win)
    },
    onInfoBarResponsed    = function(widget, response.id){
      info.bar$hide()
    }
  )
)
