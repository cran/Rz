variable.view <- 
setRefClass("RzVariableView",
  fields = c("data","main", "liststore", "sw", "column.definition", "summaries",
             "rt.index", "rt.vars", "rt.var.labs", "rt.val.labs", "rc.msr", "rt.missing",
             "rzPlot"),
  methods = list(
    initialize  = function(...) {
      initFields(...)
      
      column.definition <<- c(index=0, vars=1, var.labs=2, msr=3, val.labs=4, missing=5)
      liststore <<- gtkListStoreNew("character", "character", "character", "character", "character", "character")
      main <<- gtkTreeViewNewWithModel(liststore)
      
      main$modifyFont(pangoFontDescriptionFromString(rzSettings$getVariableViewFont()))
      
      sw   <<- gtkScrolledWindowNew()
      sw$add(main)
      sw$setPolicy(GtkPolicyType["automatic"], GtkPolicyType["automatic"])
      main["enable-grid-lines"] <<- GtkTreeViewGridLines["both"]
      main["rules-hint"] <<- TRUE
      main["has-tooltip"] <<- TRUE
      rt.index    <<- gtkCellRendererText()
      rt.vars     <<- gtkCellRendererText()
      rt.var.labs <<- gtkCellRendererText()
      rt.val.labs <<- gtkCellRendererText()
      rt.missing  <<- gtkCellRendererText()
      color       <- rt.index["cell-background-gdk"]
      color$red   <- 45000L
      color$green <- 45000L
      color$blue  <- 45000L
      rt.index["cell-background-gdk"] <<- color
      rt.index["xalign"] <<- 0.5
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
      
      rzPlot$setModel(main$getModel())
      
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
      columns$index$setMinWidth(30)
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
    
    reload = function(){
      vars      <-  data$getVariableNames()
      var.labs  <-  data$getVariableLabels()
      msr       <-  data$getMeasurement()
      val.labs  <-  data$getValueLabels()
      miss.val  <-  data$getMissingValues()
      summaries <<- data$getSummaries()
      liststore$clear()
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
    toggleView  = function(rzSearchEntry=NULL){
      .self$changeFont()
      if (is.null(rzSearchEntry)){
        main$setSearchEntry(NULL)
        sw$hideAll()
        rzPlot$setModel(NULL)
      } else {
        main$setSearchEntry(rzSearchEntry$getEntry.search())
        main$setSearchEqualFunc(rzSearchEntry$searchFunc)
        sw$showAll()
        rzPlot$setModel(main$getModel())
      }
    },
    
    # actions
    onCellEditedVars    = function(renderer, path, new.text){
      txt     <- localize(new.text)
      txt     <- sub("^([[:space:]]+)([^[:space:]]+)([[:space:]]+)$", "\\2", txt)
      invalid <- grepl("(^$)|(^[0-9]+)|([]\\[\\^$*?|(){}@!\"#$%&'*+,/:;<=>?~[:space:]-])",
                       txt)
      if(invalid) return()
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
      txt           <- sub("^([[:space:]]+)([^[:space:]]+)([[:space:]]+)$", "\\2", txt)
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
      txt           <- sub("^([[:space:]]+)([^[:space:]]+)([[:space:]]+)$", "\\2", txt)
      row           <- as.numeric(path) + 1
      data.set.name <- data$getData.set.name()
      var.name      <- data$getVariableNames()[row]
      data.set      <- data$getData.set()
      result <- try(eval(parse(text=sprintf("c(%s)", txt))), silent=TRUE)
      if (!is.numeric(result)) return()
      else {
        if (length(result)==2&&grepl("range", txt)){
          missing.values(data.set[[row]]) <- eval(parse(text=sprintf("list(%s)", txt))) 
        } else{
          missing.values(data.set[[row]]) <- result
        }
      }
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
        data.tmp   <- NULL
        while(result$retval) {
          data.tmp <- c(data.tmp, lapply(liststore2$get(result$iter, 0, 1), localize))
          result$retval <- liststore2$iterNext(result$iter)
        }
        if(is.null(data.tmp)) return()
        data.tmp <- matrix(data.tmp, byrow=TRUE, ncol=2)
        text <- paste("\"", data.tmp[,2], "\" = ", data.tmp[,1], " <- ", data.tmp[,1], sep="", collapse=",\n")
        text <- sprintf("%s,\n%s", text, "otherwise = \"copy\"")
        textbuffer$setText(text)
      }
      onResponse <- function(dialog, response){
        if (response==GtkResponseType["ok"]) {
          iter <- textbuffer$getBounds()
          text <- localize(textbuffer$getText(iter$start, iter$end))
          if(!nzchar(text)){
            dialog2 <- gtkMessageDialogNew(dialog, "destroy-with-parent",
                                           GtkMessageType["error"],
                                           GtkButtonsType["close"],
                                           gettext("This recode syntax is invalid. Please enter a valid recode syntax."))
            dialog2$run()
            dialog2$hide()
            dialog$run()
            return()
          }
          text <- paste("         ", strsplit(text, "\n")[[1]],sep="", collapse="\n")
          new.var <- localize(entry3$getText())
          new.var.lab <- localize(entry.var.lab$getText())
          new.var <- sub("^([[:space:]]+)([^[:space:]]+)([[:space:]]+)$",
                         "\\2", new.var)
          invalid <- grepl("(^$)|(^[0-9]+)|([]\\[\\^$*?|(){}@!\"#$%&'*+,/:;<=>?~[:space:]-])",
                           new.var)
          if (invalid || length(new.var) == 0 ){
            dialog2 <- gtkMessageDialogNew(dialog, "destroy-with-parent",
                                           GtkMessageType["error"],
                                           GtkButtonsType["close"],
                                           gettext("This variable name is invalid. Please enter a valid variable name."))
            dialog2$run()
            dialog2$hide()
            dialog$run()
            return()
          }
          if( any(new.var==data$getVariableNames()) ){
            dialog2 <- gtkMessageDialogNew(dialog, "destroy-with-parent",
                                           GtkMessageType["question"],
                                           GtkButtonsType["ok-cancel"],
                                           gettext("This Variable already exists. Overwrite it?"))
            response2 <- dialog2$run()
            dialog2$hide()
            if (response2!=GtkResponseType["ok"]){
              dialog$run()
              return()
            }
          }
          text <- sprintf("%s <-\n  recode(%s,\n%s)\n", new.var, var.name, text)
          data.set <- try(within(data.set, eval(parse(text=text))), silent=TRUE)
          if(!is.data.set(data.set)){
            dialog2 <- gtkMessageDialogNew(dialog, "destroy-with-parent",
                                           GtkMessageType["error"],
                                           GtkButtonsType["close"],
                                           gettext("This recode syntax is invalid. Please enter a valid recode syntax."))
            dialog2$run()
            dialog2$hide()
            dialog$run()
            return()
          }
          dialog$hide()
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
          dialog$hide()
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
      textview$modifyFont(pangoFontDescriptionFromString(rzSettings$getMonospaceFont()))
      
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
      if(rzSettings$getPlotViewEnabled()) {
        rzPlot$onPlot(data, row + 1)
      }
      if(!(rzSettings$getPlotViewEnabled()&rzSettings$getCodebookOff())) {
        print(codebook(data.set[ row+1 ]))
      }
    },  
    
    onQueryTooltip      = function(tw, x, y, keyboard_mode, tooltip){
      if(rzSettings$getPopupOff()) return(FALSE)
      path <- tw$getPathAtPos(x, y - 20)$path
      if(is.null(path)) return(FALSE)
      row <- as.numeric(path$toString())
      char  <- summaries[ row+1 ]
      tooltip$setMarkup(paste("<span font_family=\"", rzSettings$getMonospaceFontFamily(), "\">", char, "</span>", sep="", collapse=""))
      tw$setTooltipRow(tooltip, path)
      return(TRUE)
    },
    
    changeFont = function(){
      main$modifyFont(pangoFontDescriptionFromString(rzSettings$getVariableViewFont()))
    }
  )
)
variable.view$accessors("sw")
