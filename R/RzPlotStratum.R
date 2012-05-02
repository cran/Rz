rzplot.stratum <- 
setRefClass("RzPlotStratum",
  fields = c("combo.group", "combo.group.label",
             "combo.fill", "combo.fill.label",
             "combo.color", "combo.color.label",
             "combo.shape", "combo.shape.label",
             "combo.size","combo.size.label",
             "combo.line","combo.line.label",
             "combo.position", "position.entry",
             "combo.linetype", "combo.scale",
             "expander"),
  methods = list(
    initialize  = function(...) {
      initFields(...)
      brewer    <- rownames(RColorBrewer::brewer.pal.info)
      treestore <- gtkTreeStoreNew("character", "character", "GdkPixbuf")

      pixbuf.path <- file.path(rzSettings$getRzPath(), "images", "palette",
                               "scale_hue.png")
      pixbuf <- gdkPixbufNewFromFile(pixbuf.path)$retval
      iter   <- treestore$append()$iter
      treestore$set(iter, 0, "hue", 2, pixbuf)
      
      pixbuf.path <- file.path(rzSettings$getRzPath(), "images", "palette",
                               "scale_grey.png")
      pixbuf <- gdkPixbufNewFromFile(pixbuf.path)$retval
      iter   <- treestore$append()$iter      
      treestore$set(iter, 0, "grey", 2, pixbuf)

      pixbuf.path <- file.path(rzSettings$getRzPath(), "images", "palette",
                               "scale_brewer.png")
      pixbuf <- gdkPixbufNewFromFile(pixbuf.path)$retval
      iter   <- treestore$append()$iter
      treestore$set(iter, 0, "brewer", 2, pixbuf)
      
      for (i in seq_along(brewer)) {
        iter.ch <- treestore$append(parent=iter)$iter
        pixbuf.path <- file.path(rzSettings$getRzPath(), "images", "palette",
                                 sprintf("%s.png", brewer[i]))
        pixbuf      <- gdkPixbufNewFromFile(pixbuf.path)$retval
        treestore$set(iter.ch, 1, brewer[i], 2, pixbuf)
      }
      
      renderer1 <- gtkCellRendererText()
      renderer2 <- gtkCellRendererText()
      renderer3 <- gtkCellRendererPixbuf()
      renderer3["width"]  <- 120
      renderer1["xalign"] <- 0.5
      color       <- renderer1["cell-background-gdk"]
      color$red   <- 65535L
      color$green <- 65535L
      color$blue  <- 65535L
      renderer1["cell-background-gdk"] <- color
      renderer2["cell-background-gdk"] <- color
      renderer3["cell-background-gdk"] <- color
      
      label.group <-  gtkLabelNew("group")
      combo.group <<- new("RzCompletionCombo")
      label.fill  <-  gtkLabelNew("fill")
      combo.fill  <<- new("RzCompletionCombo")
      label.color <-  gtkLabelNew("color")
      combo.color <<- new("RzCompletionCombo")
      label.shape <-  gtkLabelNew("shape")
      combo.shape <<- new("RzCompletionCombo")
      label.size  <-  gtkLabelNew("size")
      combo.size  <<- new("RzCompletionCombo")
      label.line  <-  gtkLabelNew("linetype")
      combo.line  <<- new("RzCompletionCombo")
      
      # group
      combo.group.label <<- gtkComboBoxEntryNewText()
      combo.group.label["width-request"] <<- 1
      combo.group.label$show()
      labels <- c(gettext("variable label"), gettext("variable name"), gettext("(free text)"))
      for(i in labels) combo.group.label$appendText(i)
      combo.group.label$setActive(0)

      # fill
      combo.fill.label <<- gtkComboBoxEntryNewText()
      combo.fill.label["width-request"] <<- 1
      combo.fill.label$show()
      for(i in labels) combo.fill.label$appendText(i)
      combo.fill.label$setActive(0)
      
      # color
      combo.color.label <<- gtkComboBoxEntryNewText()
      combo.color.label["width-request"] <<- 1
      combo.color.label$show()
      for(i in labels) combo.color.label$appendText(i)
      combo.color.label$setActive(0)

      # shape
      combo.shape.label <<- gtkComboBoxEntryNewText()
      combo.shape.label["width-request"] <<- 1
      combo.shape.label$show()
      for(i in labels) combo.shape.label$appendText(i)
      combo.shape.label$setActive(0)

      # size
      combo.size.label <<- gtkComboBoxEntryNewText()
      combo.size.label["width-request"] <<- 1
      combo.size.label$show()
      for(i in labels) combo.size.label$appendText(i)
      combo.size.label$setActive(0)

      # linetype
      combo.line.label <<- gtkComboBoxEntryNewText()
      combo.line.label["width-request"] <<- 1
      combo.line.label$show()
      for(i in labels) combo.line.label$appendText(i)
      combo.line.label$setActive(0)

      # scale
      label.scale <- gtkLabelNew("scale")
      combo.scale <<- gtkComboBoxNew(show=TRUE)
      combo.scale$setModel(treestore)
      combo.scale$clear()
      combo.scale$packStart(renderer1, expand=TRUE)
      combo.scale$packStart(renderer2, expand=TRUE)
      combo.scale$packStart(renderer3, expand=FALSE)
      combo.scale$addAttribute(renderer1, "text"  , 0)
      combo.scale$addAttribute(renderer2, "text"  , 1)
      combo.scale$addAttribute(renderer3, "pixbuf", 2)
      combo.scale$setActive(0)
      
      # position
      label.position <- gtkLabelNew(gettext("legend position"))
      combo.position <<- gtkComboBoxNewText()
      combo.position$show()
      labels <- c("top", "right", "bottom", "left", "specify", "none")
      for(i in labels) combo.position$appendText(i)
      combo.position$setActive(1)
      position.entry <<- gtkEntryNew()
      position.entry$setText("0,1")
      locator.button <- gtkButtonNew()
      mousepix       <- gdkPixbufNewFromFile(file.path(rzSettings$getRzPath(), "images/mouse.png"))$retval
      image          <- gtkImageNewFromPixbuf(mousepix)
      locator.button$setImage(image)
      position.hbox <- gtkHBoxNew()
      position.hbox$packStart(position.entry)
      position.hbox$packStart(locator.button, expand=FALSE)
      position.hbox["sensitive"] <- FALSE
      label.linetype <- gtkLabelNew(gettext("line type"))
      combo.linetype <<- gtkComboBoxNewText()
      linetypes <- c("blank", "solid", "dashed", "dotted", "dotdash", "longdash", "twodash")
      for(i in linetypes) combo.linetype$appendText(i)
      combo.linetype$setActive(0)
      
      table  <- gtkTableNew(10, 3, FALSE)
      table["border-width"] <- 5
      table$attach        (label.group           , 0, 1, 0,  1, "shrink", "shrink", 0, 0)
      table$attach        (combo.group$getCombo(), 1, 2, 0,  1, "shrink", "shrink", 0, 0)
      table$attachDefaults(combo.group.label     , 2, 3, 0,  1)
      table$attach        (label.fill            , 0, 1, 1,  2, "shrink", "shrink", 0, 0)
      table$attach        (combo.fill$getCombo() , 1, 2, 1,  2, "shrink", "shrink", 0, 0)
      table$attachDefaults(combo.fill.label      , 2, 3, 1,  2)
      table$attach        (label.color           , 0, 1, 2,  3, "shrink", "shrink", 0, 0)
      table$attach        (combo.color$getCombo(), 1, 2, 2,  3, "shrink", "shrink", 0, 0)
      table$attachDefaults(combo.color.label     , 2, 3, 2,  3)
      table$attach        (label.shape           , 0, 1, 3,  4, "shrink", "shrink", 0, 0)
      table$attach        (combo.shape$getCombo(), 1, 2, 3,  4, "shrink", "shrink", 0, 0)
      table$attachDefaults(combo.shape.label     , 2, 3, 3,  4)
      table$attach        (label.size            , 0, 1, 4,  5, "shrink", "shrink", 0, 0)
      table$attach        (combo.size$getCombo() , 1, 2, 4,  5, "shrink", "shrink", 0, 0)
      table$attachDefaults(combo.size.label      , 2, 3, 4,  5)
      table$attach        (label.line            , 0, 1, 5,  6, "shrink", "shrink", 0, 0)
      table$attach        (combo.line$getCombo() , 1, 2, 5,  6, "shrink", "shrink", 0, 0)
      table$attachDefaults(combo.line.label      , 2, 3, 5,  6)
      table$attach        (label.scale           , 0, 1, 6,  7, "shrink", "shrink", 0, 0)
      table$attachDefaults(combo.scale           , 1, 3, 6,  7)
      table$attach        (label.position        , 0, 1, 7,  8, "shrink", "shrink", 0, 0)
      table$attachDefaults(combo.position        , 1, 3, 7,  8)
      table$attachDefaults(position.hbox         , 0, 3, 8,  9)
      table$attach        (label.linetype        , 0, 1, 9, 10, "shrink", "shrink", 0, 0)
      table$attachDefaults(combo.linetype        , 1, 3, 9, 10)
      table$setColSpacings(5)
      table$setRowSpacings(2)

      expander <<- gtkExpanderNew(gettext("stratum options"))
      expander["border-width"] <<- 3
      expander$setExpanded(FALSE)
      expander$add(table)

      gSignalConnect(locator.button, "clicked", function(button){
        locate <- grid.locator(unit="npc")
        position.entry$setText(sprintf("%s,%s", as.numeric(locate$x), as.numeric(locate$y)))
      })
      
      gSignalConnect(combo.position, "changed", function(combo){
        position <- localize(combo$getActiveText())
        if(position=="specify") position.hbox["sensitive"] <- TRUE
        else                    position.hbox["sensitive"] <- FALSE
      })

    },
    
    clear = function(){
      combo.group$clear()
      combo.group.label$setActive(0)
      combo.fill$clear()
      combo.fill.label$setActive(0)
      combo.color$clear()
      combo.color.label$setActive(0)
      combo.shape$clear()
      combo.shape.label$setActive(0)
      combo.size$clear()
      combo.size.label$setActive(0)
      combo.line$clear()
      combo.line.label$setActive(0)
      combo.scale$setActive(0)
      combo.position$setActive(1)
      position.entry$setText("0,1")
      combo.linetype$setActive(0)
    },
    
    completionSetModel = function(model){
      combo.group$setModel(model)
      combo.fill$setModel(model)
      combo.color$setModel(model)
      combo.shape$setModel(model)
      combo.size$setModel(model)
      combo.line$setModel(model)
    },
    
    getArgs = function(){
      iter  <- combo.scale$getActiveIter()$iter
      model <- combo.scale$getModel()
      scale <- localize(unlist(model$get(iter, 0, 1)))
      if(scale=="brewer") scale <- "BrBG"
      group <- localize(combo.group$getActiveText())
      fill  <- localize(combo.fill$getActiveText())
      color <- localize(combo.color$getActiveText())
      shape <- localize(combo.shape$getActiveText())
      size  <- localize(combo.size$getActiveText())
      line  <- localize(combo.line$getActiveText())
      group.label     <- localize(combo.group.label$getActiveText())
      fill.label      <- localize(combo.fill.label$getActiveText())
      color.label     <- localize(combo.color.label$getActiveText())
      shape.label     <- localize(combo.shape.label$getActiveText())
      size.label      <- localize(combo.size.label$getActiveText())
      line.label      <- localize(combo.line.label$getActiveText())
      legend.position <- localize(combo.position$getActiveText())
      legend.linetype <- localize(combo.linetype$getActiveText())      
      if(legend.position=="specify"){
        vec <- localize(position.entry$getText())
        vec <- strsplit(vec, ",")[[1]]
        vec <- suppressWarnings(as.numeric(vec))
        if(any(is.na(vec)) || length(vec) != 2){
          legend.position <- "right"
        } else {
          legend.position <- vec
        }
      }
      if(!nzchar(group)) group <- NULL
      if(!nzchar(fill))  fill  <- NULL
      if(!nzchar(color)) color <- NULL
      if(!nzchar(shape)) shape <- NULL
      if(!nzchar(size))  size  <- NULL
      if(!nzchar(line))  line  <- NULL
      args <- list(group=group, fill=fill, color=color, shape=shape, size=size, line=line,
                   group.label=group.label, fill.label=fill.label, color.label=color.label,
                   shape.label=shape.label, size.label=size.label, line.label=line.label,
                   scale=scale,
                   legend.position=legend.position, legend.linetype=legend.linetype)
      return(args)
    }
    )
)
rzplot.stratum$accessors("expander")
