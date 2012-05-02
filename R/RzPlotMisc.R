rzplot.misc <- 
setRefClass("RzPlotMisc",
  fields = c("expander", "combo.theme",
             "flip.togglebutton",
             "na.rm.togglebutton",
             "combo.scalex", "combo.scaley",
             "combo.coordx", "combo.coordy",
             "entry.xlim", "entry.ylim"),
  methods = list(
    initialize  = function(...) {
      initFields(...)
            
      # misc options
      label.scale <- gtkLabelNew("scale")
      label.coord <- gtkLabelNew("coord")
      
      label.scalex <- gtkLabelNew("x")
      combo.scalex <<- gtkComboBoxNewText()
      scales <- c("default","log10", "reverse", "sqrt")
      for(i in scales) combo.scalex$appendText(i)
      combo.scalex$setActive(0)
      
      label.scaley <- gtkLabelNew("y")
      combo.scaley <<- gtkComboBoxNewText()
      for(i in scales) combo.scaley$appendText(i)
      combo.scaley$setActive(0)
      
      
      label.coordx <- gtkLabelNew("x")
      trans <- c("identity", "asn", "atanh","exp", "log",         
                 "log10", "log2", "logit", "probit", "reverse", "sqrt")
      combo.coordx <<- gtkComboBoxNewText()
      for(i in trans) combo.coordx$appendText(i)
      combo.coordx$setActive(0)
      
      label.coordy <- gtkLabelNew("y")
      combo.coordy <<- gtkComboBoxNewText()
      for(i in trans) combo.coordy$appendText(i)
      combo.coordy$setActive(0)
      
      label.limits <-  gtkLabelNew(gettext("axis limits"))
      label.xlim   <-  gtkLabelNew("x")
      label.ylim   <-  gtkLabelNew("y")
      entry.xlim   <<- gtkEntryNew()
      entry.ylim   <<- gtkEntryNew()
      entry.xlim["width-request"] <<- 1
      entry.ylim["width-request"] <<- 1
      
      #flip
      flip.togglebutton <<- gtkToggleButtonNewWithLabel(gettext("flip"))
      
      #remove NA
      na.rm.togglebutton <<- gtkToggleButtonNewWithLabel(gettext("remove NA"))
      na.rm.togglebutton$setActive(TRUE)
      
      #theme
      label.theme <- gtkLabelNew("theme")
      combo.theme <<- gtkComboBoxNewText()
      themes <- c("grey", "bw")
      for(i in themes) combo.theme$appendText(i)
      combo.theme$setActive(0)
      
      
      table <- gtkTableNew(6, 5, FALSE)
      table["border-width"] <- 5
      table$attach        (label.scale       , 0, 1, 0, 1, "shrink", "shrink", 0, 0)
      table$attach        (label.scalex      , 1, 2, 0, 1, "shrink", "shrink", 0, 0)
      table$attachDefaults(combo.scalex      , 2, 3, 0, 1)
      table$attach        (label.scaley      , 3, 4, 0, 1, "shrink", "shrink", 0, 0)
      table$attachDefaults(combo.scaley      , 4, 5, 0, 1)
      table$attach        (label.coord       , 0, 1, 1, 2, "shrink", "shrink", 0, 0)
      table$attach        (label.coordx      , 1, 2, 1, 2, "shrink", "shrink", 0, 0)
      table$attachDefaults(combo.coordx      , 2, 3, 1, 2)
      table$attach        (label.coordy      , 3, 4, 1, 2, "shrink", "shrink", 0, 0)
      table$attachDefaults(combo.coordy      , 4, 5, 1, 2)
      table$attach        (label.limits      , 0, 1, 2, 3, "shrink", "shrink", 0, 0)
      table$attach        (label.xlim        , 1, 2, 2, 3, "shrink", "shrink", 0, 0)
      table$attachDefaults(entry.xlim        , 2, 3, 2, 3)
      table$attach        (label.ylim        , 3, 4, 2, 3, "shrink", "shrink", 0, 0)
      table$attachDefaults(entry.ylim        , 4, 5, 2, 3)
      table$attachDefaults(flip.togglebutton , 0, 5, 3, 4)
      table$attachDefaults(na.rm.togglebutton, 0, 5, 4, 5)
      table$attach        (label.theme       , 0, 1, 5, 6, "shrink", "shrink", 0, 0)
      table$attachDefaults(combo.theme       , 1, 5, 5, 6)
      
      table$setColSpacings(5)
      table$setRowSpacings(2)
            
      expander <<- gtkExpanderNew(gettext("misc options"))
      expander["border-width"] <<- 3
      expander$setExpanded(FALSE)
      expander$add(table)        
    },
    
    clear = function(){
      combo.scalex$setActive(0)
      combo.scaley$setActive(0)
      combo.coordx$setActive(0)
      combo.coordy$setActive(0)
      entry.xlim$setText("")
      entry.ylim$setText("")
      flip.togglebutton$setActive(FALSE)
      na.rm.togglebutton$setActive(TRUE)
      combo.theme$setActive(0)
    },
    
    getArgs = function(){
      scalex <- localize(combo.scalex$getActiveText())
      scaley <- localize(combo.scaley$getActiveText())
      coordx <- localize(combo.coordx$getActiveText())
      coordy <- localize(combo.coordy$getActiveText())
      xlim   <- localize(entry.xlim$getText())
      ylim   <- localize(entry.ylim$getText())
      xlim   <- strsplit(xlim, ",")[[1]]
      ylim   <- strsplit(ylim, ",")[[1]]
      xlim   <- suppressWarnings(as.numeric(xlim))
      ylim   <- suppressWarnings(as.numeric(ylim))
      if(any(is.na(xlim)) | length(xlim)==0) xlim <- NULL
      if(any(is.na(ylim)) | length(ylim)==0) ylim <- NULL
      flip   <- flip.togglebutton$getActive()
      na.rm  <- na.rm.togglebutton$getActive()
      theme  <- combo.theme$getActiveText()
      args <- list(scalex = scalex,
                   scaley = scaley,
                   coordx = coordx,
                   coordy = coordy,
                   xlim   = xlim,
                   ylim   = ylim,
                   flip   = flip,
                   na.rm  = na.rm,
                   theme  = theme)
      return(args)
    }
  )
)
rzplot.misc$accessors("expander")
