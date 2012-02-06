rzplot <- 
setRefClass("RzPlot",
  fields = c("win", "info.bar", "main",
             "geom", "stat", "label", "stratum",
             "facet", "position", "save",
             "df", "p.current", "legend.position", "theme_Rz"),
  methods = list(
    initialize  = function(...) {
      initFields(...)
      
      p.current <<- NULL
      
      geom     <<- new("RzPlotGeom")
      stat     <<- new("RzPlotStat")
      position <<- new("RzPlotPosition")
      stratum  <<- new("RzPlotStratum")
      facet    <<- new("RzPlotFacet")
      label    <<- new("RzPlotLabel")
      save     <<- new("RzPlotSave", win=win)
      theme_Rz <<- theme_grey
      
      # container
      vbox <- gtkVBoxNew()
      vbox["border-width"] <- 2
      vbox$packStart(geom$getExpander(), expand=FALSE)
      vbox$packStart(stat$getExpander(), expand=FALSE)
      vbox$packStart(stratum$getExpander(), expand=FALSE)
      vbox$packStart(position$getExpander(), expand=FALSE)
      vbox$packStart(facet$getExpander(), expand=FALSE)
      vbox$packStart(label$getExpander(), expand=FALSE)
      vbox$packStart(save$getExpander(), expand=FALSE)
      
      main <<- gtkScrolledWindowNew()
      main$setPolicy(GtkPolicyType["automatic"], GtkPolicyType["automatic"])
      main$setShadowType(GtkShadowType["none"])
      main$addWithViewport(vbox)
      
      gSignalConnect(save$getDialog(), "response", .self$onSave)
      gSignalConnect(geom$getButton(), "clicked",  .self$addGeom)
      gSignalConnect(stat$getButton(), "clicked",  .self$addStat)
    },
    
    onSave = function(dialog, response.id){
      save$onSave(response.id, p.current, theme_Rz, legend.position)
    },
    
    onPlot = function(data, col){
      font <- rzSettings$getPlotFontFamily()
      data.frame <- data$getData.frame()
      variableLabels <- data$getVariableLabels()
      variableNames <- data$getVariableNames()
      df <<- data.frame[col]
      colnames(df) <<- "x"
      
      label.args <- label$getArgs()
      title <- label.args$title
      xlab  <- label.args$xlab
      ylab  <- label.args$ylab
      
      geom.args       <-  geom$getArgs()
      stat.args       <-  stat$getArgs()
      position.args   <-  position$getArgs()
      stratum.args    <-  stratum$getArgs()
      legend.position <<- stratum.args$legend.position
      facet.args      <- facet$getArgs()
      
      y <- NULL
      if(!is.null(geom.args$y)){
        y <- geom.args$y
        df$y <<- data.frame[[y]]        
      }
      
      if (geom.args$theme=="gray") {
        theme_Rz <<- theme_grey
      } else if (geom.args$theme=="bw") {
        theme_Rz <<- theme_bw        
      }
      
      position.layer <- switch(position.args$position,
                               identity = position_identity(width=position.args$width, height= position.args$height),
                               dodge    = position_dodge   (width=position.args$width, height= position.args$height),
                               fill     = position_fill    (width=position.args$width, height= position.args$height),
                               stack    = position_stack   (width=position.args$width, height= position.args$height),
                               jitter   = position_jitter  (width=position.args$width, height= position.args$height))
      
      
      if(!is.null(stratum.args$group )){
        df$group  <<- data.frame[[stratum.args$group]]
        if(stratum.args$group.label==gettext("variable label")){
          stratum.args$group.label <- variableLabels[which(stratum.args$group==variableNames)]
        } else if(stratum.args$group.label==gettext("variable name")){
          stratum.args$group.label <- stratum.args$group
        }
      }
      if(!is.null(stratum.args$fill )){
        df$fill  <<- data.frame[[stratum.args$fill]]
        if(stratum.args$fill.label==gettext("variable label")){
          stratum.args$fill.label <- variableLabels[which(stratum.args$fill==variableNames)]
        } else if(stratum.args$fill.label==gettext("variable name")){
          stratum.args$fill.label <- stratum.args$fill
        }
      }
      if(!is.null(stratum.args$color)){
        df$color <<- data.frame[[stratum.args$color]]
        if(stratum.args$color.label==gettext("variable label")){
          stratum.args$color.label <- variableLabels[which(stratum.args$color==variableNames)]
        } else if(stratum.args$color.label==gettext("variable name")){
          stratum.args$color.label <- stratum.args$color
        }
      }
      if(!is.null(stratum.args$shape)){
        df$shape <<- data.frame[[stratum.args$shape]]
        if(stratum.args$shape.label==gettext("variable label")){
          stratum.args$shape.label <- variableLabels[which(stratum.args$shape==variableNames)]
        } else if(stratum.args$shape.label==gettext("variable name")){
          stratum.args$shape.label <- stratum.args$shape
        }
      }
      if(!is.null(stratum.args$size )){
        df$size  <<- data.frame[[stratum.args$size]]
        if(stratum.args$size.label==gettext("variable label")){
          stratum.args$size.label <- variableLabels[which(stratum.args$size==variableNames)]
        } else if(stratum.args$size.label==gettext("variable name")){
          stratum.args$size.label <- stratum.args$size
        }
        
      }
      
      if(facet.args$on){
        if(facet.args$x!="" && !is.null(data.frame[[facet.args$x]])){
          df$facet.var1 <<- as.factor(data.frame[[facet.args$x]])
        }
        if(facet.args$y!="" && !is.null(data.frame[[facet.args$y]])){
          df$facet.var2 <<- as.factor(data.frame[[facet.args$y]])
        }
        
      }
      
      if(geom.args$na.rm) df <<- na.omit(df)

      p <- NULL
      
      if (geom.args$geom == "jitter") {
        p <- qplot(data=df, geom=geom.args$geom)      
      } else {
        p <- qplot(data=df, geom=geom.args$geom, position=position.layer)
      }
      
      if (geom.args$geom == "histogram") {
        # ------------ geom_histogram ------------
        breaks <- geom.args$hist.break
        suppressWarnings(binwidth <- as.numeric(breaks))
        if(breaks=="based on Sturges"){
          breaks <- nclass.Sturges(df$x)
        } else if (breaks=="based on Freedman-Diaconis"){
          breaks <- nclass.FD(df$x)
        } else if (breaks=="based on Scott"){
          breaks <- nclass.scott(df$x)
        } else {
          breaks <- NA
        }
        if(suppressWarnings(!is.na(breaks))){
          breaks <- pretty(range(as.numeric(df$x), na.rm=TRUE), n = breaks, min.n = 1)
        }
        
        if (!is.na(binwidth)) {
          p <- p + stat_bin(binwidth=binwidth)
        } else if (!is.na(breaks)) {
          p <- p + stat_bin(breaks=breaks)          
        }        
      }
            
      if(is.null(y)){
        p <- p + aes(x=x)
      } else {
        p <- p + aes(x=x, y=y)        
      }
      
      
      # ------------------------ labels ------------------------
      if(xlab==gettext("variable label")){
        xlab <- variableLabels[col]
      } else if (xlab==gettext("variable name")){
        xlab <- variableNames[col]
      }
      p <- p + xlab(xlab)
      
      if (!is.null(y)){
        if(ylab==gettext("variable label")){
          ylab <- variableLabels[which(variableNames==y)]
        } else if (ylab==gettext("variable name")){
          ylab <- y
        }
        p <- p + ylab(ylab)        
      } else if(ylab!=gettext("variable label")&&ylab!=gettext("variable name")){
        p <- p + ylab(ylab)
      }
      
      
      # ------------------------ stat ------------------------
      if(stat.args$stat=="density"){
        p <- p + stat_density()
      } else if(stat.args$stat=="smooth"){
        p <- p + stat_smooth()
      } else if(stat.args$stat=="quantile"){
        p <- p + stat_quantile()
      } else if(stat.args$stat=="sum"){
        p <- p + stat_sum(aes(group = 1)) 
      }
      
      
      # ------------------------ facet ------------------------
      if(facet.args$on){
        if(facet.args$facet=="grid"){
          if(facet.args$x=="") {
            p <- p + facet_grid(facet.var2 ~ .)
          } else if(facet.args$y==""){
            p <- p + facet_grid(. ~ facet.var1)
          } else{
            p <- p + facet_grid(facet.var2 ~ facet.var1)
          }
        } else {
          p <- p + facet_wrap(~ facet.var1,
                              nrow  = facet.args$nrow,
                              ncol  = facet.args$ncol,
                              scale = facet.args$scale)
        }
      }
      
      
      # ------------------------ stratum ------------------------
      if(!is.null(stratum.args$group)){
        p <- p + aes(group=group)  + labs(group=stratum.args$group.label)
      }
      if(!is.null(stratum.args$fill)){
        p <- p + aes(fill=fill)    + labs(fill=stratum.args$fill.label)
      }
      if(!is.null(stratum.args$color)){
        p <- p + aes(colour=color) + labs(colour=stratum.args$color.label)
      }
      if(!is.null(stratum.args$shape)){
        p <- p + aes(shape=shape)  + labs(shape=stratum.args$shape.label)
      }
      if(!is.null(stratum.args$size)){
        p <- p + aes(size=size)    + labs(size=stratum.args$size.label)
      }
      
      
      # ------------------------ themes ------------------------
      if(grepl("mingw", R.Version()$os) & !rzSettings$getEmbededDeviceOn()){
        windowsFonts(F = windowsFont(font))
        p <- p + theme_Rz(base_family="F")
      } else if(grepl("darwin", R.Version()$os) & !rzSettings$getEmbededDeviceOn()){
        X11Fonts(F=X11Font(sprintf("-*-%s-*-*-*-*-*-*-*-*-*-*-*-*", font)))
        quartzFonts(F = quartzFont(rep(font, 4)))
        p <- p + theme_Rz(base_family="F")
      } else {
        p <- p + theme_Rz(base_family=font)
      }
      
      
      # ------------------------ flip ------------------------
      if(geom.args$flip){
        p <- p + coord_flip()
      }
      
      
      # ------------------------ title ------------------------
      if(title!="") {p <- p  + opts(title=title)}
      
      
      # ------------------------ plot ------------------------
      p <- p + opts(legend.position=stratum.args$legend.position)
      
      con <- textConnection("str", open="w", local=TRUE)
      str <- ""
      sink(con, type="message")
      e <- try(print(p), silent=TRUE)
      sink(NULL, type="message")
      close(con)
      
      if (nzchar(str)) {
        info.bar$setMessageType(GtkMessageType["warning"])
        info.bar$setText(paste(str, collapse="\n"))
        info.bar$show()
        p.current <<- p
        assign("rz.last.plot", p.current, envir=.GlobalEnv)
      } else if (!is.null(e)) {
        info.bar$setMessageType(GtkMessageType["error"])
        info.bar$setText(e[1])
        info.bar$show()
      } else {
        p.current <<- p
        assign("rz.last.plot", p.current, envir=.GlobalEnv)
        info.bar$hide()
      }
      
    },
    
    addGeom = function(...){
      if(is.null(p.current)) return()
      geom.args <- geom$getArgs()
      position.args   <-  position$getArgs()
      position.layer <- switch(position.args$position,
                               identity = position_identity(width=position.args$width, height= position.args$height),
                               dodge    = position_dodge   (width=position.args$width, height= position.args$height),
                               fill     = position_fill    (width=position.args$width, height= position.args$height),
                               stack    = position_stack   (width=position.args$width, height= position.args$height),
                               jitter   = position_jitter  (width=position.args$width, height= position.args$height))
      
      p <- p.current
      
      if (geom.args$geom == "jitter") {
        # ------------ geom_jitter ------------
        p <- p + geom_jitter()
      } else {
        geom_selected <- get(sprintf("geom_%s", geom.args$geom))
        p <- p + geom_selected(position=position.layer)
      }
      
      if (geom.args$geom == "histogram") {
        # ------------ geom_histogram ------------
        breaks <- geom.args$hist.break
        suppressWarnings(binwidth <- as.numeric(breaks))
        if(breaks=="based on Sturges"){
          breaks <- nclass.Sturges(df$x)
        } else if (breaks=="based on Freedman-Diaconis"){
          breaks <- nclass.FD(df$x)
        } else if (breaks=="based on Scott"){
          breaks <- nclass.scott(df$x)
        } else {
          breaks <- NA
        }
        if(suppressWarnings(!is.na(breaks))){
          breaks <- pretty(range(as.numeric(df$x), na.rm=TRUE), n = breaks, min.n = 1)
        }
        
        if (!is.na(binwidth)) {
          p <- p + stat_bin(binwidth=binwidth)
        } else if (!is.na(breaks)) {
          p <- p + stat_bin(breaks=breaks)          
        }
      }
      
      con <- textConnection("str", open="w", local=TRUE)
      str <- ""
      sink(con, type="message")
      e <- try(print(p), silent=TRUE)
      sink(NULL, type="message")
      close(con)
      
      if (nzchar(str)) {
        info.bar$setMessageType(GtkMessageType["warning"])
        info.bar$setText(paste(str, collapse="\n"))
        info.bar$show()
        p.current <<- p
        assign("rz.last.plot", p.current, envir=.GlobalEnv)
      } else if (!is.null(e)) {
        info.bar$setMessageType(GtkMessageType["error"])
        info.bar$setText(e[1])
        info.bar$show()
      } else {
        p.current <<- p
        assign("rz.last.plot", p.current, envir=.GlobalEnv)
        info.bar$hide()
      }
      
    },
    
    addStat = function(...){
      if(is.null(p.current)) return()
      p <- p.current
      stat.args <- stat$getArgs()
      
      if(stat.args$stat=="none"){
        return()
      } else if(stat.args$stat=="smooth"){
        p <- p + stat_smooth()
      } else if(stat.args$stat=="quantile"){
        p <- p + stat_quantile()
      } else if(stat.args$stat=="sum"){
        if(is.null(p$mapping$group)){
          p <- p + stat_sum(aes(group = 1))           
        } else {
          p <- p + stat_sum()
        }
      }
      
      con <- textConnection("str", open="w", local=TRUE)
      str <- ""
      sink(con, type="message")
      e <- try(print(p), silent=TRUE)
      sink(NULL, type="message")
      close(con)
      
      if (nzchar(str)) {
        info.bar$setMessageType(GtkMessageType["warning"])
        info.bar$setText(paste(str, collapse="\n"))
        info.bar$show()
        p.current <<- p
        assign("rz.last.plot", p.current, envir=.GlobalEnv)
      } else if (!is.null(e)) {
        info.bar$setMessageType(GtkMessageType["error"])
        info.bar$setText(e[1])
        info.bar$show()
      } else {
        p.current <<- p
        assign("rz.last.plot", p.current, envir=.GlobalEnv)
        info.bar$hide()
      }
    },
    
    setModel = function(model){
      geom$completionSetModel(model)
      stratum$completionSetModel(model)
      facet$completionSetModel(model)
    }
))
rzplot$accessors("main", "info.bar")
