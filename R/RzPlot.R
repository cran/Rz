rzplot <- 
setRefClass("RzPlot",
  fields = c("win", "info.bar", "main", "data", "model",
             "geom", "stat", "label", "stratum", "button.prev", "button.next",
             "facet", "position", "misc", "save",
             "df", "p.current", "p.list", "p.current.num",
             "legend.position", "legend.linetype", "legend.justification", "theme_Rz"),
  methods = list(
    initialize  = function(...) {
      initFields(...)
      
      p.current <<- NULL
      model <<- NULL
      data <<- NULL
      p.list <<- list()
      p.current.num <<- 1
      
      button.execute       <- gtkButtonNewFromStock(GTK_STOCK_EXECUTE)
      
      image  <- gtkImageNewFromStock(GTK_STOCK_CLEAR, GtkIconSize["button"])
      button.clear          <- gtkButtonNew()
      button.clear["sensitive"] <- TRUE
      button.clear$setImage(image)
      
      image  <- gtkImageNewFromStock(GTK_STOCK_GO_BACK, GtkIconSize["button"])
      button.prev          <<- gtkButtonNew()
      button.prev["sensitive"] <<- FALSE
      button.prev$setImage(image)
      
      image  <- gtkImageNewFromStock(GTK_STOCK_GO_FORWARD, GtkIconSize["button"])
      button.next          <<- gtkButtonNew()
      button.next["sensitive"] <<- FALSE
      button.next$setImage(image)
      
      button.box.history <- gtkHBoxNew()
      button.box.history$packStart(button.prev, expand=FALSE)
      button.box.history$packStart(button.next, expand=FALSE)
            
      button.box1          <- gtkHBoxNew()
      button.box1$packEnd(button.execute, padding=5, expand=FALSE)
      button.box1$packEnd(button.box.history, expand=FALSE)
      button.box1$packEnd(button.clear, padding=5, expand=FALSE)
      
      geom     <<- new("RzPlotGeom")
      stat     <<- new("RzPlotStat")
      position <<- new("RzPlotPosition")
      stratum  <<- new("RzPlotStratum")
      facet    <<- new("RzPlotFacet")
      label    <<- new("RzPlotLabel")
      misc     <<- new("RzPlotMisc")
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
      vbox$packStart(misc$getExpander(), expand=FALSE)
      vbox$packStart(save$getExpander(), expand=FALSE)
      
      sw <- gtkScrolledWindowNew()
      sw$setPolicy(GtkPolicyType["automatic"], GtkPolicyType["automatic"])
      sw$setShadowType(GtkShadowType["none"])
      sw$addWithViewport(vbox)
      
      main <<- gtkVBoxNew()
      main$packStart(button.box1, expand=FALSE, padding=5)
      main$packStart(sw, expand=TRUE, fill=TRUE)
      
      
      gSignalConnect(button.execute, "clicked", function(button){
        if(!is.null(data)) .self$onPlot()
      })
      
      gSignalConnect(button.clear, "clicked", function(button){
        geom$clear()
        stat$clear()
        position$clear()
        stratum$clear()
        facet$clear()
        label$clear()
        misc$clear()
      })
      
      gSignalConnect(button.prev, "clicked", function(button){
        p.current.num <<- p.current.num + 1
        p.current <<- p.list[[p.current.num]]
        suppressWarnings(print(p.current))
        button.next["sensitive"] <<- TRUE
        if(p.current.num==length(p.list)) button["sensitive"] <- FALSE        
      })

      gSignalConnect(button.next, "clicked", function(button){
        p.current.num <<- p.current.num - 1
        p.current <<- p.list[[p.current.num]]
        suppressWarnings(print(p.current))
        button.prev["sensitive"] <<- TRUE
        if(p.current.num==1) button["sensitive"] <- FALSE        
      })

      gSignalConnect(save$getDialog(), "response", .self$onSave)
      gSignalConnect(geom$getButton(), "clicked",  .self$addGeom)
      gSignalConnect(stat$getButton(), "clicked",  .self$addStat)
    },
    
    onSave = function(dialog, response.id){
      save$onSave(response.id, p.current, theme_Rz, legend.position, legend.linetype, legend.justification)
    },
    
    setX = function(col){
      variableNames <- data$getVariableNames()
      geom$setX(variableNames[col])
    },
    
    onPlot = function(){
      geom.args       <-  geom$getArgs()
      stat.args       <-  stat$getArgs()
      position.args   <-  position$getArgs()
      stratum.args    <-  stratum$getArgs()
      legend.position <<- stratum.args$legend.position
      legend.linetype <<- stratum.args$legend.linetype
      legend.justification <<- ifelse(length(legend.position)==2, c(0.5,0.5), c(0.5, 0.5))
      facet.args      <-  facet$getArgs()
      misc.args       <-  misc$getArgs()

      font <- rzSettings$getPlotFontFamily()
      data.frame <- data$getData.frame()
      variableLabels <- data$getVariableLabels()
      variableNames <- data$getVariableNames()
      
      col <- which(variableNames==geom.args$x)
      if(length(col)==0){
        info.bar$setMessageType(GtkMessageType["error"])
        info.bar$setText(gettext("Please enter valid x."))
        info.bar$show()
        return()
      }
      
      df <<- data.frame[col]
      colnames(df) <<- "x"
      
      label.args <- label$getArgs()
      title <- label.args$title
      xlab  <- label.args$xlab
      ylab  <- label.args$ylab
            
      y <- NULL
      if(!is.null(geom.args$y)){
        y <- geom.args$y
        df$y <<- data.frame[[y]]        
      }
      
      if (misc.args$theme=="grey") {
        theme_Rz <<- theme_grey
      } else if (misc.args$theme=="bw") {
        theme_Rz <<- theme_bw        
      }
      
      position.layer <- switch(position.args$position,
                               default  = NULL,
                               identity = position_identity(width=position.args$width, height= position.args$height),
                               dodge    = position_dodge   (width=position.args$width, height= position.args$height),
                               fill     = position_fill    (width=position.args$width, height= position.args$height),
                               stack    = position_stack   (width=position.args$width, height= position.args$height),
                               jitter   = position_jitter  (width=position.args$width, height= position.args$height))
      
      group <- NULL
      fill  <- NULL
      color <- NULL
      shape <- NULL
      size  <- NULL
      line  <- NULL
      if(!is.null(stratum.args$group )){
        group <- stratum.args$group
        df$group <<- data.frame[[group]]
        if(is.null(df$group)){
          group <- 1
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
      if(!is.null(stratum.args$line)){
        df$line  <<- data.frame[[stratum.args$line]]
        if(stratum.args$line.label==gettext("variable label")){
          stratum.args$line.label <- variableLabels[which(stratum.args$line==variableNames)]
        } else if(stratum.args$line.label==gettext("variable name")){
          stratum.args$line.label <- stratum.args$line
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

      scale_x <- switch(misc.args$scalex,
                        default=NULL,
                        get(sprintf("scale_x_%s", misc.args$scalex),
                            envir=findPackageEnv("package:ggplot2"))
                        )
      scale_y <- switch(misc.args$scaley,
                        default=NULL,
                        get(sprintf("scale_y_%s", misc.args$scaley),
                            envir=findPackageEnv("package:ggplot2"))
                        )
      
      if(misc.args$na.rm) df <<- na.omit(df)

      p <- NULL
      
#      if (geom.args$geom == "jitter") {
      if (is.null(position.layer)) {
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
        } else if (all(!is.na(breaks))) {
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
        p <- p + stat_smooth(method=stat.args$method)
      } else if(stat.args$stat=="quantile"){
        p <- p + stat_quantile()
      } else if(stat.args$stat=="sum"){
        if(is.null(group)){
          p <- p + stat_sum(aes(group = 1))
        } else {
          p <- p + stat_sum()          
        }
      } else if(stat.args$stat=="summary"){
        if (is.null(position.layer)) {
          if(is.null(stat.args$color)){
            p <- p + stat_summary(fun.data=stat.args$fun, geom=stat.args$geom, size=stat.args$size)            
          } else {
            p <- p + stat_summary(fun.data=stat.args$fun, geom=stat.args$geom, size=stat.args$size, color=stat.args$color)
          }
          
          if(stat.args$geom2!="none") {
            p <- p + stat_summary(fun.y=ifelse(stat.args$fun=="median_hilow","median", "mean"),
                                  geom=stat.args$geom2)
          }
        } else {
          if(is.null(stat.args$color)){
            p <- p + stat_summary(fun.data=stat.args$fun, geom=stat.args$geom, size=stat.args$size, position=position.layer)            
          } else {
            p <- p + stat_summary(fun.data=stat.args$fun, geom=stat.args$geom, size=stat.args$size, color=stat.args$color, position=position.layer)
          }
          if(stat.args$geom2!="none") {
            p <- p + stat_summary(fun.y=ifelse(stat.args$fun=="median_hilow","median", "mean"),
                                  geom=stat.args$geom2, position=position.layer)
          }
        }
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
        p <- p + aes(group=group)
      }
      if(!is.null(stratum.args$fill)){
        scale <- switch(stratum.args$scale,
                        hue = scale_fill_hue(),
                        grey= scale_fill_grey(),
                        scale_fill_brewer(palette=stratum.args$scale))
        p <- p + scale
        p <- p + aes(fill=fill)     + labs(fill=stratum.args$fill.label)
      }
      if(!is.null(stratum.args$color)){
        scale <- switch(stratum.args$scale,
                        hue = scale_colour_hue(),
                        grey= scale_colour_grey(),
                        scale_colour_brewer(palette=stratum.args$scale))
        p <- p + scale
        p <- p + aes(colour=color)  + labs(colour=stratum.args$color.label)
      }
      if(!is.null(stratum.args$shape)){
        p <- p + aes(shape=shape)   + labs(shape=stratum.args$shape.label)
      }
      if(!is.null(stratum.args$size)){
        p <- p + aes(size=size)     + labs(size=stratum.args$size.label)
      }
      if(!is.null(stratum.args$line)){
        p <- p + aes(linetype=line) + labs(linetype=stratum.args$line.label)
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
      
      
      
      # ------------------------ scale & coord ------------------------
      if(!is.null(scale_x)) p <- p + scale_x()
      if(!is.null(scale_y)) p <- p + scale_y()
      if(any(c(misc.args$coordx, misc.args$coordy)!="identity")) {
        p <- p + coord_trans(xtrans=misc.args$coordx, ytrans=misc.args$coordy)        
      }
      p <- p + coord_cartesian(xlim=misc.args$xlim, ylim=misc.args$ylim)
      if(misc.args$flip) p <- p + coord_flip()
      
      
      # ------------------------ title ------------------------
      if(title!="") {p <- p  + opts(title=title)}
      
      
      # ------------------------ plot ------------------------
      
      p <- p + opts(legend.position=stratum.args$legend.position,
                    legend.background = theme_rect(fill="white", linetype=stratum.args$legend.linetype),
                    legend.justification = legend.justification)
      
      con <- textConnection("str", open="w", local=TRUE)
      str <- ""
      sink(con, type="message")
      e <- try(print(p), silent=TRUE)
      sink(NULL, type="message")
      close(con)
      
      if (nzchar(str)) {
        info.bar$setMessageType(GtkMessageType["warning"])
        str <- paste(str, collapse="\n")
        str <- strsplit(str, " ")[[1]]
        str <- capture.output(cat(str, fill=80))
        str <- paste(str, collapse="\n")
        info.bar$setText(str)
        info.bar$show()
        p.current <<- p
        p.list    <<- c(list(p), p.list)
        p.current.num <<- 1
        if(length(p.list) >= 2) button.prev["sensitive"] <<- TRUE
        button.next["sensitive"] <<- FALSE
        assign("rz.last.plot", p.current, envir=.GlobalEnv)
      } else if (!is.list(e)) {
        info.bar$setMessageType(GtkMessageType["error"])
        str <- paste(e[1], collapse="\n")
        str <- strsplit(str, " ")[[1]]
        str <- capture.output(cat(str, fill=80))
        str <- paste(str, collapse="\n")
        info.bar$setText(str)
        info.bar$show()
      } else {
        p.current <<- p
        p.list    <<- c(list(p), p.list)
        p.current.num <<- 1
        if(length(p.list) >= 2) button.prev["sensitive"] <<- TRUE
        button.next["sensitive"] <<- FALSE
        assign("rz.last.plot", p.current, envir=.GlobalEnv)
        info.bar$hide()
      }
      
    },
    
    addGeom = function(...){
      if(is.null(p.current)) return()
      geom.args <- geom$getArgs()
      position.args   <-  position$getArgs()
      position.layer <- switch(position.args$position,
                               default  = NULL,
                               identity = position_identity(width=position.args$width, height= position.args$height),
                               dodge    = position_dodge   (width=position.args$width, height= position.args$height),
                               fill     = position_fill    (width=position.args$width, height= position.args$height),
                               stack    = position_stack   (width=position.args$width, height= position.args$height),
                               jitter   = position_jitter  (width=position.args$width, height= position.args$height))
      
      p <- p.current
      
#      if (geom.args$geom == "jitter") {
#        # ------------ geom_jitter ------------
#        p <- p + geom_jitter()
#      } else {
#        geom_selected <- get(sprintf("geom_%s", geom.args$geom))
#        p <- p + geom_selected(position=position.layer)
#      }
      
      geom_selected <- get(sprintf("geom_%s", geom.args$geom))
      if(is.null(position.layer)){
        p <- p + geom_selected()        
      } else{
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
        str <- paste(str, collapse="\n")
        str <- strsplit(str, " ")[[1]]
        str <- capture.output(cat(str, fill=80))
        str <- paste(str, collapse="\n")
        info.bar$setText(str)
        info.bar$show()
        p.current <<- p
        p.list    <<- c(list(p), p.list)
        p.current.num <<- 1
        if(length(p.list) >= 2) button.prev["sensitive"] <<- TRUE
        button.next["sensitive"] <<- FALSE
        assign("rz.last.plot", p.current, envir=.GlobalEnv)
      } else if (!is.list(e)) {
        info.bar$setMessageType(GtkMessageType["error"])
        str <- paste(e[1], collapse="\n")
        str <- strsplit(str, " ")[[1]]
        str <- capture.output(cat(str, fill=80))
        str <- paste(str, collapse="\n")
        info.bar$setText(str)
        info.bar$show()
      } else {
        p.current <<- p
        p.list    <<- c(list(p), p.list)
        p.current.num <<- 1
        if(length(p.list) >= 2) button.prev["sensitive"] <<- TRUE
        button.next["sensitive"] <<- FALSE
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
        if(is.null(p$mapping$group)){
          p <- p + stat_smooth(method=stat.args$method, aes(group=1))
        } else {
          p <- p + stat_smooth(method=stat.args$method)          
        }
      } else if(stat.args$stat=="quantile"){
        p <- p + stat_quantile()
      } else if(stat.args$stat=="sum"){
        if(is.null(p$mapping$group)){
          p <- p + stat_sum(aes(group = 1))           
        } else {
          p <- p + stat_sum()
        }
      } else if(stat.args$stat=="summary"){
        if(is.null(p$mapping$group)) p <- p + aes(group=1)
        
        if(is.null(stat.args$color)){
          p <- p + stat_summary(fun.data=stat.args$fun, geom=stat.args$geom, size=stat.args$size)            
        } else {
          p <- p + stat_summary(fun.data=stat.args$fun, geom=stat.args$geom, size=stat.args$size, color=stat.args$color)
        }
        
        if(stat.args$geom2!="none") {
          p <- p + stat_summary(fun.y=ifelse(stat.args$fun=="median_hilow","median", "mean"),
                                geom=stat.args$geom2)
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
        str <- paste(str, collapse="\n")
        str <- strsplit(str, " ")[[1]]
        str <- capture.output(cat(str, fill=80))
        str <- paste(str, collapse="\n")
        info.bar$setText(str)
        info.bar$show()
        p.current <<- p
        p.list    <<- c(list(p), p.list)
        p.current.num <<- 1
        if(length(p.list) >= 2) button.prev["sensitive"] <<- TRUE
        button.next["sensitive"] <<- FALSE
        assign("rz.last.plot", p.current, envir=.GlobalEnv)
      } else if (!is.list(e)) {
        info.bar$setMessageType(GtkMessageType["error"])
        str <- paste(e[1], collapse="\n")
        str <- strsplit(str, " ")[[1]]
        str <- capture.output(cat(str, fill=80))
        str <- paste(str, collapse="\n")
        info.bar$setText(str)
        info.bar$show()
      } else {
        p.current <<- p
        p.list    <<- c(list(p), p.list)
        p.current.num <<- 1
        if(length(p.list) >= 2) button.prev["sensitive"] <<- TRUE
        button.next["sensitive"] <<- FALSE
        assign("rz.last.plot", p.current, envir=.GlobalEnv)
        info.bar$hide()
      }
    },
    
    setModel = function(model){
      model <<- model
      geom$completionSetModel(model)
      stratum$completionSetModel(model)
      facet$completionSetModel(model)
    }
))
rzplot$accessors("main", "info.bar", "data")
