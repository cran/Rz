analysisStat <-
setRefClass("RzAnalysisStat",
  fields = c("main", "textview", "button.clear", "signal.clear",
             "scrolledwindow.vbox", "hbox.methods"),
  methods = list(
    initialize            = function(...) {
      initFields(...)
      signal.clear   <<- NULL
      
      methods <- c(gettext("Basic Statistics"),
                   gettext("Cross Tabulation"),
                   gettext("Correlation"))
      
      combo.methods <- gtkComboBoxNewText()
      for(i in methods) combo.methods$appendText(i)
      hbox.methods  <<- gtkHBoxNew(spacing=5)
      hbox.methods$packStart(combo.methods)
      
      textview <<- gtkTextViewNew()
      textview$modifyFont(pangoFontDescriptionFromString(rzSettings$getMonospaceFont()))
      textview$setLeftMargin(5)
      textview$setRightMargin(5)
      scrolledwindow.textview <- gtkScrolledWindowNew()
      scrolledwindow.textview["shadow-type"] <- GtkShadowType["in"]
      scrolledwindow.textview$setPolicy(GtkPolicyType["automatic"], GtkPolicyType["automatic"])
      scrolledwindow.textview$add(textview)
      
      button.execute <-  gtkButtonNewFromStock(GTK_STOCK_EXECUTE)
      button.clear   <<- gtkButtonNewFromStock(GTK_STOCK_CLEAR)
      button.box     <- gtkHButtonBoxNew()
      button.box$packStart(button.clear)
      button.box$packStart(button.execute)
      
      scrolledwindow.vbox <<- gtkScrolledWindowNew()
      scrolledwindow.vbox$setPolicy(GtkPolicyType["automatic"], GtkPolicyType["automatic"])
      
      hpaned <- gtkHPanedNew()
      hpaned$pack1(scrolledwindow.vbox    , resize=FALSE)
      hpaned$pack2(scrolledwindow.textview, resize=TRUE)
      hpaned$setPosition(280)
      
      main <<- gtkVBoxNew(spacing=0)
      main$packStart(hbox.methods, expand=FALSE, padding=2)
      main$packStart(hpaned      , expand=TRUE , padding=2)
      main$packStart(button.box  , expand=FALSE, padding=2)
            
      gSignalConnect(combo.methods, "changed", .self$toggleMethods)
      
      gSignalConnect(button.execute, "clicked", function(button){
        variable.view <- rzTools$getVariableView()
        if(is.null(variable.view)) return()
        
        buffer <- textview$getBuffer()
        iter   <- buffer$getBounds()
        script <- localize(buffer$getText(iter$start, iter$end))
        script <- sub("^([[:space:]]+)([^[:space:]]+)([[:space:]]+)$", "\\2", script)
        if(!nzchar(script)) return()
        
        cat(gettext("\n=============== Output by Rz ===============\n"), fill=TRUE)
        e <- try(eval(parse(text=script), envir=.GlobalEnv), silent=TRUE)
        info.bar <- rzTools$getInfoBar()
        if (any(class(e)=="try-error")) {
          info.bar$setMessageType(GtkMessageType["error"])
          info.bar$setText(e[1])
          info.bar$show()
        } else {
          if (!is.null(e)) {
            print(e)
          }
          info.bar$hide()
        }
      })
      
    },
    
    toggled = function(){

    },
    
    toggleMethods = function(combo){
      text <- localize(combo$getActiveText())
      vbox <- NULL
      child <- scrolledwindow.vbox$getChild()
      if( !is.null(child) ) {
        scrolledwindow.vbox$remove(child)
        gSignalHandlerDisconnect(button.clear  , signal.clear)
      }
      
      if (text == gettext("Basic Statistics")){
        vbox <- constructBasicStatistics()
      } else if (text == gettext("Cross Tabulation")){
        vbox <- constructCrossTable()
      } else if (text == gettext("Correlation")){
        vbox <- constructCorrelation()
      }
      
      scrolledwindow.vbox$addWithViewport(vbox)
      scrolledwindow.vbox$getChild()$setShadowType(GtkShadowType["none"])
      
    },
    
    constructBasicStatistics = function(){
      setScript <- function(object=NULL, widgets){
        script <- NULL
        variable.view  <- rzTools$getVariableView()
        if(is.null(variable.view)) return()
        data.set.name  <- variable.view$getData()$getData.set.name()
        variable.names <- variable.view$getData()$getVariableNames()
        variable.names <- variable.names[variable.view$getSelectedRows()]
        variable.names <- paste("\"", variable.names, "\"", sep="", collapse=", ")
        
        group  <- localize(widgets[[1]]$getActiveText())
        check1 <- widgets[[2]]$getActive()
        check2 <- widgets[[3]]$getActive()
        check3 <- widgets[[4]]$getActive()
        check4 <- widgets[[5]]$getActive()
        trim   <- localize(widgets[[6]]$getText())
        type   <- localize(widgets[[7]]$getActiveText())

        if(!nzchar(group)){
          script <- sprintf("psych::describe(%s[c(%s)],\n    na.rm = %s, interp = %s, skew = %s, ranges = %s,\n    trim=%s, type=%s)",
                            data.set.name, variable.names,
                            check1, check2, check3, check4,
                            trim, type)
        } else {
          script <- sprintf("psych::describe.by(%s[c(%s)],\n    group=%s$%s,\n    na.rm = %s, interp = %s, skew = %s, ranges = %s,\n    trim=%s, type=%s)",
                            data.set.name, variable.names,
                            data.set.name, group,
                            check1, check2, check3, check4,
                            trim, type)
        }
        
        textview$getBuffer()$setText(script)
      }
      
      group.combo <- new("RzCompletionCombo")
      group.label <- gtkLabelNew("group")
      check1      <- gtkCheckButtonNewWithLabel("na.rm")
      check2      <- gtkCheckButtonNewWithLabel("interp")
      check3      <- gtkCheckButtonNewWithLabel("skew")
      check4      <- gtkCheckButtonNewWithLabel("ranges")
      trim.entry  <- gtkEntryNew()
      trim.label  <- gtkLabelNew("trim")
      type.combo  <- gtkComboBoxNewText()
      text <- c("1", "2", "3")
      for(i in text) type.combo$appendText(i)
      type.label  <- gtkLabelNew("type")
      
      check1$setActive(TRUE)
      check3$setActive(TRUE)
      check4$setActive(TRUE)
      trim.entry$setText(".1")
      type.combo$setActive(2)
      
      widgets <- list(group.combo, check1, check2, check3, check4, trim.entry, type.combo)
      
      variable.view <- rzTools$getVariableView()
      if(!is.null(variable.view)){
        model  <- variable.view$getListstore()
        group.combo$setModel(model)
      }
      
      basic.table <- gtkTableNew(homogeneous=FALSE)
      basic.table$setSizeRequest(250, -1)
      basic.table$attach(group.label           , 0, 1, 0, 1, xpadding=2, xoptions="shrink")
      basic.table$attach(group.combo$getCombo(), 1, 2, 0, 1)
      basic.table$attach(check1                , 0, 2, 1, 2, xpadding=2, xoptions="fill")
      basic.table$attach(check2                , 0, 2, 2, 3, xpadding=2, xoptions="fill")
      basic.table$attach(check3                , 0, 2, 3, 4, xpadding=2, xoptions="fill")
      basic.table$attach(check4                , 0, 2, 4, 5, xpadding=2, xoptions="fill")
      basic.table$attach(trim.label            , 0, 1, 5, 6, xpadding=2, xoptions="shrink")
      basic.table$attach(trim.entry            , 1, 2, 5, 6)
      basic.table$attach(type.label            , 0, 1, 6, 7, xpadding=2, xoptions="shrink")
      basic.table$attach(type.combo            , 1, 2, 6, 7)
      
      vbox <- gtkVBoxNew()
      hbox <- gtkHBoxNew()
      hbox$packStart(basic.table, padding=2, expand=FALSE)
      vbox$packStart(hbox       , padding=2, expand=FALSE)
            
      gSignalConnect(group.combo$getCombo(), "changed", setScript, widgets)
      gSignalConnect(check1                , "toggled", setScript, widgets)
      gSignalConnect(check2                , "toggled", setScript, widgets)
      gSignalConnect(check3                , "toggled", setScript, widgets)
      gSignalConnect(check4                , "toggled", setScript, widgets)
      gSignalConnect(trim.entry            , "changed", setScript, widgets)
      gSignalConnect(type.combo            , "changed", setScript, widgets)
      
      signal.clear <<- gSignalConnect(button.clear, "clicked", function(button){
        group.combo$clear()
        check1$setActive(TRUE)
        check2$setActive(FALSE)
        check3$setActive(TRUE)
        check4$setActive(TRUE)
        trim.entry$setText(".1")
        type.combo$setActive(2)
      })
      
      setScript(widgets=widgets)
      
      return(vbox)
    },
    
    constructCrossTable = function(){
      setScript <- function(combo=NULL, list.combo){
        script <- NULL
        data.set.name <- rzTools$getVariableView()$getData()$getData.set.name()
        x <- localize(list.combo[[1]]$getActiveText())
        y <- localize(list.combo[[2]]$getActiveText())
        z <- localize(list.combo[[3]]$getActiveText())
        if(!nzchar(z)){
          script <- sprintf("with(%s,\n    summary(crossTable(%s, %s))\n)",
                            data.set.name, x, y)        
        } else {
          script <- sprintf("with(%s,\n    summary(crossTable(%s, %s, %s))\n)",
                            data.set.name, z, x, y)
        }
        
        textview$getBuffer()$setText(script)
      }
      
      cross.combo1 <- new("RzCompletionCombo")
      cross.combo2 <- new("RzCompletionCombo")
      cross.combo3 <- new("RzCompletionCombo")
      cross.label1 <-  gtkLabelNew(gettext("Row"))
      cross.label2 <-  gtkLabelNew(gettext("Col"))
      cross.label3 <-  gtkLabelNew(gettext("Stratum"))
      
      variable.view <- rzTools$getVariableView()
      if(!is.null(variable.view)){
        model  <- variable.view$getListstore()
        cross.combo1$setModel(model)
        cross.combo2$setModel(model)
        cross.combo3$setModel(model)        
      }
      
      cross.table <- gtkTableNew(homogeneous=FALSE)
      cross.table$setSizeRequest(250, -1)
      cross.table$attach(cross.label1           , 0, 1, 0, 1, xpadding=2, xoptions=2)
      cross.table$attach(cross.combo1$getCombo(), 1, 2, 0, 1)
      cross.table$attach(cross.label2           , 0, 1, 1, 2, xpadding=2, xoptions=2)
      cross.table$attach(cross.combo2$getCombo(), 1, 2, 1, 2)
      cross.table$attach(cross.label3           , 0, 1, 2, 3, xpadding=2, xoptions=2)
      cross.table$attach(cross.combo3$getCombo(), 1, 2, 2, 3)
      
      vbox <- gtkVBoxNew()
      hbox <- gtkHBoxNew()
      hbox$packStart(cross.table, padding=2, expand=FALSE)
      vbox$packStart(hbox       , padding=2, expand=FALSE)
      
      gSignalConnect(cross.combo1$getCombo(), "changed", setScript, list(cross.combo1, cross.combo2, cross.combo3))
      gSignalConnect(cross.combo2$getCombo(), "changed", setScript, list(cross.combo1, cross.combo2, cross.combo3))
      gSignalConnect(cross.combo3$getCombo(), "changed", setScript, list(cross.combo1, cross.combo2, cross.combo3))
      
      signal.clear <<- gSignalConnect(button.clear, "clicked", function(button){
        cross.combo1$clear()
        cross.combo2$clear()
        cross.combo3$clear()
      })
      
      setScript(list.combo=list(cross.combo1, cross.combo2, cross.combo3))
      
      return(vbox)
    },
    
    constructCorrelation = function(){
      setScript <- function(combo=NULL, widgets){
        script <- NULL
        variable.view  <- rzTools$getVariableView()
        if(is.null(variable.view)) return()
        data.set.name  <- variable.view$getData()$getData.set.name()
        variable.names <- variable.view$getData()$getVariableNames()
        variable.names <- variable.names[variable.view$getSelectedRows()]
        variable.names <- paste("\"", variable.names, "\"", sep="", collapse=", ")
        
        use     <- sprintf("use = \"%s\""    ,localize(widgets[[1]]$getActiveText()))
        method  <- sprintf("method = \"%s\"" ,localize(widgets[[2]]$getActiveText()))
        adjust  <- sprintf("adjust = \"%s\"" ,localize(widgets[[3]]$getActiveText()))
        check1  <- widgets[[4]]$getActive()
        check2  <- sprintf("smooth = %s"     , widgets[[5]]$getActive())
        check3  <- sprintf("scale = %s"      , widgets[[6]]$getActive())
        check4  <- sprintf("density = %s"    , widgets[[7]]$getActive())
        check5  <- sprintf("ellipses = %s"   , widgets[[8]]$getActive())
        check6  <- sprintf("lm = %s"         , widgets[[9]]$getActive())
        check7  <- sprintf("cor = %s"        , widgets[[10]]$getActive())
        check8  <- sprintf("jiggle = %s"     , widgets[[11]]$getActive())
        factor  <- sprintf("factor = %s"     , localize(widgets[[12]]$getText()))
        check9  <- sprintf("show.points = %s", widgets[[13]]$getActive())
        check10 <- sprintf("rug = %s"        , widgets[[14]]$getActive())
        
        script1 <- sprintf("corr.test(%s[c(%s)],\n    %s, %s, %s)",
                           data.set.name, variable.names, use, method, adjust)
        script2 <- sprintf("pairs.panels(%s[c(%s)],\n    %s, %s, %s,\n    %s, %s, %s,\n    %s, %s, %s,\n    %s, %s)",
                           data.set.name, variable.names,
                           check2, check3, check4, check5, method, check6,
                           check7, check8, factor, check9, check10)
        if(check1){
          script <- sprintf("%s\n%s", script2, script1)
        } else {
          script <- script1
        }
        textview$getBuffer()$setText(script)
      }
      
      text      <- c("pairwise", "complete")
      use.combo <- gtkComboBoxNewText()
      for(i in text) use.combo$appendText(i)

      text      <- c("pearson", "spearman", "kendall")
      method.combo <- gtkComboBoxNewText()
      for(i in text) method.combo$appendText(i)
      
      text      <- c("holm", "hochberg", "hommel", "bonferroni", "BH", "BY", "fdr", "none")
      adjust.combo <- gtkComboBoxNewText()
      for(i in text) adjust.combo$appendText(i)
      
      use.combo$setActive(0)
      method.combo$setActive(0)
      adjust.combo$setActive(0)
      
      use.label    <-  gtkLabelNew(gettext("use"))
      method.label <-  gtkLabelNew(gettext("method"))
      adjust.label <-  gtkLabelNew(gettext("adjust"))
      
      check1  <- gtkCheckButtonNewWithLabel("SPLOM")
      check2  <- gtkCheckButtonNewWithLabel("smooth")
      check3  <- gtkCheckButtonNewWithLabel("scale")
      check4  <- gtkCheckButtonNewWithLabel("density")
      check5  <- gtkCheckButtonNewWithLabel("ellipses")
      check6  <- gtkCheckButtonNewWithLabel("lm")
      check7  <- gtkCheckButtonNewWithLabel("cor")
      check8  <- gtkCheckButtonNewWithLabel("jiggle")
      factor.label <- gtkLabelNew("factor")
      factor.entry <- gtkEntryNew()
      factor.entry$setWidthChars(3)
      factor.hbox <- gtkHBoxNew()
      factor.hbox$packStart(factor.label, padding=2, expand=FALSE)
      factor.hbox$packStart(factor.entry, expand=FALSE)
      check9  <- gtkCheckButtonNewWithLabel("show.points")
      check10 <- gtkCheckButtonNewWithLabel("rug")
      
      check1$setActive(FALSE)
      check2$setActive(TRUE)
      check3$setActive(FALSE)
      check4$setActive(TRUE)
      check5$setActive(TRUE)
      check6$setActive(FALSE)
      check7$setActive(TRUE)
      check8$setActive(FALSE)
      factor.entry$setText("2")
      check9$setActive(TRUE)
      check10$setActive(TRUE)
      
      widgets <- list(use.combo, method.combo, adjust.combo,
                      check1, check2, check3, check4, check5,
                      check6, check7, check8, factor.entry,
                      check9, check10)
      
      vbox.splom <- gtkVBoxNew()
      vbox.splom$packStart(check2)
      vbox.splom$packStart(check3)
      vbox.splom$packStart(check4)
      vbox.splom$packStart(check5)
      vbox.splom$packStart(check6)
      vbox.splom$packStart(check7)
      vbox.splom$packStart(check8)
      vbox.splom$packStart(factor.hbox)
      vbox.splom$packStart(check9)
      vbox.splom$packStart(check10)
      
      
      splom <- gtkFrameNew()
      splom$setSizeRequest(250, -1)
      splom$setBorderWidth(2)
      splom$setLabelWidget(check1)
      splom$add(vbox.splom)
      vbox.splom$setSensitive(FALSE)
      
      cor.table <- gtkTableNew(homogeneous=FALSE)
      cor.table$setSizeRequest(250, -1)
      cor.table$attach(use.label   , 0, 1, 0, 1, xpadding=2, xoptions=2)
      cor.table$attach(use.combo   , 1, 2, 0, 1)
      cor.table$attach(method.label, 0, 1, 1, 2, xpadding=2, xoptions=2)
      cor.table$attach(method.combo, 1, 2, 1, 2)
      cor.table$attach(adjust.label, 0, 1, 2, 3, xpadding=2, xoptions=2)
      cor.table$attach(adjust.combo, 1, 2, 2, 3)
      
      vbox <- gtkVBoxNew()
      hbox <- gtkHBoxNew()
      hbox$packStart(cor.table, padding=2, expand=FALSE)
      vbox$packStart(hbox     , padding=2, expand=FALSE)
      vbox$packStart(splom    , padding=2, expand=FALSE)
      
      gSignalConnect(use.combo   , "changed", setScript, widgets)
      gSignalConnect(method.combo, "changed", setScript, widgets)
      gSignalConnect(adjust.combo, "changed", setScript, widgets)
      gSignalConnect(factor.entry, "changed", setScript, widgets)
      gSignalConnect(check1      , "toggled", setScript, widgets)
      gSignalConnect(check2      , "toggled", setScript, widgets)
      gSignalConnect(check3      , "toggled", setScript, widgets)
      gSignalConnect(check4      , "toggled", setScript, widgets)
      gSignalConnect(check5      , "toggled", setScript, widgets)
      gSignalConnect(check6      , "toggled", setScript, widgets)
      gSignalConnect(check7      , "toggled", setScript, widgets)
      gSignalConnect(check8      , "toggled", setScript, widgets)
      gSignalConnect(check9      , "toggled", setScript, widgets)
      gSignalConnect(check10     , "toggled", setScript, widgets)
      gSignalConnect(check1      , "toggled", function(button){
        if(button$getActive())
          vbox.splom$setSensitive(TRUE)
        else
          vbox.splom$setSensitive(FALSE)
      })
      
      signal.clear <<- gSignalConnect(button.clear, "clicked", function(button){
        use.combo$setActive(0)
        method.combo$setActive(0)
        adjust.combo$setActive(0)
        check1$setActive(FALSE)
        check2$setActive(TRUE)
        check3$setActive(FALSE)
        check4$setActive(TRUE)
        check5$setActive(TRUE)
        check6$setActive(FALSE)
        check7$setActive(TRUE)
        check8$setActive(FALSE)
        factor.entry$setText("2")
        check9$setActive(TRUE)
        check10$setActive(TRUE)
        vbox.splom$setSensitive(FALSE)
      })
      
      setScript(widgets=widgets)
      
      return(vbox)
    },
    
    rebuild = function(){
      hbox.methods$getChildren()[[1]]$destroy()
      
      methods <- c(gettext("Basic Statistics"),
                   gettext("Cross Tabulation"),
                   gettext("Correlation"))
      
      combo.methods <- gtkComboBoxNewText()
      for(i in methods) combo.methods$appendText(i)
      hbox.methods$packStart(combo.methods)
      gSignalConnect(combo.methods, "changed", .self$toggleMethods)
      
    }

  )
)
analysisStat$accessors("main")
