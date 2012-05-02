settings <- 
setRefClass("RzSettings",
  fields = c("RzPath", "path", "theme", "theme.this", "globalFont", "variableViewFont", "monospaceFont", "monospaceFontFamily",
    "plotFont", "plotFontFamily", "useDataSetObject", "themesFolder",
    "useEmbededDevice", "embededDeviceOn", "runPlot", "codebookOff", "popupOff",
    "plotViewEnabled", "variableEditorViewEnabled", "psFont", "pdfFont"),
  methods = list(
    load = function(){
      path <<- "~/.Rz"
      RzPath <<- system.file(package = "Rz")
      
      # Does a setting file exits?
      if(file.exists(path)) {
        con <- file(path)
        open(con)
        settings <- dget(con)
        close(con)
        themesFolder     <<- ifelse(is.null(settings$themesFolder), file.path(RzPath, "themes"), settings$themesFolder)
        # platform dependent settings
        if(grepl("linux", R.Version()$os)){
          theme            <<- ifelse(is.null(settings$theme)   , "Default", settings$theme)          
        } else {
          theme            <<- ifelse(is.null(settings$theme)   , "kde42-oxygen", settings$theme)
        }
        if(grepl("darwin", R.Version()$os)){
          globalFont       <<- ifelse(is.null(settings$globalFont)   , "Arial 10"     , settings$globalFont)
          variableViewFont <<- ifelse(is.null(settings$variableViewFont)   , "Arial 10"     , settings$variableViewFont)
          plotFont         <<- ifelse(is.null(settings$plotFont)     , "Arial 10"     , settings$plotFont)
        } else {
          globalFont       <<- ifelse(is.null(settings$globalFont)   , "sans 10"     , settings$globalFont)
          variableViewFont <<- ifelse(is.null(settings$variableViewFont)   , "sans 10"     , settings$variableViewFont)
          plotFont         <<- ifelse(is.null(settings$plotFont)     , "sans 10"     , settings$plotFont)
        }
        # settings        
        monospaceFont    <<- ifelse(is.null(settings$monospaceFont), "monospace 10", settings$monospaceFont)
        monospaceFontFamily <<- pangoFontDescriptionFromString(monospaceFont)$getFamily()
        plotFontFamily   <<- pangoFontDescriptionFromString(plotFont)$getFamily()
        psFont           <<- ifelse(is.null(settings$psFont)       , "sans"        , settings$psFont)
        pdfFont          <<- ifelse(is.null(settings$pdfFont)      , "sans"        , settings$pdfFont)
        useDataSetObject <<- ifelse(is.null(settings$useDataSetObject), FALSE, settings$useDataSetObject)
        useEmbededDevice <<- ifelse(is.null(settings$useEmbededDevice), FALSE, settings$useEmbededDevice)
        runPlot          <<- ifelse(is.null(settings$runPlot),          FALSE, settings$runPlot)
        codebookOff      <<- ifelse(is.null(settings$codebookOff),      FALSE, settings$codebookOff)
        popupOff         <<- ifelse(is.null(settings$popupOff),         FALSE, settings$popupOff)
      } else {
        # initialize settings
        themesFolder <<- file.path(RzPath, "themes")
        if(grepl("linux", R.Version()$os)){
          theme      <<- "Default"
        } else {
          theme      <<- "kde42-oxygen"          
        }
        if(grepl("darwin", R.Version()$os)){
          globalFont <<- "Arial 10"
          variableViewFont <<- "Arial 10"
          plotFont <<- "Arial 10"
        } else {
          globalFont <<- "sans 10"
          variableViewFont <<- "sans 10"
          plotFont <<- "sans 10"
        }
        monospaceFont <<- "monospace 10"
        monospaceFontFamily <<- pangoFontDescriptionFromString(monospaceFont)$getFamily()
        plotFontFamily      <<- pangoFontDescriptionFromString(plotFont)$getFamily()
        psFont <<- "sans"
        pdfFont <<- "sans"
        useDataSetObject <<- FALSE
        useEmbededDevice <<- FALSE
        runPlot         <<- FALSE
        codebookOff      <<- FALSE
        popupOff         <<- FALSE
      }
      theme.path  <- file.path(themesFolder, theme, "gtk-2.0", "gtkrc")
      theme.path2 <- file.path(RzPath, "themes", theme, "gtk-2.0", "gtkrc")
      if(file.exists(theme.path)){
        gtkRcParse(theme.path)
      } else if(file.exists(theme.path2)){
        gtkRcParse(theme.path2)        
      }
      
      plotViewEnabled <<- FALSE
      variableEditorViewEnabled <<- FALSE
    },
    
    runDialog = function(win){
      dialog <- gtkDialogNewWithButtons(gettext("Settings"), win,
                                        c("modal", "destroy-with-parent"), 
                                        "gtk-ok", GtkResponseType["accept"], 
                                        "gtk-cancel", GtkResponseType["reject"],
                                        show=FALSE)
      
      themes.folder.label  <- gtkLabelNew(gettext("Themes Folder"))
      themes.folder.button <- gtkFileChooserButtonNew(gettext("Themes Folder"), GtkFileChooserAction["select-folder"])
      themes.folder.button$setCurrentFolder(themesFolder)
      themes.folder.hbox <- gtkHBoxNew(spacing=5)
      themes.folder.hbox$packStart(themes.folder.label, expand=FALSE)
      themes.folder.hbox$packStart(themes.folder.button)
      
      themes.label <- gtkLabelNew(gettext("Theme (requires restart R)"))
      themesCombo  <- gtkComboBoxNewText()
      themesCombo$getCells()[[1]]$setAlignment(0.5, 0.5)
      themes  <- sapply(list.dirs(file.path(RzPath, "themes"), recursive=FALSE), basename)
      if(nzchar(themesFolder)){
        themes2 <- sapply(list.dirs(themesFolder, recursive=FALSE), basename)
        themes  <- unique(c(themes, themes2))        
      }
      for(i in themes) themesCombo$appendText(i)
      themesCombo$setActive(which(theme==themes) - 1)
      themes.hbox <- gtkHBoxNew(spacing=5)
      themes.hbox$packStart(themes.label, expand=FALSE)
      themes.hbox$packStart(themesCombo)
      
      checkButtonUseDataSet <- gtkCheckButtonNewWithLabel(gettext("Sync as data.set object"))
      checkButtonUseDataSet$setActive(useDataSetObject)
      checkButtonUseEmbededDevice <- gtkCheckButtonNewWithLabel(gettext("Use embeded graphics divice (requires cairoDevice package)"))
      checkButtonUseEmbededDevice$setActive(useEmbededDevice)
      checkButtonRunPlot <- gtkCheckButtonNewWithLabel(gettext("Plot when a index cell is double-clicked"))
      checkButtonRunPlot$setActive(runPlot)
      checkButtonCodebookOff <- gtkCheckButtonNewWithLabel(gettext("Don't output summary while plot view open"))
      checkButtonCodebookOff$setActive(codebookOff)
      checkButtonPopupOff <- gtkCheckButtonNewWithLabel(gettext("Don't Popup Summary"))
      checkButtonPopupOff$setActive(popupOff)
      
      general.tab <- gtkVBoxNew()
      general.tab["border-width"] <- 2
      general.tab$packStart(themes.folder.hbox, expand=FALSE)
      general.tab$packStart(themes.hbox, expand=FALSE)
      general.tab$packStart(checkButtonUseDataSet, expand=FALSE)
      general.tab$packStart(checkButtonUseEmbededDevice, expand=FALSE)
      general.tab$packStart(checkButtonRunPlot, expand=FALSE)
      general.tab$packStart(checkButtonCodebookOff, expand=FALSE)
      general.tab$packStart(checkButtonPopupOff, expand=FALSE)
      
      
      rzFontSettingWidget1 <- new("RzFontSettingWidget", title = gettext("Global Font"), fontName = globalFont, showSize = TRUE, showStyle=TRUE)
      rzFontSettingWidget4 <- new("RzFontSettingWidget", title = gettext("Variable View Font"), fontName = variableViewFont, showSize = TRUE, showStyle=TRUE)
      rzFontSettingWidget2 <- new("RzFontSettingWidget", title = gettext("Monospace Font"), fontName = monospaceFont, showSize = TRUE, showStyle=TRUE)
      rzFontSettingWidget3 <- new("RzFontSettingWidget", title = gettext("Plot Font"), fontName = plotFont, showSize = FALSE, showStyle=FALSE)
      
      pdf.font.label <- gtkLabelNew(gettext("PDF Font"))
      pdfFontCombo <- gtkComboBoxNewText()
      pdfFontCombo$getCells()[[1]]$setAlignment(0.5, 0.5)
      for(i in names(pdfFonts())) pdfFontCombo$appendText(i)
      pdfFontCombo$setActive(which(pdfFont==names(pdfFonts())) - 1)
      
      ps.font.label <- gtkLabelNew(gettext("PostScript Font"))
      psFontCombo <- gtkComboBoxNewText()
      psFontCombo$getCells()[[1]]$setAlignment(0.5, 0.5)
      for(i in names(postscriptFonts())) psFontCombo$appendText(i)
      psFontCombo$setActive(which(psFont==names(postscriptFonts())) - 1)
      
      pdffont.hbox <- gtkHBoxNew(spacing=5)
      pdffont.hbox$packStart(pdf.font.label, expand=FALSE)
      pdffont.hbox$packStart(pdfFontCombo)
      psfont.hbox <- gtkHBoxNew(spacing=5)
      psfont.hbox$packStart(ps.font.label, expand=FALSE)
      psfont.hbox$packStart(psFontCombo)
      
      font.tab <- gtkVBoxNew(spacing=2)
      font.tab["border-width"] <- 2
      font.tab$packStart(rzFontSettingWidget1$getFontBox(), fill=FALSE, expand=FALSE)
      font.tab$packStart(rzFontSettingWidget4$getFontBox(), fill=FALSE, expand=FALSE)
      font.tab$packStart(rzFontSettingWidget2$getFontBox(), fill=FALSE, expand=FALSE)
      font.tab$packStart(rzFontSettingWidget3$getFontBox(), fill=FALSE, expand=FALSE)
      font.tab$packStart(pdffont.hbox, fill=FALSE, expand=FALSE)
      font.tab$packStart(psfont.hbox, fill=FALSE, expand=FALSE)
      
      note <- gtkNotebookNew()
      note$appendPage(general.tab, gtkLabelNew(gettext("General")))
      note$appendPage(font.tab, gtkLabelNew(gettext("Font")))
      dialog[["vbox"]]$packStart(note)
      
      onResponse <- function(dialog, response.id){
        if(response.id == GtkResponseType["accept"]) {
          themesFolder     <<- normalizePath(localize(themes.folder.button$getCurrentFolder()), "/")
          theme            <<- localize(themesCombo$getActiveText())
          globalFont       <<- localize(rzFontSettingWidget1$getFontName())
          variableViewFont <<- localize(rzFontSettingWidget4$getFontName())
          monospaceFont    <<- localize(rzFontSettingWidget2$getFontName())
          monospaceFontFamily <<- pangoFontDescriptionFromString(monospaceFont)$getFamily()
          plotFont         <<- localize(rzFontSettingWidget3$getFontName())
          plotFontFamily   <<- pangoFontDescriptionFromString(plotFont)$getFamily()
          psFont           <<- localize(psFontCombo$getActiveText())
          pdfFont          <<- localize(pdfFontCombo$getActiveText())
          useDataSetObject <<- checkButtonUseDataSet$getActive()
          useEmbededDevice <<- checkButtonUseEmbededDevice$getActive()
          runPlot          <<- checkButtonRunPlot$getActive()
          codebookOff      <<- checkButtonCodebookOff$getActive()
          popupOff         <<- checkButtonPopupOff$getActive()
          settings <- gtkSettingsGetDefault()
          settings$setStringProperty("gtk-font-name", rzSettings$getGlobalFont(), NULL)
          con <- file(path, open="w")
          dput(list(
            themesFolder     = themesFolder,
            theme            = theme,
            globalFont       = globalFont,
            variableViewFont = variableViewFont,
            monospaceFont    = monospaceFont,
            plotFont         = plotFont,
            psFont           = psFont,
            pdfFont          = pdfFont,
            useDataSetObject = useDataSetObject,
            useEmbededDevice = useEmbededDevice,
            runPlot          = runPlot,
            codebookOff      = codebookOff,
            popupOff         = popupOff
            ),
               file=con, control=NULL)
          close(con)
          dialog$hide()
        } else {
          dialog$hide()
        }
      }
      gSignalConnect(dialog, "response", onResponse)
      
      dialog$run()
    }
  )
)
settings$accessors(c("RzPath", "themesFolder", "globalFont", "variableViewFont", "monospaceFont", "monospaceFontFamily", "plotFont", "plotFontFamily",
                     "useDataSetObject","useEmbededDevice", "embededDeviceOn", "runPlot", "codebookOff", "popupOff",
                     "plotViewEnabled", "variableEditorViewEnabled", "psFont", "pdfFont"))

