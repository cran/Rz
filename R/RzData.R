rzdata <-
setRefClass("RzData",
  fields = c("file.path", "data.set.name", "original.name",
             "data.set", "original.data.set", "data.frame",
             "log",
             "system", "package.version", "encoding", "time"),
  methods = list(
    initialize        = function(...) {
      initFields(...)
#      data.set.name     <<- NULL
      original.data.set <<- data.set
      data.frame        <<- as.data.frame(data.set)
#      log               <<- NULL
      system            <<- R.Version()
      package.version   <<- list(Rz = packageVersion("Rz"),
                                 memisc=packageVersion("memisc"),
                                 RGtk2=packageVersion("RGtk2"))
      encoding          <<- localeToCharset()
      time              <<- Sys.time()
    },

    save = function(file){
      rzdata <- new.env()
      rzdata$file.path           <- file.path
      rzdata$data.set.name       <- data.set.name
      rzdata$original.name       <- original.name
      rzdata$data.set            <- data.set
#      rzdata$original.data.set   <- original.data.set
#      rzdata$data.frame          <- data.frame
      rzdata$log                 <- log
      rzdata$system              <- system
      rzdata$package.version     <- package.version
      rzdata$encoding            <- encoding
      rzdata$time                <- time
      base::save(rzdata, file=file)
    },

    revert = function(){
      data.set   <<- original.data.set
      data.frame <<- as.data.frame(data.set)
      .self$linkDataFrame()
    },

    reloadFromGlobalEnv = function(){
      data.set.tmp <- try(get(data.set.name, envir=.GlobalEnv), silent=TRUE)
      if(is.data.set(data.set.tmp)){
        data.set   <<- data.set.tmp
        data.frame <<- as.data.frame(data.set)
        .self$linkDataFrame()
        return(TRUE)
      } else {
        return(data.set.name)
      }
    },
    
    ncol              = function() { length(description(data.set)) },
    
    constructVariable = function(col){
      data.frame[[col]] <<- as.data.frame(data.set[[col]])[[1]]
      names(data.frame) <<- names(data.set)
    },
    
    linkDataFrame     = function(){
      if(rzSettings$getUseDataSetObject()){
        assign(data.set.name, data.set, envir=.GlobalEnv)
      } else {
        assign(data.set.name, data.frame, envir=.GlobalEnv)
      }
    },

    import = function(data){
      
    },

    # getter methods
    getVariableLabels = function() gsub("(^')|('$)", "", as.character(description(data.set))),
    
    getVariableNames  = function() names(description(data.set)),
    
    getMeasurement    = function() sapply(data.set, measurement),
    
    getMissingValues  = function() {
      miss.val  <- lapply(data.set, missing.values)
      miss.val  <- sapply(miss.val, function(x){
        if      (is.null(x))        return("")
        else if (is.null(x@filter)) return(sprintf("range=c(%s, %s)", x@range[1], x@range[2]))
        else                        return(paste(x@filter, collapse=","))
      })
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
rzdata$accessors(c("data.set.name", "original.name", "data.set", "data.frame", "file.path"))
