tools <-
setRefClass("RzTools",
  fields = c("variableView", "infoBar"),
  methods = list(
    initialize = function(...) {
      initFields(...)
      variableView <<- NULL
      infoBar      <<- NULL
    },
    
    clean = function(){
      variableView <<- NULL
      infoBar      <<- NULL
    }
  )
)
tools$accessors("variableView", "infoBar")
