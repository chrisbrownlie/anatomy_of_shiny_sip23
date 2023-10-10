ReactiveVal <- R6Class(
  'ReactiveVal',
  portable = FALSE,
  private = list(
    reactId = character(0),
    value = NULL,
    label = NULL,
    frozen = FALSE,
    dependents = NULL
  ),
  public = list(
    initialize = function(value, label = NULL) {
      reactId <- nextGlobalReactId()
      private$reactId <- reactId
      private$value <- value
      private$label <- label
      private$dependents <- Dependents$new(reactId = private$reactId)
      rLog$define(private$reactId, value, private$label, type = "reactiveVal", getDefaultReactiveDomain())
    },
    get = function() {
      private$dependents$register()

      if (private$frozen)
        reactiveStop()

      private$value
    },
    set = function(value) {
      if (identical(private$value, value)) {
        return(invisible(FALSE))
      }
      rLog$valueChange(private$reactId, value, getDefaultReactiveDomain())
      private$value <- value
      private$dependents$invalidate()
      invisible(TRUE)
    },
    freeze = function(session = getDefaultReactiveDomain()) {
      checkReactiveDomain(session)
      rLog$freezeReactiveVal(private$reactId, session)
      session$onFlushed(function() {
        self$thaw(session)
      })
      private$frozen <- TRUE
    },
    thaw = function(session = getDefaultReactiveDomain()) {
      rLog$thawReactiveVal(private$reactId, session)
      private$frozen <- FALSE
    },
    isFrozen = function() {
      private$frozen
    },
    format = function(...) {
      # capture.output(print()) is necessary because format() doesn't
      # necessarily return a character vector, e.g. data.frame.
      label <- utils::capture.output(print(base::format(private$value, ...)))
      if (length(label) == 1) {
        paste0("reactiveVal: ", label)
      } else {
        c("reactiveVal:", label)
      }
    }
  )
)
