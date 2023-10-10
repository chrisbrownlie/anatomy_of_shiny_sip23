Observable <- R6Class(
  'Observable',
  portable = FALSE,
  public = list(
    .reactId = character(0),
    .origFunc = 'function',
    .func = 'function',
    .label = character(0),
    .domain = NULL,
    .dependents = 'Dependents',
    .invalidated = logical(0),
    .running = logical(0),
    .value = NULL,
    .error = FALSE,
    .visible = logical(0),
    .execCount = integer(0),
    .mostRecentCtxId = character(0),
    .ctx = 'Context',

    initialize = function(func, label = deparse(substitute(func)),
                          domain = getDefaultReactiveDomain(),
                          ..stacktraceon = TRUE) {
      if (length(formals(func)) > 0)
        rlang::abort(c(
          "Can't make a reactive expression from a function that takes arguments.",
          "Only functions without parameters can become reactive expressions."
        ))

      # This is to make sure that the function labels that show in the profiler
      # and in stack traces doesn't contain whitespace. See
      # https://github.com/rstudio/profvis/issues/58
      if (grepl("\\s", label, perl = TRUE)) {
        funcLabel <- "<reactive>"
      } else {
        funcLabel <- paste0("<reactive:", label, ">")
      }

      .reactId <<- nextGlobalReactId()
      .origFunc <<- func
      .func <<- wrapFunctionLabel(func, funcLabel,
                                  ..stacktraceon = ..stacktraceon)
      .label <<- label
      .domain <<- domain
      .dependents <<- Dependents$new(reactId = .reactId)
      .invalidated <<- TRUE
      .running <<- FALSE
      .execCount <<- 0L
      .mostRecentCtxId <<- ""
      .ctx <<- NULL
      rLog$define(.reactId, .value, .label, type = "observable", .domain)
    },
    getValue = function() {
      .dependents$register()

      if (.invalidated || .running) {
        ..stacktraceoff..(
          self$.updateValue()
        )
      }

      if (.error) {
        stop(.value)
      }

      if (.visible)
        .value
      else
        invisible(.value)
    },
    format = function() {
      simpleExprToFunction(fn_body(.origFunc), "reactive")
    },
    .updateValue = function() {
      ctx <- Context$new(.domain, .label, type = 'observable',
                         prevId = .mostRecentCtxId, reactId = .reactId,
                         weak = TRUE)
      .mostRecentCtxId <<- ctx$id

      # A Dependency object will have a weak reference to the context, which
      # doesn't prevent it from being GC'd. However, as long as this
      # Observable object is reachable and not invalidated, we need to make
      # sure the context isn't GC'd. To do that we need a strong reference to
      # the context.
      .ctx <<- ctx

      ctx$onInvalidate(function() {
        .invalidated <<- TRUE
        .value <<- NULL # Value can be GC'd, it won't be read once invalidated
        .dependents$invalidate(log = FALSE)
        .ctx <<- NULL   # No longer need to prevent the context from being GC'd.
      })
      .execCount <<- .execCount + 1L

      .invalidated <<- FALSE

      wasRunning <- .running
      .running <<- TRUE
      on.exit(.running <<- wasRunning)

      ctx$run(function() {
        result <- withCallingHandlers(

          {
            .error <<- FALSE
            withVisible(.func())
          },

          error = function(cond) {
            # If an error occurs, we want to propagate the error, but we also
            # want to save a copy of it, so future callers of this reactive will
            # get the same error (i.e. the error is cached).
            .value <<- cond
            .error <<- TRUE
            .visible <<- FALSE
          }
        )
        .value <<- result$value
        .visible <<- result$visible
      })
    }
  )
)
