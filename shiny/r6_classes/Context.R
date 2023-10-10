Context <- R6Class(
  'Context',
  portable = FALSE,
  class = FALSE,
  public = list(
    id = character(0),
    .reactId = character(0),
    .reactType = "other",
    .label = character(0),      # For debug purposes
    .invalidated = FALSE,
    .invalidateCallbacks = list(),
    .flushCallbacks = list(),
    .domain = NULL,
    .pid = NULL,
    .weak = NULL,

    initialize = function(
    domain, label='', type='other', prevId='',
    reactId = rLog$noReactId,
    id = .getReactiveEnvironment()$nextId(), # For dummy context
    weak = FALSE
    ) {
      id <<- id
      .label <<- label
      .domain <<- domain
      .pid <<- processId()
      .reactId <<- reactId
      .reactType <<- type
      .weak <<- weak
      rLog$createContext(id, label, type, prevId, domain)
    },
    run = function(func) {
      "Run the provided function under this context."

      promises::with_promise_domain(reactivePromiseDomain(), {
        withReactiveDomain(.domain, {
          env <- .getReactiveEnvironment()
          rLog$enter(.reactId, id, .reactType, .domain)
          on.exit(rLog$exit(.reactId, id, .reactType, .domain), add = TRUE)
          env$runWith(self, func)
        })
      })
    },
    invalidate = function() {
      "Invalidate this context. It will immediately call the callbacks
        that have been registered with onInvalidate()."

      if (!identical(.pid, processId())) {
        rlang::abort("Reactive context was created in one process and invalidated from another.")
      }

      if (.invalidated)
        return()
      .invalidated <<- TRUE

      rLog$invalidateStart(.reactId, id, .reactType, .domain)
      on.exit(rLog$invalidateEnd(.reactId, id, .reactType, .domain), add = TRUE)

      lapply(.invalidateCallbacks, function(func) {
        func()
      })
      .invalidateCallbacks <<- list()
      NULL
    },
    onInvalidate = function(func) {
      "Register a function to be called when this context is invalidated.
        If this context is already invalidated, the function is called
        immediately."

      if (!identical(.pid, processId())) {
        rlang::abort("Reactive context was created in one process and accessed from another.")
      }

      if (.invalidated)
        func()
      else
        .invalidateCallbacks <<- c(.invalidateCallbacks, func)
      NULL
    },
    addPendingFlush = function(priority) {
      "Tell the reactive environment that this context should be flushed the
        next time flushReact() called."
      .getReactiveEnvironment()$addPendingFlush(self, priority)
    },
    onFlush = function(func) {
      "Register a function to be called when this context is flushed."
      .flushCallbacks <<- c(.flushCallbacks, func)
    },
    executeFlushCallbacks = function() {
      "For internal use only."

      lapply(.flushCallbacks, function(flushCallback) {
        flushCallback()
      })
    },
    isWeak = function() {
      .weak
    }
  )
)
