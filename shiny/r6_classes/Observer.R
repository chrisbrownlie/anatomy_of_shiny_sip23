Observer <- R6Class(
  'Observer',
  portable = FALSE,
  public = list(
    .reactId = character(0),
    .func = 'function',
    .label = character(0),
    .domain = 'ANY',
    .priority = numeric(0),
    .autoDestroy = logical(0),
    # A function that, when invoked, unsubscribes the autoDestroy
    # listener (or NULL if autodestroy is disabled for this observer).
    # We must unsubscribe when this observer is destroyed, or else
    # the observer cannot be garbage collected until the session ends.
    .autoDestroyHandle = 'ANY',
    .invalidateCallbacks = list(),
    .execCount = integer(0),
    .onResume = 'function',
    .suspended = logical(0),
    .destroyed = logical(0),
    .prevId = character(0),
    .ctx = NULL,

    initialize = function(observerFunc, label, suspended = FALSE, priority = 0,
                          domain = getDefaultReactiveDomain(),
                          autoDestroy = TRUE, ..stacktraceon = TRUE) {
      if (length(formals(observerFunc)) > 0)
        rlang::abort(c(
          "Can't make an observer from a function that takes arguments.",
          "Only functions without arguments can become observers."
        ))
      if (grepl("\\s", label, perl = TRUE)) {
        funcLabel <- "<observer>"
      } else {
        funcLabel <- paste0("<observer:", label, ">")
      }
      .func <<- wrapFunctionLabel(observerFunc, funcLabel, ..stacktraceon = ..stacktraceon)
      .label <<- label
      .domain <<- domain
      .priority <<- normalizePriority(priority)
      .execCount <<- 0L
      .suspended <<- suspended
      .onResume <<- function() NULL
      .destroyed <<- FALSE
      .prevId <<- ''

      .autoDestroy <<- FALSE
      .autoDestroyHandle <<- NULL
      setAutoDestroy(autoDestroy)

      .reactId <<- nextGlobalReactId()
      rLog$defineObserver(.reactId, .label, .domain)

      # Defer the first running of this until flushReact is called
      .createContext()$invalidate()
    },
    .createContext = function() {
      ctx <- Context$new(.domain, .label, type='observer', prevId=.prevId, reactId = .reactId)
      .prevId <<- ctx$id

      if (!is.null(.ctx)) {
        # If this happens, something went wrong.
        warning("Created a new context without invalidating previous context.")
      }
      # Store the context explicitly in the Observer object. This is necessary
      # to make sure that when the observer is destroyed, it also gets
      # invalidated. Otherwise the upstream reactive (on which the observer
      # depends) will hold a (indirect) reference to this context until the
      # reactive is invalidated, which may not happen immediately or at all.
      # This can lead to a memory leak (#1253).
      .ctx <<- ctx

      ctx$onInvalidate(function() {
        # Context is invalidated, so we don't need to store a reference to it
        # anymore.
        .ctx <<- NULL

        lapply(.invalidateCallbacks, function(invalidateCallback) {
          invalidateCallback()
          NULL
        })

        continue <- function() {
          ctx$addPendingFlush(.priority)
          if (!is.null(.domain)) {
            .domain$incrementBusyCount()
          }
        }

        if (.suspended == FALSE)
          continue()
        else
          .onResume <<- continue
      })

      ctx$onFlush(function() {

        hybrid_chain(
          {
            if (!.destroyed) {
              shinyCallingHandlers(run())
            }
          },
          catch = function(e) {
            # It's OK for shiny.silent.error errors to cause an observer to stop running
            # shiny.silent.error = function(e) NULL
            # validation = function(e) NULL,
            # shiny.output.cancel = function(e) NULL

            if (cnd_inherits(e, "shiny.silent.error")) {
              return()
            }

            printError(e)
            if (!is.null(.domain)) {
              .domain$unhandledError(e)
            }
          },
          finally = .domain$decrementBusyCount
        )
      })

      return(ctx)
    },
    run = function() {
      ctx <- .createContext()
      .execCount <<- .execCount + 1L
      ctx$run(.func)
    },
    onInvalidate = function(callback) {
      "Register a callback function to run when this observer is invalidated.
      No arguments will be provided to the callback function when it is
      invoked."
      .invalidateCallbacks <<- c(.invalidateCallbacks, callback)
    },
    setPriority = function(priority = 0) {
      "Change this observer's priority. Note that if the observer is currently
      invalidated, then the change in priority will not take effect until the
      next invalidation--unless the observer is also currently suspended, in
      which case the priority change will be effective upon resume."
      .priority <<- normalizePriority(priority)
    },
    setAutoDestroy = function(autoDestroy) {
      "Sets whether this observer should be automatically destroyed when its
      domain (if any) ends. If autoDestroy is TRUE and the domain already
      ended, then destroy() is called immediately."

      if (.autoDestroy == autoDestroy) {
        return(.autoDestroy)
      }

      oldValue <- .autoDestroy
      .autoDestroy <<- autoDestroy

      if (autoDestroy) {
        if (!.destroyed && !is.null(.domain)) { # Make sure to not try to destroy twice.
          if (.domain$isEnded()) {
            destroy()
          } else {
            .autoDestroyHandle <<- onReactiveDomainEnded(.domain, .onDomainEnded)
          }
        }
      } else {
        if (!is.null(.autoDestroyHandle))
          .autoDestroyHandle()
        .autoDestroyHandle <<- NULL
      }

      invisible(oldValue)
    },
    suspend = function() {
      "Causes this observer to stop scheduling flushes (re-executions) in
      response to invalidations. If the observer was invalidated prior to this
      call but it has not re-executed yet (because it waits until onFlush is
      called) then that re-execution will still occur, because the flush is
      already scheduled."
      .suspended <<- TRUE
    },
    resume = function() {
      "Causes this observer to start re-executing in response to invalidations.
      If the observer was invalidated while suspended, then it will schedule
      itself for re-execution (pending flush)."
      if (.suspended) {
        .suspended <<- FALSE
        .onResume()
        .onResume <<- function() NULL
      }
      invisible()
    },
    destroy = function() {
      "Prevents this observer from ever executing again (even if a flush has
      already been scheduled)."

      # Make sure to not try to destory twice.
      if (.destroyed)
        return()

      suspend()
      .destroyed <<- TRUE

      if (!is.null(.autoDestroyHandle)) {
        .autoDestroyHandle()
      }
      .autoDestroyHandle <<- NULL

      if (!is.null(.ctx)) {
        .ctx$invalidate()
      }
    },
    .onDomainEnded = function() {
      if (isTRUE(.autoDestroy)) {
        destroy()
      }
    }
  )
)
