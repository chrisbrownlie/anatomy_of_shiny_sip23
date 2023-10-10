
ReactiveEnvironment <- R6Class(
  'ReactiveEnvironment',
  portable = FALSE,
  class = FALSE,
  public = list(
    .currentContext = NULL,
    .nextId = 0L,
    .pendingFlush = 'PriorityQueue',
    .inFlush = FALSE,

    initialize = function() {
      .pendingFlush <<- PriorityQueue$new()
    },
    nextId = function() {
      .nextId <<- .nextId + 1L
      return(as.character(.nextId))
    },
    currentContext = function() {
      if (is.null(.currentContext)) {
        if (isTRUE(getOption('shiny.suppressMissingContextError'))) {
          return(getDummyContext())
        } else {
          rlang::abort(c(
            'Operation not allowed without an active reactive context.',
            paste0(
              'You tried to do something that can only be done from inside a ',
              'reactive consumer.'
            )
          ))
        }
      }
      return(.currentContext)
    },
    runWith = function(ctx, contextFunc) {
      old.ctx <- .currentContext
      .currentContext <<- ctx
      on.exit(.currentContext <<- old.ctx)
      contextFunc()
    },
    addPendingFlush = function(ctx, priority) {
      .pendingFlush$enqueue(ctx, priority)
    },
    hasPendingFlush = function() {
      return(!.pendingFlush$isEmpty())
    },
    # Returns TRUE if anything was actually called
    flush = function() {
      # If nothing to flush, exit early
      if (!hasPendingFlush()) return(invisible(FALSE))
      # If already in a flush, don't start another one
      if (.inFlush) return(invisible(FALSE))
      .inFlush <<- TRUE
      on.exit({
        .inFlush <<- FALSE
        rLog$idle(domain = NULL)
      })

      while (hasPendingFlush()) {
        ctx <- .pendingFlush$dequeue()
        ctx$executeFlushCallbacks()
      }

      invisible(TRUE)
    }
  )
)
