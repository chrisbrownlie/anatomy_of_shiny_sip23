Callbacks <- R6Class(
  'Callbacks',
  portable = FALSE,
  class = FALSE,
  public = list(
    .nextId = integer(0),
    .callbacks = 'Map',

    initialize = function() {
      # NOTE: we avoid using '.Machine$integer.max' directly
      # as R 3.3.0's 'radixsort' could segfault when sorting
      # an integer vector containing this value
      .nextId <<- as.integer(.Machine$integer.max - 1L)
      .callbacks <<- Map$new()
    },
    register = function(callback) {
      if (!is.function(callback)) {
        stop("callback must be a function")
      }
      id <- as.character(.nextId)
      .nextId <<- .nextId - 1L
      .callbacks$set(id, callback)
      return(function() {
        .callbacks$remove(id)
      })
    },
    invoke = function(..., onError=NULL, ..stacktraceon = FALSE) {
      # Ensure that calls are invoked in the order that they were registered
      keys <- as.character(sort(as.integer(.callbacks$keys()), decreasing = TRUE))
      callbacks <- .callbacks$mget(keys)

      for (callback in callbacks) {
        if (is.null(onError)) {
          if (..stacktraceon) {
            ..stacktraceon..(callback(...))
          } else {
            callback(...)
          }
        } else {
          tryCatch(
            captureStackTraces(
              if (..stacktraceon)
                ..stacktraceon..(callback(...))
              else
                callback(...)
            ),
            error = onError
          )
        }
      }
    },
    count = function() {
      .callbacks$size()
    }
  )
)
