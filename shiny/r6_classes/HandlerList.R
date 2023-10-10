#
# ## Handler manager
#
# The handler manager gives you a place to register handlers (of both http and
# websocket varieties) and provides an httpuv-compatible set of callbacks for
# invoking them.
#
# Create one of these, make zero or more calls to `addHandler` and
# `addWSHandler` methods (order matters--first one wins!), and then pass the
# return value of `createHttpuvApp` to httpuv's `startServer` function.
#
## ------------------------------------------------------------------------
HandlerList <- R6Class("HandlerList",
                       portable = FALSE,
                       class = FALSE,
                       public = list(
                         handlers = list(),

                         add = function(handler, key, tail = FALSE) {
                           if (!is.null(handlers[[key]]))
                             stop("Key ", key, " already in use")
                           newList <- structure(names=key, list(handler))

                           if (length(handlers) == 0)
                             handlers <<- newList
                           else if (tail)
                             handlers <<- c(handlers, newList)
                           else
                             handlers <<- c(newList, handlers)
                         },
                         remove = function(key) {
                           handlers[key] <<- NULL
                         },
                         clear = function() {
                           handlers <<- list()
                         },
                         invoke = function(...) {
                           for (handler in handlers) {
                             result <- handler(...)
                             if (!is.null(result))
                               return(result)
                           }
                           return(NULL)
                         }
                       )
)
