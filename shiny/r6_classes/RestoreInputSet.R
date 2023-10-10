# Restore input set. This is basically a key-value store, except for one
# important difference: When the user `get()`s a value, the value is marked as
# pending; when `flushPending()` is called, those pending values are marked as
# used. When a value is marked as used, `get()` will not return it, unless
# called with `force=TRUE`. This is to make sure that a particular value can be
# restored only within a single call to `withRestoreContext()`. Without this, if
# a value is restored in a dynamic UI, it could completely prevent any other
# (non- restored) kvalue from being used.
RestoreInputSet <- R6Class("RestoreInputSet",
                           private = list(
                             values = NULL,
                             pending = character(0),
                             used = character(0)     # Names of values which have been used
                           ),

                           public = list(
                             initialize = function(values) {
                               private$values <- list2env2(values, parent = emptyenv())
                             },

                             exists = function(name) {
                               exists(name, envir = private$values)
                             },

                             # Return TRUE if the value exists and has not been marked as used.
                             available = function(name) {
                               self$exists(name) && !self$isUsed(name)
                             },

                             isPending = function(name) {
                               name %in% private$pending
                             },

                             isUsed = function(name) {
                               name %in% private$used
                             },

                             # Get a value. If `force` is TRUE, get the value without checking whether
                             # has been used, and without marking it as pending.
                             get = function(name, force = FALSE) {
                               if (force)
                                 return(private$values[[name]])

                               if (!self$available(name))
                                 return(NULL)

                               # Mark this name as pending. Use unique so that it's not added twice.
                               private$pending <- unique(c(private$pending, name))
                               private$values[[name]]
                             },

                             # Take pending names and mark them as used, then clear pending list.
                             flushPending = function() {
                               private$used <- unique(c(private$used, private$pending))
                               private$pending <- character(0)
                             },

                             asList = function() {
                               as.list.environment(private$values, all.names = TRUE)
                             }
                           )
)
