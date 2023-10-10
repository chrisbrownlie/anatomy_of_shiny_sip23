ShinySaveState <- R6Class("ShinySaveState",
                          public = list(
                            input = NULL,
                            exclude = NULL,
                            onSave = NULL, # A callback to invoke during the saving process.

                            # These are set not in initialize(), but by external functions that modify
                            # the ShinySaveState object.
                            dir = NULL,


                            initialize = function(input = NULL, exclude = NULL, onSave = NULL) {
                              self$input   <- input
                              self$exclude <- exclude
                              self$onSave  <- onSave
                              private$values_  <- new.env(parent = emptyenv())
                            }
                          ),

                          active = list(
                            # `values` looks to the outside world like an environment for storing
                            # arbitrary values. Two things to note: (1) This is an environment (instead
                            # of, say, a list) because if the onSave function represents multiple
                            # callback functions (when onBookmark is called multiple times), each
                            # callback can change `values`, and if we used a list, one of the callbacks
                            # could easily obliterate values set by another. This can happen when using
                            # modules that have an onBookmark function. (2) The purpose of the active
                            # binding is to prevent replacing state$values with another arbitrary
                            # object. (Simply locking the binding would prevent all changes to
                            # state$values.)
                            values = function(value) {
                              if (missing(value))
                                return(private$values_)

                              if (identical(value, private$values_)) {
                                return(value)
                              } else {
                                stop("Items in `values` can be changed, but `values` itself cannot be replaced.")
                              }
                            }
                          ),

                          private = list(
                            values_ = NULL
                          )
)


# Save a state to disk. Returns a query string which can be used to restore the
# session.
saveShinySaveState <- function(state) {
  id <- createUniqueId(8)

  # A function for saving the state object to disk, given a directory to save
  # to.
  saveState <- function(stateDir) {
    state$dir <- stateDir

    # Allow user-supplied onSave function to do things like add state$values, or
    # save data to state dir.
    if (!is.null(state$onSave))
      isolate(state$onSave(state))

    # Serialize values, possibly saving some extra data to stateDir
    exclude <- c(state$exclude, "._bookmark_")
    inputValues <- serializeReactiveValues(state$input, exclude, state$dir)
    saveRDS(inputValues, file.path(stateDir, "input.rds"))

    # If values were added, save them also.
    if (length(state$values) != 0)
      saveRDS(state$values, file.path(stateDir, "values.rds"))
  }

  # Pass the saveState function to the save interface function, which will
  # invoke saveState after preparing the directory.

  # Look for a save.interface function. This will be defined by the hosting
  # environment if it supports bookmarking.
  saveInterface <- getShinyOption("save.interface", default = NULL)

  if (is.null(saveInterface)) {
    if (inShinyServer()) {
      # We're in a version of Shiny Server/Connect that doesn't have
      # bookmarking support.
      saveInterface <- function(id, callback) {
        stop("The hosting environment does not support saved-to-server bookmarking.")
      }

    } else {
      # We're running Shiny locally.
      saveInterface <- saveInterfaceLocal
    }
  }

  saveInterface(id, saveState)

  paste0("_state_id_=", encodeURIComponent(id))
}

# Encode the state to a URL. This does not save to disk.
encodeShinySaveState <- function(state) {
  exclude <- c(state$exclude, "._bookmark_")
  inputVals <- serializeReactiveValues(state$input, exclude, stateDir = NULL)

  # Allow user-supplied onSave function to do things like add state$values.
  if (!is.null(state$onSave))
    isolate(state$onSave(state))

  inputVals <- vapply(inputVals,
                      function(x) toJSON(x, strict_atomic = FALSE),
                      character(1),
                      USE.NAMES = TRUE
  )

  res <- ""

  # If any input values are present, add them.
  if (length(inputVals) != 0) {
    res <- paste0(res, "_inputs_&",
                  paste0(
                    encodeURIComponent(names(inputVals)),
                    "=",
                    encodeURIComponent(inputVals),
                    collapse = "&"
                  )
    )
  }

  # If 'values' is present, add them as well.
  if (length(state$values) != 0) {
    values <- vapply(state$values,
                     function(x) toJSON(x, strict_atomic = FALSE),
                     character(1),
                     USE.NAMES = TRUE
    )

    res <- paste0(res,
                  if (length(inputVals != 0)) "&",  # Add separator if there were inputs
                  "_values_&",
                  paste0(
                    encodeURIComponent(names(values)),
                    "=",
                    encodeURIComponent(values),
                    collapse = "&"
                  )
    )
  }

  res
}
