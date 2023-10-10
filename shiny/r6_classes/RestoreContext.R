
RestoreContext <- R6Class("RestoreContext",
                          public = list(
                            # This will be set to TRUE if there's actually a state to restore
                            active = FALSE,

                            # This is set to an error message string in case there was an initialization
                            # error. Later, after the app has started on the client, the server can send
                            # this message as a notification on the client.
                            initErrorMessage = NULL,

                            # This is a RestoreInputSet for input values. This is a key-value store with
                            # some special handling.
                            input = NULL,

                            # Directory for extra files, if restoring from state that was saved to disk.
                            dir = NULL,

                            # For values other than input values. These values don't need the special
                            # phandling that's needed for input values, because they're only accessed
                            # from the onRestore function.
                            values = NULL,

                            initialize = function(queryString = NULL) {
                              self$reset() # Need this to initialize self$input

                              if (!is.null(queryString) && nzchar(queryString)) {
                                tryCatch(
                                  withLogErrors({
                                    qsValues <- parseQueryString(queryString, nested = TRUE)

                                    if (!is.null(qsValues[["__subapp__"]]) && qsValues[["__subapp__"]] == 1) {
                                      # Ignore subapps in shiny docs
                                      self$reset()

                                    } else if (!is.null(qsValues[["_state_id_"]]) && nzchar(qsValues[["_state_id_"]])) {
                                      # If we have a "_state_id_" key, restore from saved state and
                                      # ignore other key/value pairs. If not, restore from key/value
                                      # pairs in the query string.
                                      self$active <- TRUE
                                      private$loadStateQueryString(queryString)

                                    } else {
                                      # The query string contains the saved keys and values
                                      self$active <- TRUE
                                      private$decodeStateQueryString(queryString)
                                    }
                                  }),
                                  error = function(e) {
                                    # If there's an error in restoring problem, just reset these values
                                    self$reset()
                                    self$initErrorMessage <- e$message
                                    warning(e$message)
                                  }
                                )
                              }
                            },

                            reset = function() {
                              self$active <- FALSE
                              self$initErrorMessage <- NULL
                              self$input <- RestoreInputSet$new(list())
                              self$values <- new.env(parent = emptyenv())
                              self$dir <- NULL
                            },

                            # Completely replace the state
                            set = function(active = FALSE, initErrorMessage = NULL, input = list(), values = list(), dir = NULL) {
                              # Validate all inputs
                              stopifnot(is.logical(active))
                              stopifnot(is.null(initErrorMessage) || is.character(initErrorMessage))
                              stopifnot(is.list(input))
                              stopifnot(is.list(values))
                              stopifnot(is.null(dir) || is.character(dir))

                              self$active <- active
                              self$initErrorMessage <- initErrorMessage
                              self$input <- RestoreInputSet$new(input)
                              self$values <- list2env2(values, parent = emptyenv())
                              self$dir <- dir
                            },

                            # This should be called before a restore context is popped off the stack.
                            flushPending = function() {
                              self$input$flushPending()
                            },


                            # Returns a list representation of the RestoreContext object. This is passed
                            # to the app author's onRestore function. An important difference between
                            # the RestoreContext object and the list is that the former's `input` field
                            # is a RestoreInputSet object, while the latter's `input` field is just a
                            # list.
                            asList = function() {
                              list(
                                input = self$input$asList(),
                                dir = self$dir,
                                values = self$values
                              )
                            }
                          ),

                          private = list(
                            # Given a query string with a _state_id_, load saved state with that ID.
                            loadStateQueryString = function(queryString) {
                              values <- parseQueryString(queryString, nested = TRUE)
                              id <- values[["_state_id_"]]

                              # Check that id has only alphanumeric chars
                              if (grepl("[^a-zA-Z0-9]", id)) {
                                stop("Invalid state id: ", id)
                              }

                              # This function is passed to the loadInterface function; given a
                              # directory, it will load state from that directory
                              loadFun <- function(stateDir) {
                                self$dir <- stateDir

                                if (!dirExists(stateDir)) {
                                  stop("Bookmarked state directory does not exist.")
                                }

                                tryCatch({
                                  inputValues <- readRDS(file.path(stateDir, "input.rds"))
                                  self$input <- RestoreInputSet$new(inputValues)
                                },
                                error = function(e) {
                                  stop("Error reading input values file.")
                                }
                                )

                                valuesFile <- file.path(stateDir, "values.rds")
                                if (file.exists(valuesFile)) {
                                  tryCatch({
                                    self$values <- readRDS(valuesFile)
                                  },
                                  error = function(e) {
                                    stop("Error reading values file.")
                                  }
                                  )
                                }
                              }

                              # Look for a load.interface function. This will be defined by the hosting
                              # environment if it supports bookmarking.
                              loadInterface <- getShinyOption("load.interface", default = NULL)

                              if (is.null(loadInterface)) {
                                if (inShinyServer()) {
                                  # We're in a version of Shiny Server/Connect that doesn't have
                                  # bookmarking support.
                                  loadInterface <- function(id, callback) {
                                    stop("The hosting environment does not support saved-to-server bookmarking.")
                                  }

                                } else {
                                  # We're running Shiny locally.
                                  loadInterface <- loadInterfaceLocal
                                }
                              }

                              loadInterface(id, loadFun)

                              invisible()
                            },

                            # Given a query string with values encoded in it, restore saved state
                            # from those values.
                            decodeStateQueryString = function(queryString) {
                              # Remove leading '?'
                              if (substr(queryString, 1, 1) == '?')
                                queryString <- substr(queryString, 2, nchar(queryString))

                              # The "=" after "_inputs_" is optional. Shiny doesn't generate URLs with
                              # "=", but httr always adds "=".
                              inputs_reg <- "(^|&)_inputs_=?(&|$)"
                              values_reg <- "(^|&)_values_=?(&|$)"

                              # Error if multiple '_inputs_' or '_values_'. This is needed because
                              # strsplit won't add an entry if the search pattern is at the end of a
                              # string.
                              if (length(gregexpr(inputs_reg, queryString)[[1]]) > 1)
                                stop("Invalid state string: more than one '_inputs_' found")
                              if (length(gregexpr(values_reg, queryString)[[1]]) > 1)
                                stop("Invalid state string: more than one '_values_' found")

                              # Look for _inputs_ and store following content in inputStr
                              splitStr <- strsplit(queryString, inputs_reg)[[1]]
                              if (length(splitStr) == 2) {
                                inputStr <- splitStr[2]
                                # Remove any _values_ (and content after _values_) that may come after
                                # _inputs_
                                inputStr <- strsplit(inputStr, values_reg)[[1]][1]

                              } else {
                                inputStr <- ""
                              }

                              # Look for _values_ and store following content in valueStr
                              splitStr <- strsplit(queryString, values_reg)[[1]]
                              if (length(splitStr) == 2) {
                                valueStr <- splitStr[2]
                                # Remove any _inputs_ (and content after _inputs_) that may come after
                                # _values_
                                valueStr <- strsplit(valueStr, inputs_reg)[[1]][1]

                              } else {
                                valueStr <- ""
                              }


                              inputs <- parseQueryString(inputStr, nested = TRUE)
                              values <- parseQueryString(valueStr, nested = TRUE)

                              valuesFromJSON <- function(vals) {
                                varsUnparsed <- c()
                                valsParsed <- mapply(names(vals), vals, SIMPLIFY = FALSE,
                                                     FUN = function(name, value) {
                                                       tryCatch(
                                                         safeFromJSON(value),
                                                         error = function(e) {
                                                           varsUnparsed <<- c(varsUnparsed, name)
                                                           warning("Failed to parse URL parameter \"", name, "\"")
                                                         }
                                                       )
                                                     }
                                )
                                valsParsed[varsUnparsed] <- NULL
                                valsParsed
                              }

                              inputs <- valuesFromJSON(inputs)
                              self$input <- RestoreInputSet$new(inputs)

                              values <- valuesFromJSON(values)
                              self$values <- list2env2(values, self$values)
                            }
                          )
)
