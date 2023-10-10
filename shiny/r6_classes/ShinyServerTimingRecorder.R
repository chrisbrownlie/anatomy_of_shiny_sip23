# Helper class for emitting log messages to stdout that will be interpreted by
# a Shiny Server parent process. The duration it's trying to record is the time
# between a websocket message being received, and the next flush to the client.
ShinyServerTimingRecorder <- R6Class("ShinyServerTimingRecorder",
                                     cloneable = FALSE,
                                     public = list(
                                       initialize = function() {
                                         private$shiny_stdout <- if (exists(".shiny__stdout", globalenv()))
                                           get(".shiny__stdout", globalenv())
                                         else
                                           NULL
                                         private$guid <- NULL
                                       },
                                       start = function(guid) {
                                         if (is.null(private$shiny_stdout)) return()

                                         private$guid <- guid
                                         if (!is.null(guid)) {
                                           private$write("n")
                                         }
                                       },
                                       stop = function() {
                                         if (is.null(private$shiny_stdout)) return()

                                         if (!is.null(private$guid)) {
                                           private$write("x")
                                           private$guid <- NULL
                                         }
                                       }
                                     ),
                                     private = list(
                                       shiny_stdout = NULL,
                                       guid = character(),
                                       write = function(code) {
                                         # eNter or eXit a flushReact
                                         writeLines(paste("_", code, "_flushReact ", private$guid,
                                                          " @ ", sprintf("%.3f", as.numeric(Sys.time())),
                                                          sep=""), con=private$shiny_stdout)
                                         flush(private$shiny_stdout)
                                       }
                                     )
)
