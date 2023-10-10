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
HandlerManager <- R6Class("HandlerManager",
                          portable = FALSE,
                          class = FALSE,
                          public = list(
                            handlers = "HandlerList",
                            wsHandlers = "HandlerList",

                            initialize = function() {
                              handlers <<- HandlerList$new()
                              wsHandlers <<- HandlerList$new()
                            },

                            addHandler = function(handler, key, tail = FALSE) {
                              handlers$add(handler, key, tail)
                            },
                            removeHandler = function(key) {
                              handlers$remove(key)
                            },
                            addWSHandler = function(wsHandler, key, tail = FALSE) {
                              wsHandlers$add(wsHandler, key, tail)
                            },
                            removeWSHandler = function(key) {
                              wsHandlers$remove(key)
                            },
                            clear = function() {
                              handlers$clear()
                              wsHandlers$clear()
                            },
                            createHttpuvApp = function() {
                              list(
                                onHeaders = function(req) {
                                  maxSize <- getOption('shiny.maxRequestSize') %||% (5 * 1024 * 1024)
                                  if (maxSize <= 0)
                                    return(NULL)

                                  reqSize <- 0
                                  if (length(req$CONTENT_LENGTH) > 0)
                                    reqSize <- as.numeric(req$CONTENT_LENGTH)
                                  else if (length(req$HTTP_TRANSFER_ENCODING) > 0)
                                    reqSize <- Inf

                                  if (reqSize > maxSize) {
                                    return(list(status = 413L,
                                                headers = list('Content-Type' = 'text/plain'),
                                                body = 'Maximum upload size exceeded'))
                                  }
                                  else {
                                    return(NULL)
                                  }
                                },
                                call = .httpServer(
                                  function (req) {
                                    hybrid_chain(
                                      hybrid_chain(
                                        withCallingHandlers(withLogErrors(handlers$invoke(req)),
                                                            error = function(cond) {
                                                              sanitizeErrors <- getOption('shiny.sanitize.errors', FALSE)
                                                              if (inherits(cond, 'shiny.custom.error') || !sanitizeErrors) {
                                                                stop(cond$message, call. = FALSE)
                                                              } else {
                                                                stop(paste("An error has occurred. Check your logs or",
                                                                           "contact the app author for clarification."),
                                                                     call. = FALSE)
                                                              }
                                                            }
                                        ),
                                        catch = function(err) {
                                          httpResponse(status = 500L,
                                                       content_type = "text/html; charset=UTF-8",
                                                       content = as.character(htmltools::htmlTemplate(
                                                         system_file("template", "error.html", package = "shiny"),
                                                         message = conditionMessage(err)
                                                       ))
                                          )
                                        }
                                      ),
                                      function(resp) {
                                        maybeInjectAutoreload(resp)
                                      }
                                    )
                                  },
                                  loadSharedSecret()
                                ),
                                onWSOpen = function(ws) {
                                  return(wsHandlers$invoke(ws))
                                }
                              )
                            },
                            .httpServer = function(handler, checkSharedSecret) {
                              filter <- getOption('shiny.http.response.filter')
                              if (is.null(filter))
                                filter <- function(req, response) response

                              function(req) {
                                if (!checkSharedSecret(req$HTTP_SHINY_SHARED_SECRET)) {
                                  return(list(status=403,
                                              body='<h1>403 Forbidden</h1><p>Shared secret mismatch</p>',
                                              headers=list('Content-Type' = 'text/html')))
                                }

                                # Catch HEAD requests. For the purposes of handler functions, they
                                # should be treated like GET. The difference is that they shouldn't
                                # return a body in the http response.
                                head_request <- FALSE
                                if (identical(req$REQUEST_METHOD, "HEAD")) {
                                  head_request <- TRUE
                                  req$REQUEST_METHOD <- "GET"
                                }

                                response <- handler(req)

                                res <- hybrid_chain(response, function(response) {
                                  if (is.null(response))
                                    response <- httpResponse(404, content="<h1>Not Found</h1>")

                                  if (inherits(response, "httpResponse")) {
                                    headers <- as.list(response$headers)
                                    headers$'Content-Type' <- response$content_type

                                    response <- filter(req, response)
                                    if (head_request) {

                                      headers$`Content-Length` <- getResponseContentLength(response, deleteOwnedContent = TRUE)

                                      return(list(
                                        status = response$status,
                                        body = "",
                                        headers = headers
                                      ))
                                    } else {
                                      return(list(
                                        status = response$status,
                                        body = response$content,
                                        headers = headers
                                      ))
                                    }

                                  } else {
                                    # Assume it's a Rook-compatible response
                                    return(response)
                                  }
                                })
                              }
                            }
                          )
)
