MessageLogger = R6Class(
  "MessageLogger",
  portable = FALSE,
  public = list(
    depth = 0L,
    reactCache = list(),
    option = "shiny.reactlog.console",

    initialize = function(option = "shiny.reactlog.console", depth = 0L) {
      if (!missing(depth)) self$depth <- depth
      if (!missing(option)) self$option <- option
    },

    isLogging = function() {
      isTRUE(getOption(self$option))
    },
    isNotLogging = function() {
      ! isTRUE(getOption(self$option))
    },
    depthIncrement = function() {
      if (self$isNotLogging()) return(NULL)
      self$depth <- self$depth + 1L
    },
    depthDecrement = function() {
      if (self$isNotLogging()) return(NULL)
      self$depth <- self$depth - 1L
    },
    hasReact = function(reactId) {
      if (self$isNotLogging()) return(FALSE)
      !is.null(self$getReact(reactId))
    },
    getReact = function(reactId, force = FALSE) {
      if (identical(force, FALSE) && self$isNotLogging()) return(NULL)
      self$reactCache[[reactId]]
    },
    setReact = function(reactObj, force = FALSE) {
      if (identical(force, FALSE) && self$isNotLogging()) return(NULL)
      self$reactCache[[reactObj$reactId]] <- reactObj
    },
    shortenString = function(txt, n = 250) {
      if (is.null(txt) || isTRUE(is.na(txt))) {
        return("")
      }
      if (nchar(txt) > n) {
        return(
          paste0(substr(txt, 1, n - 3), "...")
        )
      }
      return(txt)
    },
    singleLine = function(txt) {
      gsub("([^\\])\\n", "\\1\\\\n", txt)
    },
    valueStr = function(valueStr) {
      paste0(
        " '",  self$shortenString(self$singleLine(valueStr)), "'"
      )
    },
    reactStr = function(reactId) {
      if (self$isNotLogging()) return(NULL)
      reactInfo <- self$getReact(reactId)
      if (is.null(reactInfo)) return(" <UNKNOWN_REACTID>")
      paste0(
        " ", reactInfo$reactId, ":'", self$shortenString(self$singleLine(reactInfo$label)), "'"
      )
    },
    typeStr = function(type = NULL) {
      self$ctxStr(ctxId = NULL, type = type)
    },
    ctxStr = function(ctxId = NULL, type = NULL) {
      if (self$isNotLogging()) return(NULL)
      self$ctxPrevCtxStr(ctxId = ctxId, prevCtxId = NULL, type = type)
    },
    ctxPrevCtxStr = function(ctxId = NULL, prevCtxId = NULL, type = NULL, preCtxIdTxt = " in ") {
      if (self$isNotLogging()) return(NULL)
      paste0(
        if (!is.null(ctxId)) paste0(preCtxIdTxt, ctxId),
        if (!is.null(prevCtxId)) paste0(" from ", prevCtxId),
        if (!is.null(type) && !identical(type, "other")) paste0(" - ", type)
      )
    },
    log = function(...) {
      if (self$isNotLogging()) return(NULL)
      msg <- paste0(
        paste0(rep("= ", depth), collapse = ""), "- ", paste0(..., collapse = ""),
        collapse = ""
      )
      message(msg)
    }
  )
)
