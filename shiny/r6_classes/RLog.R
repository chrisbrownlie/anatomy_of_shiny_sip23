RLog <- R6Class(
  "RLog",
  portable = FALSE,
  private = list(
    option = "shiny.reactlog",
    msgOption = "shiny.reactlog.console",

    appendEntry = function(domain, logEntry) {
      if (self$isLogging()) {
        sessionToken <- if (is.null(domain)) NULL else domain$token
        logStack$push(c(logEntry, list(
          session = sessionToken,
          time = as.numeric(Sys.time())
        )))
      }
      if (!is.null(domain)) domain$reactlog(logEntry)
    }
  ),
  public = list(
    msg = "<MessageLogger>",
    logStack = "<Stack>",

    noReactIdLabel = "NoCtxReactId",
    noReactId = reactIdStr("NoCtxReactId"),
    dummyReactIdLabel = "DummyReactId",
    dummyReactId = reactIdStr("DummyReactId"),

    asList = function() {
      ret <- self$logStack$as_list()
      attr(ret, "version") <- "1"
      ret
    },

    ctxIdStr = function(ctxId) {
      if (is.null(ctxId) || identical(ctxId, "")) return(NULL)
      paste0("ctx", ctxId)
    },
    namesIdStr = function(reactId) {
      paste0("names(", reactId, ")")
    },
    asListIdStr = function(reactId) {
      paste0("reactiveValuesToList(", reactId, ")")
    },
    asListAllIdStr = function(reactId) {
      paste0("reactiveValuesToList(", reactId, ", all.names = TRUE)")
    },
    keyIdStr = function(reactId, key) {
      paste0(reactId, "$", key)
    },

    valueStr = function(value, n = 200) {
      if (!self$isLogging()) {
        # return a placeholder string to avoid calling str
        return("<reactlog is turned off>")
      }
      output <- try(silent = TRUE, {
        # only capture the first level of the object
        utils::capture.output(utils::str(value, max.level = 1))
      })
      outputTxt <- paste0(output, collapse="\n")
      msg$shortenString(outputTxt, n = n)
    },

    initialize = function(rlogOption = "shiny.reactlog", msgOption = "shiny.reactlog.console") {
      private$option <- rlogOption
      private$msgOption <- msgOption

      self$reset()
    },
    reset = function() {
      .globals$reactIdCounter <- 0L

      self$logStack <- fastmap::faststack()
      self$msg <- MessageLogger$new(option = private$msgOption)

      # setup dummy and missing react information
      self$msg$setReact(force = TRUE, list(reactId = self$noReactId, label = self$noReactIdLabel))
      self$msg$setReact(force = TRUE, list(reactId = self$dummyReactId, label = self$dummyReactIdLabel))
    },
    isLogging = function() {
      isTRUE(getOption(private$option, FALSE))
    },

    define = function(reactId, value, label, type, domain) {
      valueStr <- self$valueStr(value)
      if (msg$hasReact(reactId)) {
        stop("react definition for id: ", reactId, " already found!!", "Label: ", label, "Type: ", type)
      }
      msg$setReact(list(reactId = reactId, label = label))
      msg$log("define:", msg$reactStr(reactId), msg$typeStr(type = type), msg$valueStr(valueStr))
      private$appendEntry(domain, list(
        action = "define",
        reactId = reactId,
        label = msg$shortenString(label),
        type = type,
        value = valueStr
      ))
    },
    defineNames = function(reactId, value, label, domain) {
      self$define(self$namesIdStr(reactId), value, self$namesIdStr(label), "reactiveValuesNames", domain)
    },
    defineAsList = function(reactId, value, label, domain) {
      self$define(self$asListIdStr(reactId), value, self$asListIdStr(label), "reactiveValuesAsList", domain)
    },
    defineAsListAll = function(reactId, value, label, domain) {
      self$define(self$asListAllIdStr(reactId), value, self$asListAllIdStr(label), "reactiveValuesAsListAll", domain)
    },
    defineKey = function(reactId, value, key, label, domain) {
      self$define(self$keyIdStr(reactId, key), value, self$keyIdStr(label, key), "reactiveValuesKey", domain)
    },
    defineObserver = function(reactId, label, domain) {
      self$define(reactId, value = NULL, label, "observer", domain)
    },

    dependsOn = function(reactId, depOnReactId, ctxId, domain) {
      if (is.null(reactId)) return()
      ctxId <- ctxIdStr(ctxId)
      msg$log("dependsOn:", msg$reactStr(reactId), " on", msg$reactStr(depOnReactId), msg$ctxStr(ctxId))
      private$appendEntry(domain, list(
        action = "dependsOn",
        reactId = reactId,
        depOnReactId = depOnReactId,
        ctxId = ctxId
      ))
    },
    dependsOnKey = function(reactId, depOnReactId, key, ctxId, domain) {
      self$dependsOn(reactId, self$keyIdStr(depOnReactId, key), ctxId, domain)
    },

    dependsOnRemove = function(reactId, depOnReactId, ctxId, domain) {
      ctxId <- self$ctxIdStr(ctxId)
      msg$log("dependsOnRemove:", msg$reactStr(reactId), " on", msg$reactStr(depOnReactId), msg$ctxStr(ctxId))
      private$appendEntry(domain, list(
        action = "dependsOnRemove",
        reactId = reactId,
        depOnReactId = depOnReactId,
        ctxId = ctxId
      ))
    },
    dependsOnKeyRemove = function(reactId, depOnReactId, key, ctxId, domain) {
      self$dependsOnRemove(reactId, self$keyIdStr(depOnReactId, key), ctxId, domain)
    },

    createContext = function(ctxId, label, type, prevCtxId, domain) {
      ctxId <- self$ctxIdStr(ctxId)
      prevCtxId <- self$ctxIdStr(prevCtxId)
      msg$log("createContext:", msg$ctxPrevCtxStr(preCtxIdTxt = " ", ctxId, prevCtxId, type))
      private$appendEntry(domain, list(
        action = "createContext",
        ctxId = ctxId,
        label = msg$shortenString(label),
        type = type,
        prevCtxId = prevCtxId,
        srcref = as.vector(attr(label, "srcref")), srcfile=attr(label, "srcfile")
      ))
    },

    enter = function(reactId, ctxId, type, domain) {
      ctxId <- self$ctxIdStr(ctxId)
      if (identical(type, "isolate")) {
        msg$log("isolateEnter:", msg$reactStr(reactId), msg$ctxStr(ctxId))
        msg$depthIncrement()
        private$appendEntry(domain, list(
          action = "isolateEnter",
          reactId = reactId,
          ctxId = ctxId
        ))
      } else {
        msg$log("enter:", msg$reactStr(reactId), msg$ctxStr(ctxId, type))
        msg$depthIncrement()
        private$appendEntry(domain, list(
          action = "enter",
          reactId = reactId,
          ctxId = ctxId,
          type = type
        ))
      }
    },
    exit = function(reactId, ctxId, type, domain) {
      ctxId <- self$ctxIdStr(ctxId)
      if (identical(type, "isolate")) {
        msg$depthDecrement()
        msg$log("isolateExit:", msg$reactStr(reactId), msg$ctxStr(ctxId))
        private$appendEntry(domain, list(
          action = "isolateExit",
          reactId = reactId,
          ctxId = ctxId
        ))
      } else {
        msg$depthDecrement()
        msg$log("exit:", msg$reactStr(reactId), msg$ctxStr(ctxId, type))
        private$appendEntry(domain, list(
          action = "exit",
          reactId = reactId,
          ctxId = ctxId,
          type = type
        ))
      }
    },

    valueChange = function(reactId, value, domain) {
      valueStr <- self$valueStr(value)
      msg$log("valueChange:", msg$reactStr(reactId), msg$valueStr(valueStr))
      private$appendEntry(domain, list(
        action = "valueChange",
        reactId = reactId,
        value = valueStr
      ))
    },
    valueChangeNames = function(reactId, nameValues, domain) {
      self$valueChange(self$namesIdStr(reactId), nameValues, domain)
    },
    valueChangeAsList = function(reactId, listValue, domain) {
      self$valueChange(self$asListIdStr(reactId), listValue, domain)
    },
    valueChangeAsListAll = function(reactId, listValue, domain) {
      self$valueChange(self$asListAllIdStr(reactId), listValue, domain)
    },
    valueChangeKey = function(reactId, key, value, domain) {
      self$valueChange(self$keyIdStr(reactId, key), value, domain)
    },


    invalidateStart = function(reactId, ctxId, type, domain) {
      ctxId <- self$ctxIdStr(ctxId)
      if (identical(type, "isolate")) {
        msg$log("isolateInvalidateStart:", msg$reactStr(reactId), msg$ctxStr(ctxId))
        msg$depthIncrement()
        private$appendEntry(domain, list(
          action = "isolateInvalidateStart",
          reactId = reactId,
          ctxId = ctxId
        ))
      } else {
        msg$log("invalidateStart:", msg$reactStr(reactId), msg$ctxStr(ctxId, type))
        msg$depthIncrement()
        private$appendEntry(domain, list(
          action = "invalidateStart",
          reactId = reactId,
          ctxId = ctxId,
          type = type
        ))
      }
    },
    invalidateEnd = function(reactId, ctxId, type, domain) {
      ctxId <- self$ctxIdStr(ctxId)
      if (identical(type, "isolate")) {
        msg$depthDecrement()
        msg$log("isolateInvalidateEnd:", msg$reactStr(reactId), msg$ctxStr(ctxId))
        private$appendEntry(domain, list(
          action = "isolateInvalidateEnd",
          reactId = reactId,
          ctxId = ctxId
        ))
      } else {
        msg$depthDecrement()
        msg$log("invalidateEnd:", msg$reactStr(reactId), msg$ctxStr(ctxId, type))
        private$appendEntry(domain, list(
          action = "invalidateEnd",
          reactId = reactId,
          ctxId = ctxId,
          type = type
        ))
      }
    },

    invalidateLater = function(reactId, runningCtx, millis, domain) {
      msg$log("invalidateLater: ", millis, "ms", msg$reactStr(reactId), msg$ctxStr(runningCtx))
      private$appendEntry(domain, list(
        action = "invalidateLater",
        reactId = reactId,
        ctxId = runningCtx,
        millis = millis
      ))
    },

    idle = function(domain = NULL) {
      msg$log("idle")
      private$appendEntry(domain, list(
        action = "idle"
      ))
    },

    asyncStart = function(domain = NULL) {
      msg$log("asyncStart")
      private$appendEntry(domain, list(
        action = "asyncStart"
      ))
    },
    asyncStop = function(domain = NULL) {
      msg$log("asyncStop")
      private$appendEntry(domain, list(
        action = "asyncStop"
      ))
    },

    freezeReactiveVal = function(reactId, domain) {
      msg$log("freeze:", msg$reactStr(reactId))
      private$appendEntry(domain, list(
        action = "freeze",
        reactId = reactId
      ))
    },
    freezeReactiveKey = function(reactId, key, domain) {
      self$freezeReactiveVal(self$keyIdStr(reactId, key), domain)
    },

    thawReactiveVal = function(reactId, domain) {
      msg$log("thaw:", msg$reactStr(reactId))
      private$appendEntry(domain, list(
        action = "thaw",
        reactId = reactId
      ))
    },
    thawReactiveKey = function(reactId, key, domain) {
      self$thawReactiveVal(self$keyIdStr(reactId, key), domain)
    },

    userMark = function(domain = NULL) {
      msg$log("userMark")
      private$appendEntry(domain, list(
        action = "userMark"
      ))
    }

  )
)
