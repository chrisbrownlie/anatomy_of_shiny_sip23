ReactiveValues <- R6Class(
  'ReactiveValues',
  portable = FALSE,
  public = list(
    # For debug purposes
    .reactId = character(0),
    .label = character(0),
    .values = 'Map',
    .metadata = 'Map',
    # A map of Dependents objects, one for each key
    .dependents = 'Map',
    # Dependents for the list of all names, including hidden
    .namesDeps = 'Dependents',
    # Dependents for all values, including hidden
    .allValuesDeps = 'Dependents',
    # Dependents for all values
    .valuesDeps = 'Dependents',
    .dedupe = logical(0),
    # Key, asList(), or names() have been retrieved
    .hasRetrieved = list(),
    # All names, in insertion order. The names are also stored in the .values
    # object, but it does not preserve order.
    .nameOrder = character(0),


    initialize = function(
    dedupe = TRUE,
    label = paste0('reactiveValues', p_randomInt(1000, 10000))
    ) {
      .reactId <<- nextGlobalReactId()
      .label <<- label
      .values <<- Map$new()
      .metadata <<- Map$new()
      .dependents <<- Map$new()
      .hasRetrieved <<- list(names = FALSE, asListAll = FALSE, asList = FALSE)
      .namesDeps <<- Dependents$new(reactId = rLog$namesIdStr(.reactId))
      .allValuesDeps <<- Dependents$new(reactId = rLog$asListAllIdStr(.reactId))
      .valuesDeps <<- Dependents$new(reactId = rLog$asListIdStr(.reactId))
      .dedupe <<- dedupe
    },

    get = function(key) {
      # get value right away to use for logging
      keyValue <- .values$get(key)

      if (!.dependents$containsKey(key)) {
        # If we got here, this is the first time someone has tried to access
        # this key.
        rLog$defineKey(.reactId, keyValue, key, .label, getCurrentContext()$.domain)

        reactKeyId <- rLog$keyIdStr(.reactId, key)
        .dependents$set(key, Dependents$new(reactKeyId))
      }

      # Register the "downstream" reactive which is accessing this value, so
      # that we know to invalidate them when this value changes.
      .dependents$get(key)$register()

      if (isFrozen(key))
        reactiveStop()

      keyValue
    },

    set = function(key, value, force = FALSE) {
      # if key exists
      #   if it is the same value, return
      #
      # update value of `key`
      #
      # if key exists
      #   if `key` has been read,
      #     log `update key`
      #     ## (invalidate key later in code)
      # else # if new key
      #   if `names()` have been read,
      #     log `update names()`
      #     invalidate `names()`
      #
      # if hidden
      #   if asListAll has been read,
      #     log `update asList(all.names = TRUE)`
      #     invalidate `asListAll`
      # else # not hidden
      #   if asList has been read,
      #     log `update asList()`
      #     invalidate `asList`
      #
      # update value of `key`
      # invalidate all deps of `key`

      domain <- getDefaultReactiveDomain()
      hidden <- substr(key, 1, 1) == "."

      key_exists <- .values$containsKey(key)

      if (key_exists && !isTRUE(force) && .dedupe && identical(.values$get(key), value)) {
        return(invisible())
      }

      # If it's new, append key to the name order
      if (!key_exists) {
        .nameOrder[length(.nameOrder) + 1] <<- key
      }

      # set the value for better logging
      .values$set(key, value)

      # key has been depended upon
      if (.dependents$containsKey(key)) {
        rLog$valueChangeKey(.reactId, key, value, domain)
        .dependents$get(key)$invalidate()
      }

      # only invalidate if there are deps
      if (!key_exists && isTRUE(.hasRetrieved$names)) {
        rLog$valueChangeNames(.reactId, .values$keys(), domain)
        .namesDeps$invalidate()
      }

      if (hidden) {
        if (isTRUE(.hasRetrieved$asListAll)) {
          rLog$valueChangeAsListAll(.reactId, .values$values(), domain)
          .allValuesDeps$invalidate()
        }
      } else {
        if (isTRUE(.hasRetrieved$asList)) {
          react_vals <- .values$values()
          react_vals <- react_vals[!grepl("^\\.", base::names(react_vals))]
          # leave as is. both object would be registered to the listening object
          rLog$valueChangeAsList(.reactId, react_vals, domain)
          .valuesDeps$invalidate()
        }
      }

      invisible()
    },

    mset = function(lst) {
      lapply(base::names(lst),
             function(name) {
               self$set(name, lst[[name]])
             })
    },

    names = function() {
      if (!isTRUE(.hasRetrieved$names)) {
        domain <- getDefaultReactiveDomain()
        rLog$defineNames(.reactId, .nameOrder, .label, domain)
        .hasRetrieved$names <<- TRUE
      }
      .namesDeps$register()
      return(.nameOrder)
    },

    # Get a metadata value. Does not trigger reactivity.
    getMeta = function(key, metaKey) {
      # Make sure to use named (not numeric) indexing into list.
      metaKey <- as.character(metaKey)
      .metadata$get(key)[[metaKey]]
    },

    # Set a metadata value. Does not trigger reactivity.
    setMeta = function(key, metaKey, value) {
      # Make sure to use named (not numeric) indexing into list.
      metaKey <- as.character(metaKey)

      if (!.metadata$containsKey(key)) {
        .metadata$set(key, list())
      }

      m <- .metadata$get(key)
      m[[metaKey]] <- value
      .metadata$set(key, m)
    },

    # Mark a value as frozen If accessed while frozen, a shiny.silent.error will
    # be thrown.
    freeze = function(key, invalidate = FALSE) {
      domain <- getDefaultReactiveDomain()
      rLog$freezeReactiveKey(.reactId, key, domain)
      setMeta(key, "frozen", TRUE)

      if (invalidate) {
        # Force an invalidation
        self$set(key, NULL, force = TRUE)
      }
    },

    thaw = function(key) {
      domain <- getDefaultReactiveDomain()
      rLog$thawReactiveKey(.reactId, key, domain)
      setMeta(key, "frozen", NULL)
    },

    isFrozen = function(key) {
      isTRUE(getMeta(key, "frozen"))
    },

    toList = function(all.names=FALSE) {
      listValue <- .values$mget(.nameOrder)
      if (!all.names) {
        listValue <- listValue[!grepl("^\\.", base::names(listValue))]
      }
      if (all.names) {
        if (!isTRUE(.hasRetrieved$asListAll)) {
          domain <- getDefaultReactiveDomain()
          rLog$defineAsListAll(.reactId, listValue, .label, domain)
          .hasRetrieved$asListAll <<- TRUE
        }
        .allValuesDeps$register()
      }

      if (!isTRUE(.hasRetrieved$asList)) {
        domain <- getDefaultReactiveDomain()
        # making sure the value being recorded is with `all.names = FALSE`
        rLog$defineAsList(.reactId, listValue[!grepl("^\\.", base::names(listValue))], .label, domain)
        .hasRetrieved$asList <<- TRUE
      }
      .valuesDeps$register()

      return(listValue)
    }

  )
)
