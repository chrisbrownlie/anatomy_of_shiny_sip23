Dependents <- R6Class(
  'Dependents',
  portable = FALSE,
  class = FALSE,
  public = list(
    .reactId = character(0),
    .dependents = 'Map',

    initialize = function(reactId = NULL) {
      .reactId <<- reactId
      .dependents <<- Map$new()
    },
    # ... ignored, use to be depLabel and depId, not used anymore
    register = function(...) {
      ctx <- getCurrentContext()
      if (!.dependents$containsKey(ctx$id)) {

        # must wrap in if statement as ctx react id could be NULL
        #   if options(shiny.suppressMissingContextError = TRUE)
        if (is.character(.reactId) && is.character(ctx$.reactId)) {
          rLog$dependsOn(ctx$.reactId, .reactId, ctx$id, ctx$.domain)
        }

        if (ctx$isWeak()) {
          .dependents$set(ctx$id, rlang::new_weakref(ctx))
        } else {
          .dependents$set(ctx$id, ctx)
        }

        ctx$onInvalidate(function() {
          rLog$dependsOnRemove(ctx$.reactId, .reactId, ctx$id, ctx$.domain)
          .dependents$remove(ctx$id)
        })
      }
    },
    # at times, the context is run in a ctx$onInvalidate(...) which has no runtime context
    invalidate = function(log = TRUE) {
      if (isTRUE(log)) {

        domain <- getDefaultReactiveDomain()
        rLog$invalidateStart(.reactId, NULL, "other", domain)
        on.exit(
          rLog$invalidateEnd(.reactId, NULL, "other", domain),
          add = TRUE
        )
      }
      lapply(
        .dependents$values(sort = TRUE),
        function(ctx) {
          if (rlang::is_weakref(ctx)) {
            ctx <- rlang::wref_key(ctx)
            if (is.null(ctx)) {
              # Can get here if weakref target was GC'd
              return()
            }
          }
          ctx$invalidate()
          NULL
        }
      )
    }
  )
)
