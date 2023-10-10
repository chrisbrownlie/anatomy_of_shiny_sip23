MockableTimerCallbacks <- R6Class(
  'MockableTimerCallbacks',
  inherit = TimerCallbacks,
  portable = FALSE,
  class = FALSE,
  public = list(
    # Empty constructor defaults to the getNow implementation
    initialize = function() {
      super$initialize(self$mockNow)
    },
    mockNow = function() {
      return(private$time)
    },
    elapse = function(millis) {
      private$time <- private$time + millis
    },
    getElapsed = function() {
      private$time
    }
  ), private = list(
    time = 0L
  )
)
