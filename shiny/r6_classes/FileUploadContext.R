FileUploadContext <- R6Class(
  'FileUploadContext',
  class = FALSE,
  private = list(
    basedir = character(0),
    operations = 'Map',
    ids = character(0)  # Keep track of all ids used for file uploads
  ),
  public = list(
    initialize = function(dir=tempdir()) {
      private$basedir <- dir
      private$operations <- Map$new()
    },
    createUploadOperation = function(fileInfos) {
      while (TRUE) {
        id <- createUniqueId(12)
        private$ids <- c(private$ids, id)
        dir <- file.path(private$basedir, id)
        if (!dir.create(dir))
          next

        op <- FileUploadOperation$new(self, id, dir, fileInfos)
        private$operations$set(id, op)
        return(id)
      }
    },
    getUploadOperation = function(jobId) {
      private$operations$get(jobId)
    },
    onJobFinished = function(jobId) {
      private$operations$remove(jobId)
    },
    # Remove the directories containing file uploads; this is to be called when
    # a session ends.
    rmUploadDirs = function() {
      # Make sure all_paths is underneath the tempdir()
      if (!grepl(normalizePath(tempdir()), normalizePath(private$basedir), fixed = TRUE)) {
        stop("Won't remove upload path ", private$basedir,
             "because it is not under tempdir(): ", tempdir())
      }

      all_paths <- file.path(private$basedir, private$ids)
      unlink(all_paths, recursive = TRUE)
    }
  )
)
