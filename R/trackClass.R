#' Define a S4 class for audio
#' @export
audio <- setClass("audio", slots =
                    list(track = "list",
                         bars = "list",
                         beats = "list",
                         tatums = "list",
                         sections = "list",
                         segments = "list"))

#' Plotting for audio.
#'
#' @export
setMethod("plot", "audio", function(x) {
  print("a")
})
