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
  plotBars(audio)

})


#' Plots bars.
#'
#' @export
plotBars <- function(bars, thePlot) {

}


#' S4 class for Audio features
#'
#' @export
audioFeatures <- setClass("audioFeatures", slots =
                       list(
                         dancebility = "numeric",
                         energy = "numeric",
                         key = "integer",
                         loudness = "numeric",
                         mode = "integer",
                         speechiness = "numeric",
                         acousticness = "numeric",
                         instrumentalness = "integer",
                         liveness = "numeric",
                         valence = "numeric",
                         tempo = "numeric",
                         type = "character",
                         id = "character",
                         uri = "character",
                         track_href = "character",
                         analysis_url = "character",
                         duration_ms = "integer",
                         time_signature = "integer"
                       ))


