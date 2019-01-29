#' Define a S4 class for audio
#' @export
Audio <- setClass("audio", slots =
                    list(track = "list",
                         bars = "list",
                         beats = "list",
                         tatums = "list",
                         sections = "list",
                         segments = "list"))

#' @importFrom graphics plot
setGeneric("plot")

#' Plots audio object.
#'
#' @param timeRange A vector of length 2 that contains the start and finnish of
#' range you desire to explore.
#' @param showSegments Whether to show segments of not, since they are numerous so they might
#' get on the way of seeing other stuff.
#' @export
plot.audio <- function(x, timeRange = NULL, showSegments = FALSE) {
  g <- ggplot() +
    ylim(0, 3) +
    theme_minimal() +
    ylab("") +
    xlab("Time")

  g <- plotBars(x, g)
  g <- plotSections(x, g)
  g <- plotTatums(x, g)
  g <- plotBeat(x, g)

  if (showSegments == TRUE) {
    g <- plotSegments(x, g)
  }

  if (is.null(timeRange)) {
    show(g)
  } else {
    g <- g +
      xlim(timeRange[1], timeRange[2])

    show(g)
  }
}

#' Plots bars.
#'
#'
plotBars <- function(audio, g) {
  bars <- map_dbl(audio@bars, function(x){
    return(x[[ "start" ]])
  })
  g <- g +
    geom_point(aes(bars, 2), shape = 8)
  return(g)
}

#' Plots sections.
#'
#'
plotSections <- function(audio, g) {
  sections <- map_dbl(audio@sections, function(x) {
    return(x[[ "start" ]])
  })

  g <- g +
    geom_vline(xintercept = sections, col = "green")

  return(g)
}

#' Plot tatums.
#'
#'
plotTatums <- function(audio, g) {
  tatums <- map_dbl(audio@tatums, function(x) {
    return(x[[ "start" ]])
  })

  g <- g +
    geom_point(aes(tatums, 1.5), shape = 20, col = "blue")

  return(g)
}


#' Plots segments.
#'
#'
plotSegments <- function(audio, g) {
  seg <- map_dbl(audio@segments, function(x) {
    return(x[[ "start" ]])
  })

  g <- g +
    geom_vline(xintercept = seg, col = "blue")

  return(g)
}

#' Plot beat.
#'
#'
plotBeat <- function(audio, g) {
  beat <- map_dbl(audio@beats, function(x) {
    return(x[[ "start" ]])
  })

  g <- g +
    geom_point(aes(beat, 1), shape = 3, col = "red")

  return(g)
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



#' TODO: plotting method for audio features. Requires new getter that constructs
#' a list of lists of audio features from playlists that we want to explore.
#' Polots then the selected playlists on 2d grapgh that has x and y axes that
#' represent two of the features.

#' Plot audio features.
#'
#' Plots one feature against other given a list of audio feature objects.
#'
#' @param x Name of the feature to be plotted on x-axis.
#' @param y Naem of the feature to be plotted on y-axis.
#' @param tracks A list of audioFeatures objects.
#'
#' @export
featurePlot <- function(x, y, trakcs) {
  df <- mapTo(x, y, trakcs)

  g <- ggplot(df, aes(x, y)) +
    geom_point() +
    theme_minimal() +
    ylab(y) +
    xlab(x)

  return(g)
}


#' Helper to map the track list to numerical vectors
mapTo <- function(x, y, tracks) {
  x_dat <- map_dbl(tracks, function(track) return(slot(track, x)))
  y_dat <- map_dbl(tracks, function(track) return(slot(track, y)))

  return(data.frame(x = x_dat, y = y_dat))
}





