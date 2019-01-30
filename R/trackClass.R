#' Define a S4 class for audio
#' @export
Audio <- setClass("audio", slots =
                    list(track = "list",
                         bars = "list",
                         beats = "list",
                         tatums = "list",
                         sections = "list",
                         segments = "list"))

#' Check if object is of type audio
#'
#' @export
is.audio <- function(x) inherits(x, "audio")


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

#' Check if object is audioFeature
#'
#' @export
is.audioFeatures <- function(x) inherits(x, "audioFeatures")



#' TODO: Add legends to the  plots made with this function.
#' TODO: A function that creates ready lists of playlists to be plotted.

#' Plot audio features.
#'
#' Plots one feature against other given a list of audio feature objects.
#'
#' @param x Name of the feature to be plotted on x-axis.
#' @param y Naem of the feature to be plotted on y-axis.
#' @param tracks A list of audioFeatures objects, or a list of lists(different
#' playlists perhaps).
#'
#' @export
featurePlot <- function(x, y, trakcs) {

  if (is.audioFeatures(trakcs[[1]])) {
    g <- listOfAudioFeaturse(x, y, trakcs)
  } else if (is.list(trakcs[[1]])) {
    g <- listOfPlaylists(x, y, trakcs)
  } else {
    stop("Invalid input type on tracks.")
  }

  show(g)
}


listOfPlaylists <- function(x, y, tracks) {
  #nam <- names(tracks)

  g <- ggplot() +
    theme_minimal() +
    ylab(y) +
    xlab(x)

  colType <- 1:length(tracks)

  for (i in 1:length(tracks)) {
    df <- mapToFeatureDF(x, y, tracks[i])
    g <- g +
      geom_point(data = df,
                 aes(x, y),
                 col = colType[i])
  }

  return(g)
}

listOfAudioFeaturse <- function(x, y, tracks) {
  df <- mapToFeatureDF(x, y, trakcs)

  g <- ggplot(df, aes(x, y)) +
    geom_point() +
    theme_minimal() +
    ylab(y) +
    xlab(x)

  return(g)
}


#' Helper to map the track list to numerical vectors
mapToFeatureDF <- function(x, y, tracks) {
  tracks <- tracks[[1]]
  x_dat <- map_dbl(tracks, function(track) return(slot(track, x)))
  y_dat <- map_dbl(tracks, function(track) return(slot(track, y)))

  return(data.frame(x = x_dat, y = y_dat))
}





