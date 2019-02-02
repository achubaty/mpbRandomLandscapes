
# Everything in this file gets sourced during simInit, and all functions and objects
#  are put into the simList. To use objects and functions, use sim$xxx.
defineModule(sim, list(
  name = "mpbRandomLandscapes",
  description = "Mountain Pine Beetle Red Top Growth Model: Short-run Potential for Establishment, Eruption, and Spread",
  keywords = c("mountain pine beetle, outbreak dynamics, eruptive potential, spread, climate change, twitch response"),
  authors = c(
    person(c("Alex", "M"), "Chubaty", email = "alexander.chubaty@canada.ca", role = c("aut", "cre"))
  ),
  childModules = character(),
  version = numeric_version("0.0.1"),
  spatialExtent = raster::extent(rep(NA_real_, 4)),
  timeframe = as.POSIXlt(c(NA, NA)),
  timeunit = "year",
  citation = list(),
  reqdPkgs = list("amc", "RandomFields", "raster", "RColorBrewer"),
  parameters = rbind(
    defineParameter(".plotInitialTime", "numeric", start(sim), NA, NA, "This describes the simulation time at which the first plot event should occur"),
    defineParameter(".plotInterval", "numeric", NA, NA, NA, "This describes the interval between plot events"),
    defineParameter(".saveInitialTime", "numeric", NA, NA, NA, "This describes the simulation time at which the first save event should occur"),
    defineParameter(".saveInterval", "numeric", NA, NA, NA, "This describes the interval between save events"),
    defineParameter(".useCache", "logical", FALSE, NA, NA, "Should this entire module be run with caching activated?")
  ),
  inputObjects = bind_rows(
    expectsInput("studyArea", "SpatialPolygons",
                 "The study area to which all maps will be cropped and reprojected.", sourceURL = NA)
  ),
  outputObjects = bind_rows(
    createsOutput("climateSuitabilityMap", "RasterLayer", "A climatic suitablity map for the current year."),
    createsOutput("pineMap", "RasterLayer", "Current lodgepole and jack pine available for MPB."),
    createsOutput("pineDT", "data.table", "Current lodgepole and jack pine available for MPB.")
  )
))

## event types
#   - type `init` is required for initiliazation

doEvent.mpbRandomLandscapes <- function(sim, eventTime, eventType, debug = FALSE) {
  switch(eventType,
    "init" = {
      ### check for more detailed object dependencies:
      ### (use `checkObject` or similar)

      # do stuff for this event
      sim <- Init(sim)

      # schedule future event(s)
      sim <- scheduleEvent(sim, P(sim)$.plotInitialTime, "mpbRandomLandscapes", "plot")
      sim <- scheduleEvent(sim, P(sim)$.saveInitialTime, "mpbRandomLandscapes", "save")
    },
    "plot" = {
      # ! ----- EDIT BELOW ----- ! #
      # do stuff for this event
      Plot(sim$climateSuitabilityMap)
      Plot(sim$pineMap)

      # schedule future event(s)
      sim <- scheduleEvent(sim, time(sim) + P(sim)$.plotInterval, "mpbRandomLandscapes", "plot")

      # ! ----- STOP EDITING ----- ! #
    },
    "save" = {
      # ! ----- EDIT BELOW ----- ! #
      # do stuff for this event

      # e.g., call your custom functions/methods here
      # you can define your own methods below this `doEvent` function

      # schedule future event(s)

      # e.g.,
      # sim <- scheduleEvent(sim, time(sim) + increment, "mpbRandomLandscapes", "save")

      # ! ----- STOP EDITING ----- ! #
      },
      warning(paste("Undefined event type: '", events(sim)[1, "eventType", with = FALSE],
                    "' in module '", events(sim)[1, "moduleName", with = FALSE], "'", sep = ""))
  )
  return(invisible(sim))
}

.inputObjects <- function(sim) {
  if (!suppliedElsewhere("studyArea")) {
    prj <- paste("+proj=aea +lat_1=47.5 +lat_2=54.5 +lat_0=0 +lon_0=-113",
                 "+x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0")
    sim$studyArea <- amc::loadStudyArea(dataPath(sim), "studyArea.kml", prj)
  }

  return(invisible(sim))
}

### template initilization
Init <- function(sim) {
  # # ! ----- EDIT BELOW ----- ! #
  template <- if (is(sim$studyArea, "RasterLayer")) {
    sim$studyArea
  } else {
    raster(sim$studyArea, crs = CRS(proj4string(sim$studyArea)))
  }
  speedup <- max(1, ncol(sim$studyArea) / 5e2)
  inMemory <- TRUE

  pineMap <- gaussMap(template, scale = 10, var = 1, speedup = speedup, inMemory = inMemory)
  pineMap[] <- round(getValues(pineMap), 1)
  pineMap <- pineMap / maxValue(pineMap) * 500 ## scale values to range 0-500
  setColors(pineMap) <- brewer.pal(9, "Greens")
  sim$pineMap <- pineMap

  # Make layers that are derived from other layers
  climateSuitabilityMap <- gaussMap(template, scale = 10, var = 1, speedup = speedup, inMemory = inMemory)
  climateSuitabilityMap[] <- getValues(climateSuitabilityMap) / maxValue(climateSuitabilityMap) ## values range from 0-1
  setColors(climateSuitabilityMap) <- rev(brewer.pal(8, "Spectral"))
  sim$climateSuitabilityMap <- climateSuitabilityMap

  # ! ----- STOP EDITING ----- ! #
  return(invisible(sim))
}
