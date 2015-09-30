sd_section(
  "Trip Functions",
  "These functions are used to query, prepare, explore, and analyze trip data.",
  c("tripAnalysis", "gpsTrimmer", "tripPrep","attach_TripGeoData",
    "attach_TripTimeData", "attach_TripWeatherData","trip_barPlots",
    "updateMapMatchCoords","updateMapMatchLinks","similarityWeight")
)
sd_section(
  "User Functions",
  "These functions are used to query, prepare, explore, and analyze user data.",
  c("userAnalysis", "user_barPlots", "userPrep")
)
sd_section(
  "Report Functions",
  "These functions are used to query, prepare, explore, and analyze report data.",
  c("reportAnalysis","report_barPlots", "reportPrep","attach_ReportGeoData", "trafficStress", "syncPhotos","downloadReportPhotos", "attach_GmapReportData")
)
sd_section(
  "General Use Functions",
  "These functions are used by other functions in the package.",
  c("imputeResponses", "wrap_sentence","forestplot","crossTabPlot","subDataByGeoBound")
)
sd_section(
  "Google API Utilities",
  "These functions are used to interact with Google APIs",
  c("decodeLineR", "getGoogleDirections","lines2points","route2shape")
)
