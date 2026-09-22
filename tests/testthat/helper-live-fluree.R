live_tagger_enabled <- function() {
  identical(tolower(Sys.getenv("FLUREE_LIVE_TEST", "false")), "true")
}
