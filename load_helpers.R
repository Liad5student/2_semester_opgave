# ------------------------------------------------------------------------------
# Helper-funktion: Hent og læs en .rds-fil fra en anden branch (Git)
# ------------------------------------------------------------------------------

get_rds_from_branch <- function(branch, file) {
  filepath <- file.path("data", file)
  
  if (file.exists(filepath)) {
    message("ℹ️  Filen findes allerede: ", filepath, " – springer over.")
    return(readRDS(filepath))
  }
  
  cmd <- paste("git checkout", branch, "--", filepath)
  message("\n🔄 Henter '", file, "' fra branch '", branch, "' ...")
  result <- system(cmd, intern = TRUE)
  
  if (!file.exists(filepath)) {
    stop("❌ Filen blev ikke hentet. Tjek at den eksisterer i den angivne branch.")
  }
  
  message("✅ Fil hentet og klar til brug: ", filepath)
  readRDS(filepath)
}