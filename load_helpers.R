# -------------------------------------------------------------------
# Helper-funktion: Hent og læs en .rds-fil fra en anden branch
# Overskriver altid eksisterende filer og bruger enkel, lækker output
# -------------------------------------------------------------------

get_rds_from_branch <- function(branch, file) {
  filepath <- file.path("data", file)
  
  green <- "\033[32m"
  red   <- "\033[31m"
  reset <- "\033[0m"
  
  cat("\n🔄 Henter '", file, "' fra branch '", branch, "' ...\n", sep = "")
  
  if (file.exists(filepath)) {
    file.remove(filepath)
  }
  
  result <- suppressWarnings(system(paste("git checkout", branch, "--", filepath), intern = TRUE))
  
  if (!file.exists(filepath)) {
    cat(red, "  ❌ Filen blev ikke hentet. Tjek at den eksisterer i den angivne branch.", reset, "\n", sep = "")
    return(NULL)
  }
  
  cat(green, "✅ Fil hentet og klar til brug: ", filepath, reset, "\n", sep = "")
  readRDS(filepath)
}