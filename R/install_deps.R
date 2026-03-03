pkgs <- c(
  "readxl","writexl","dplyr","tibble","tidyr",
  "ggplot2",
  "drc","data.table","caTools","MESS","minpack.lm","stringr"
)

to_install <- pkgs[!vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)]

if (length(to_install) > 0) {
  message("Installing missing packages: ", paste(to_install, collapse = ", "))
  install.packages(to_install, repos = "https://cloud.r-project.org")
} else {
  message("All required packages are already installed.")
}
