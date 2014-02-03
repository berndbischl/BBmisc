inDirectory = function(dir, expr) {
  old = getwd()
  on.exit(setwd(old))
  setwd(dir)
  expr
}
