readlink = function(a) {
  f = pipe(paste("readlink -f",a))
  ret = readLines(f)
  close(f)
  ret
}

relativePath = function(a,b) {
  a = readlink(a)
  b = readlink(b)
  a = strsplit(a,"/")[[1]]
  b = strsplit(b,"/")[[1]]
  k = 1:min(length(a),length(b))
  k = min(which(a[k] != b[k]))
  ret = c(rep("..",length(b)-k),a[k:length(a)])
  ret = paste(ret,collapse="/")
  ret
}
