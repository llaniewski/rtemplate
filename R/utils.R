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
  k = min(which(as[k] != bs[k]))
  ret = c(rep("..",length(bs)-k),as[k:length(as)])
  ret = paste(ret,collapse="/")
  ret
}
