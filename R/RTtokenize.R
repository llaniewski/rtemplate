
#' Change RT chunks into tokens
#'
#' The function takes input file and changes all RT blocks into tokens. This can be reversed using inverse=TRUE
#'
#' @export
RTtokenize = function(infile="stdin", token="__RT_*_RT__", inverse=FALSE, tokenfile="tmp.rds", outfile=stdout(), delete_tokenfile=TRUE) {
  lines = readLines(con = infile)
  output = character(0)
  if (! inverse) {
    chunks = RTchunks(lines)
    to_save = character(0)
    for (i in 1:nrow(chunks))  {
      n=chunks[i,]
      lu = lines[n$start.line:n$end.line]
      if (n$start.line == n$end.line) {
        lu = substr(lu,n$start.char,n$end.char)
      } else {
        lu[1] = substr(lu[1],n$start.char,nchar(lu[1]))
        lu[length(lu)] = substr(lu[length(lu)],1,n$end.char)
      }
      tag = as.character(n$tag)
      outadd = NULL
      if (n$start.line != n$end.line) lu[2:length(lu) - 1] = paste(lu[2:length(lu) - 1],"\n",sep="")
      if (tag == "") {
        outadd = lu
      } else {
        lu[1] = paste0("<?", tag, lu[1])
        lu[length(lu)] = paste0(lu[length(lu)], "?>")
        to_save = c(to_save, paste0(lu,collapse=""))
        k = length(to_save)
        outadd = sprintf("__RT_%04d_RT__", k)
      }
      output = c(output,outadd)
    }
    if (tokenfile != "") saveRDS(to_save, tokenfile)
    output = c(output,"\n")
  } else {
    if (!file.exists(tokenfile)) stop("Token-file ",tokenfile," doesn't exist")
    token_list = readRDS(tokenfile)
    if (delete_tokenfile) file.remove(tokenfile)
    x = gregexpr("__RT_([[:digit:]]*)_RT__", lines, perl=TRUE)
    sel = which(sapply(x,function(x) x[1] != -1))
    for (i in sel) {
      s = lines[i]
      a = x[[i]]; b = attr(x[[i]],"match.length")
      tok = substring(s,a,a+b-1)
      a = attr(x[[i]],"capture.start"); b = attr(x[[i]],"capture.length")
      idx = as.integer(substring(s,a,a+b-1))
      for (j in seq_along(tok)) s = gsub(tok[j], token_list[idx[j]], s)
      lines[i] = s
    }
    output = paste0(lines,"\n")
  }
  writeLines(output, con = outfile, sep = "")
  invisible(output)
}
