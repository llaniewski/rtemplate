
#' Change RT chunks into tokens
#'
#' The function takes input file and changes all RT blocks into tokens. This can be reversed using inverse=TRUE
#'
#' @export
RTtokenize = function(infile="stdin", token="__RT_*_RT__", inverse=FALSE, tokenfile="tmp.rds", outfile=stdout(), delete_tokenfile=TRUE) {
  lines = readLines(con = infile)
  lines_n = length(lines)
  output = character(0)
  if (! inverse) {
    chunks = RTchunks(lines)
    chunks$tok = chunks$tag %in% c("R","RT","python") | substr(chunks$tag,1,1) == "%"
    to_save = list(0)
    to_save_index = 0
    for (i in seq_len(nrow(chunks))) {
      n=chunks[i,]
      lu = lines[n$start.line:n$end.line]
      if (n$start.line == n$end.line) {
        lu = substr(lu,n$whole.start.char,n$whole.end.char)
      } else {
        lu[1] = substr(lu[1],n$whole.start.char,nchar(lu[1]))
        lu[length(lu)] = substr(lu[length(lu)],1,n$whole.end.char)
      }
      tag = as.character(n$tag)
      tok = n$tok
      outadd = NULL
      if (n$start.line != n$end.line) lu[2:length(lu) - 1] = paste(lu[2:length(lu) - 1],"\n",sep="")
      if (tag == "") {
        outadd = lu
      } else {
        if (tok) {
          if (n$start.line == 1 && n$whole.start.char == 1) {
            token_name = "__RT_FIRST_RT__"
          } else if (n$end.line == lines_n && n$whole.end.char == nchar(lines[lines_n])) {
            token_name = "__RT_LAST_RT__"
          } else {
            to_save_index = to_save_index + 1
            token_name = sprintf("__RT_%04d_RT__", to_save_index)
            outadd = token_name
          }
          to_save[[token_name]] = paste0(lu,collapse="")
        } else {
          outadd = lu
        }
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
      for (j in tok) s = gsub(j, token_list[[j]], s, fixed=TRUE)
      lines[i] = s
    }
    if (lines_n > 0) {
      lines[1] = paste0(token_list[["__RT_FIRST_RT__"]],lines[1])
      lines[lines_n] = paste0(lines[lines_n],token_list[["__RT_LAST_RT__"]])
    }
    output = paste0(lines,"\n")
  }
  writeLines(output, con = outfile, sep = "")
  invisible(output)
}
