
#' Interpret a RT source in vector of strings and generates R code
#'
#' Interpret a RT source in vector of strings and generates R code
#'
#' @export
#'
#' @examples
#' RTconvert(c(
#'  "Hello",
#'  "<?R cat('World') ?>"
#' ))
RTtokenize = function(infile, token, tokenize, tokenfile)
{
  if (is.character(infile)) {
    filename = infile
    if (tokenfile == "") tokenfile = paste0(infile,".rds")
  } else {
    filename = "<connection>"
  }
  if (is.character(infile)) {
    f = file(infile,"r")
  } else {
    f = infile
  }
  lines = readLines(f)
  if (is.character(infile)) close(f)
  output = character(0)
  if (tokenize) {
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
    c(output,"\n")
  } else {
    if (!file.exists(tokenfile)) stop("Token-file ",tokenfile," doesn't exist")
    token_list = readRDS(tokenfile)
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
    paste0(lines,"\n")
  }
}
