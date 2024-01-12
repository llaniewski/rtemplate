#' Divides the RT source into chunks
#'
#' Divides the RT source into chunks based on the <? and ?> markup
#'
#' @export
RTchunks = function(lines) {
  transf = function(x,open) {
    ret = lapply(seq_along(x), function(i) {
      p = x[[i]]
      if (p[1] == -1) return(NULL)
      ret = data.frame(line=i, start=p, end=p+attr(p,"match.length") - 1,open=open)
      ret$match = substring(lines[ret$line],ret$start,ret$end)
      ret$tag = trimws(substring(lines[ret$line],ret$start+2,ret$end))
      ret
    })
    do.call(rbind, ret)
  }

  x = gregexpr("<[?][^[:blank:]]*[[:blank:]]*", lines)
  y = gregexpr("[?]>", lines)
  x = transf(x,open=TRUE)
  y = transf(y,open=FALSE)
  tokens = rbind(x,y)
  if (is.null(tokens)) {
    chunks = data.frame(
      start.line = 1,
      start.char = 1,
      whole.start.char = 1,
      end.line = length(lines),
      end.char = nchar(lines[length(lines)]),
      whole.end.char = nchar(lines[length(lines)]),
      tag=""
    )
  } else {
    i = order(tokens$line,tokens$start)
    tokens = tokens[i,]

    check = cumsum(tokens$open*2-1)
    if (any(check > 1 | check < 0) | tail(check,1) != 0 ) stop ("Non matching <? and ?> found\n")
    tokens$edge = ifelse(tokens$open, tokens$start, tokens$end+1)
    chunks = data.frame(
      start.line = c(1,tokens$line),
      start.char = c(1,tokens$end+1),
      whole.start.char = c(1,tokens$edge),
      end.line = c(tokens$line,length(lines)),
      end.char = c(tokens$start-1,nchar(lines[length(lines)])),
      whole.end.char = c(tokens$edge-1,nchar(lines[length(lines)])),
      tag=c("",as.character(tokens$tag)),
      plain = c(TRUE, ! tokens$open)
    )
  }
  sel = (chunks$start.line == chunks$end.line) & (chunks$end.char - chunks$start.char < 0)
  chunks[!sel,]
}
