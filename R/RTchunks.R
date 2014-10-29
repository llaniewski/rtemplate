#' Divides the RT source into chunks
#'
#' Divides the RT source into chunks based on the <? and ?> markup
#'
#' @export
RTchunks = function(lines) {
  x = gregexpr("<[?][^[:blank:]]*", lines)
  y = gregexpr("[?]>", lines)

  transf = function(x) {
    r = data.frame(line=c(),start=c(),end=c())
    for (i in seq_along(x))
    {  p = x[[i]]
       if (p[1] != -1)
       {
         tags = substring(lines[i],p+2,p+attr(p,"match.length")-1)
         r = rbind(r,
                   data.frame(line=i,start=p, end=p+attr(p,"match.length"),tag=tags)
         )
       }
    }
    r
  }

  x = transf(x)
  if (nrow(x) > 0) {
    x$open=T
    y = transf(y)
    y$open=F
    tokens = rbind(x,y)
    #                print(tokens)
    i = order(tokens$line,tokens$start)
    tokens = tokens[i,]

    check = cumsum(tokens$open*2-1)
    if (any(check > 1 | check < 0) ) stop ("Non matching <? and ?> found\n")

    chunks = data.frame(
      start.line = c(1,tokens$line), start.char = c(1,tokens$end),
      end.line = c(tokens$line,length(lines)), end.char = c(tokens$start-1,nchar(lines[length(lines)])), tag=c("",as.character(tokens$tag)) )
  } else {chunks = data.frame(
    start.line = 1, start.char = 1,
    end.line = length(lines), end.char = nchar(lines[length(lines)]), tag="")
  }
  chunks
}
