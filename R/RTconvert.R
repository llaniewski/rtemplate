#' Interpret a RT file
#'
#' Takes a RT file and generates R source.
#'
#' @export
RTfile = function(infile, ..., shell=FALSE, filename)
{
  if (missing(filename)) {
    if (is.character(infile)) {
      filename = infile
    } else {
      filename = "<connection>"
    }
  }
  if (is.character(infile)) {
    f = file(infile,"r")
  } else {
    f = infile
  }
  lines = readLines(f)
  if (is.character(infile)) close(f)

  if (shell) {
    if (substr(lines[1],1,2) == "#!") {
      lines = lines[-1]
    }
  }
  RTconvert(lines, filename=filename, ...);
}

RT.linemark = function(i,filename="",reset=FALSE) {
  if (filename != "") {
    paste0("linemark(",i,",\"",filename,"\",reset=",reset,")")
  } else {
    paste0("linemark(",i,",reset=",reset,")")
  }
}

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
RTconvert = function(lines, add=c(), mark.lines=FALSE, filename="")
{
  chunks = RTchunks(lines)

  output = c(
    "# RTemplate genereted code.",
    "############# RT standard functions ########",
    "",
    RT.standards,
    "",
    "############# Parameters and settings ######",
    "",
    add,
    "",
    "############# Code from Rt file ############",
    ""
  )
  loaded.rpython = FALSE
  if (mark.lines) output = c(output, RT.linemark(1,filename))
  for (i in 1:nrow(chunks))
  {
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
    if (tag == "") {
      if (n$start.line != n$end.line)
        lu[2:length(lu) - 1] = paste(lu[2:length(lu) - 1],"\n",sep="")
      outadd = paste("cat( ", encodeString(lu,quote="\"")," );",sep="")
      if (n$start.line != n$end.line)
        if (mark.lines) {
          outadd = rep(outadd,each=2)
          outadd[seq(1,length(outadd),2)] = RT.linemark(n$start.line:n$end.line, filename)

#          outadd = c(outadd[1], RT.linemark(n$start.line+1,filename),outadd[-1])
        }
    } else if (tag == "R") {
      outadd = lu;
      if (n$start.line != n$end.line) if (mark.lines) {
        outadd = c(RT.linemark(n$start.line, filename, reset=TRUE), outadd)
#        outadd = rep(outadd,each=2)
#        outadd[seq(1,length(outadd),2)] = RT.linemark(n$start.line:n$end.line, filename)
      }
    } else if (tag == "python") {
      if (! loaded.rpython) {
        output = c(output,python.standards)
        loaded.rpython = TRUE
      }
      if (substr(lu[1],1,1) == " ") lu[1] = substr(lu[1],2,nchar(lu[1]))

      outadd = c(
        "python.export.all();",
        "python.run(c(",
        "\"old_stdout = sys.stdout\",",
        "\"sys.stdout = mystdout = StringIO()\",",
        paste0(encodeString(lu,quote="\""),","),
        "\"sys.stdout = old_stdout\"))",
        "cat(py_eval(\"mystdout.getvalue()\"))",
        "invisible(py_eval(\"mystdout.close()\"))")
    } else if (substr(tag,1,1) == "%") {
      if (length(lu) > 1) stop("'%' tag allowed only for one-line expressions");
      outadd = paste("cat(sprintf(",encodeString(tag,quote="\""),",  ", lu, "  ));",sep="");
    } else { warning("Unknown tag",tag,"\n");
             if (n$start.line != n$end.line)
               lu[2:length(lu) - 1] = paste(lu[2:length(lu) - 1],"\n",sep="")
             outadd = c(
               paste("cat( ", encodeString(paste("<?",tag,sep=""),quote="\"")," );",sep="") ,
               paste("cat( ", encodeString(lu,quote="\"")," );",sep=""),
               paste("cat( ", encodeString("?>",quote="\"")," );",sep="")
             )

    }
    output = c(output,outadd)
  }
  outadd = "cat(\"\\n\")"
  output = c(output,outadd)
  if (mark.lines) output = c(output,RT.linemark(-1,filename))
  output
}
