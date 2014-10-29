#' Interpret a RT file
#'
#' Takes a RT file and generates R source.
#'
#' @export
RTfile = function(infile, ..., shell=FALSE)
{
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
  RTconvert(lines, ...);
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
RTconvert = function(lines, add=c())
{
  chunks = RTchunks(lines)


  #print(chunks)


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
    } else if (tag == "R") {
      outadd = lu;
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
  output
}
