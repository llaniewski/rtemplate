
RT.standards = c(
  "printf = function(...) {",
  "  cat(sprintf(...))",
  "}",
  "quiet.source = function(..., comment=\"# \") {",
  "  f=textConnection(\"quiet.source.text\",\"w\");",
  "  sink(f); ret=source(...); sink();",
  "  close(f);",
  "  cat(paste(comment,quiet.source.text,\"\n\",sep=\"\"),sep=\"\")",
  "  ret",
  "}",
  "if (!exists(\"include.dir\")) include.dir=NULL;",
  "source = function(file,...) {",
  "  pot = c(file, paste(include.dir,file,sep=\"/\"))",
  "  sel = sapply(pot,file.exists)",
  "  sel = which(sel)",
  "  if (length(sel) < 1)",
  "    stop(\"file not found:\",file,\" in include directories:\",paste(include.dir,collapse=\",\"))",
  "  newfile=pot[sel[1]]",
  "  base::source(file=newfile,...)",
  "}",
  "add.include.dir = function(dir) {",
  "  if (substr(dir,1,1) != \"/\") dir = paste(getwd(),dir,sep=\"/\");",
  "  include.dir = c(include.dir,dir)",
  "}",
  "linemark=function(...) {invisible(NULL)}"
)

python.standards = c(
  "require(rjson,quietly=TRUE,warn.conflicts=FALSE)",
  "require(RJSONIO,quietly=TRUE,warn.conflicts=FALSE)",
  "require(rPython,quietly=TRUE,warn.conflicts=FALSE)",
  "python.export.all = function() {",
    "python.myassign = function (var.name, value)",
  "{",
  "value <- try(rjson::toJSON(value),silent=TRUE)",
  "if (\"try-error\" %in% class(value)) return(invisible(NULL))",
  "var.name = gsub(\"[.]\",\"_\",var.name)",
  "python.command <- c(paste(var.name, \"='\", value, \"'\", sep = \" \"), ",
  "paste(var.name, \"= json.loads(\", var.name, \")\", sep = \"\"))",
  "python.command <- paste(python.command, collapse = \"\\n\")",
  "python.exec(python.command)",
  "invisible(NULL)",
  "}",
  "for (i__ in ls(parent.frame())) python.myassign(i__,get(i__));",
  "}")


#' Main sript for usage of RTemplate as a command-line tool
#'
#' You just call it, and it takes arguments from command-line and does stuff.
#' an example shell script would look like this
#'
#' "#!/bin/bash"
#' "R -e "rtemplate:RTscript()" --args $@@"
#'
#' @export
#' @import optparse
RTscript = function() {

  options <- list(
    make_option(c("-f","--file"), "store", default="", help="Input file", type="character"),
    make_option(c("-o","--out"), "store", default="", help="Output file", type="character"),
    make_option(c("-x","--kill"), "store_true", default=FALSE, help="Put the output in a file without \".Rt\" (make a \"dead\" file)"),
    make_option(c("-d","--destroy"), "store_true", default=FALSE, help="Delete the orginal template"),
    make_option(c("-e","--exterminate"), "store_true", default=FALSE, help="Kill and Destroy"),
    make_option(c("-c","--code"), "store_true", default=FALSE, help="Generate R code"),
    make_option(c("-s","--shell"), "store_true", default=FALSE,help="Discart first '#!...' line"),
    make_option(c("-i","--include"), "store", default="", help="Include a .R file", type="character"),
    make_option(c("-I","--includedir"), "store", default="", help="Include directory", type="character"),
    make_option(c("-w","--workdir"), "store", default="", help="Set working directory", type="character"),
    make_option(c("-q","--quiet"), "store_true", default=FALSE,help="Quiet (print only errors)"),
    make_option(c("-t","--csv"), "store", default="", help="Read csv. use: \"-t example.csv:3\" the 3 record of example.csv", type="character"),
    make_option(c("-b","--code-fallback"), "store_true", default=FALSE, help="Fallback to code on error"),
    make_option(c("-l","--mark-lines"), "store_true", default=FALSE, help="Map lines of input to output (usefull for error marking in C)"),
    make_option(c("-p","--profile"), "store_true", default=FALSE, help="Run profiling")
  )

  opt <- parse_args(OptionParser(usage="Usage: RT [-x] -f inputfile [-o outputfile]", options), positional_arguments=TRUE)
  args = opt$args
  opt = opt$options

  if (opt$file == "") stop("Input file not specified\nUsage: RT -f file\n");
  if (opt$exterminate) { opt$kill = T; opt$destroy = T; }

  if (opt$out == "") opt$out = NULL
  if (opt$kill) {
    if (is.null(opt$out))
    {	re = "[.]R[tT][^.]*$";
      if (!grepl(re, opt$file))
      {
        cat("Warning: file not ending with .Rt* while killing - generating .dead file\n")
        opt$out = paste(opt$file,"dead",sep=".");
      } else {
        opt$out = sub(re,"",opt$file);
      }
      if (opt$code) opt$out = paste(opt$out,".R",sep="");
    } else {
      cat("Warning: output file name provided while killing\n");
    }
  }

  if (opt$profile) {
    if (is.null(opt$out)) {
      opt$profile.file = opt$file
    } else {
      opt$profile.file = opt$out
    }
  }
  if (opt$profile) {
    Rprof(paste0(opt$profile.file,".RT.Rprof"))
  }
  if (!is.null(opt$out) && !(opt$quiet))
    cat("Parsing RT:",opt$file, "->", opt$out,"\n");

  addcode = NULL
  e = new.env()

  if(opt$csv != "")
  {
    #        cat("Getting params from",opt$csv,"\n");
    csv = strsplit(opt$csv,":")
    csv = csv[[1]];
    ind = 1;
    if (length(csv) == 2) {ind = as.integer(csv[2]); csv = csv[1]; }
    if (length(csv) >  2) stop("Wrong format -t argument: file:record");
    addcode = c(addcode,
                paste("csvfile = \"", encodeString(csv), "\";",sep=""),
                paste("record = ", ind, ";",sep=""),
                "tab = read.csv(csvfile);",
                "for (n in names(tab))",
                "assign(n,tab[record,n]);"
    )
  }

  if (length(args) > 0) {
    #	cat("Evaluating commandline arguments\n");
    #	argp = parse(text=args)
    #	eval(argp,e)
    addcode = c(addcode,
                args
    )
  }

  if (opt$includedir != "") {
    opt$includedir = strsplit(opt$includedir,",")[[1]]
    addcode = c(addcode, paste("add.include.dir(\"",opt$includedir,"\")",sep=""))
  }


  if (opt$workdir != "") {
    addcode = c(addcode, paste("setwd(\"",opt$workdir,"\")",sep=""))
  }


  if (opt$include != "") {
    opt$include = strsplit(opt$include,",")[[1]]
    addcode = c(addcode, paste("source(\"", opt$include, "\")",sep=""))
  }

  code = RTfile(opt$file, add=addcode, shell=opt$shell, mark.lines=opt$"mark-lines")

  if (opt$profile) {
    Rprof(NULL)
  }

  if (opt$code) {
    if (is.null(opt$out)) {
      writeLines(code);
    } else {
      writeLines(code, con=opt$out);
    }
  } else {
    if (opt$profile) {
      Rprof(paste0(opt$profile.file,".parse.Rprof"))
    }
    code.p = try(parse(text=code))
    if (class(code.p) %in% "try-error") {
      if (opt$"code-fallback") {
        if (!is.null(opt$out)) {
          fn = paste0(opt$out,".parse-failed.R")
          writeLines(code, con=fn)
          stop("Failed to parse: writen code to ", fn)
        } else {
          stop("Failed to parse: no output file provided to dump code")
        }
      } else {
        stop("Failed to parse R code")
      }
    }
    if (opt$profile) {
      Rprof(NULL)
      Rprof(paste0(opt$profile.file,".run.Rprof"))
    }
    if (! is.null(opt$out)) sink(opt$out);
    eval(code.p, globalenv())
    if (! is.null(opt$out)) sink()
    if (opt$profile) {
      Rprof(NULL)
    }
  }

  if (opt$destroy)
  {
    cat("Should destroy:\n");
    cat("rm",opt$file,"\n");
  }
}
