findFileInDirs = function(file,dirs) {
  pot = c(file, if (length(dirs)>0) paste(dirs,file,sep="/") else NULL)
  sel = sapply(pot,file.exists)
  sel = which(sel)
  if (length(sel) < 1)
    stop("file not found:",file," in include directories:",paste(dirs,collapse=","))
  pot[sel[1]]
}

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
  "findFileInDirs =",
  deparse(findFileInDirs),
  "source = function(file,...) {",
  "  newfile=findFileInDirs(file, include.dir)",
  "  base::source(file=newfile,...)",
  "}",
  "add.include.dir = function(dir) {",
  "  if (substr(dir,1,1) != \"/\") dir = paste(getwd(),dir,sep=\"/\");",
  "  include.dir <<- c(include.dir,dir)",
  "}",
  "linemark=function(...) {invisible(NULL)}"
)

python.standards = c(
  "require(rjson,quietly=TRUE,warn.conflicts=FALSE)",
  "require(reticulate,quietly=TRUE,warn.conflicts=FALSE)",
  "python.run = function(str) py_run_string(paste(str,collapse=\"\\n\"))",
  "python.export.all = function() {",
    "python.myassign = function (var.name, value)",
  "{",
  "value <- try(rjson::toJSON(value),silent=TRUE)",
  "if (\"try-error\" %in% class(value)) return(invisible(NULL))",
  "var.name = gsub(\"[.]\",\"_\",var.name)",
  "python.command <- c(paste(var.name, \"='\", value, \"'\", sep = \" \"), ",
  "paste(var.name, \"= json.loads(\", var.name, \")\", sep = \"\"))",
  "python.command <- paste(python.command, collapse = \"\\n\")",
  "python.run(python.command)",
  "invisible(NULL)",
  "}",
  "for (i__ in ls(parent.frame())) python.myassign(i__,get(i__));",
  "}",
  "python.run(c(",
  "\"from io import StringIO\",",
  "\"import sys\",",
  "\"import json\"))"
)


combine.repeated.opt = function(obj, args, sep = "|") {
  opts <- obj@options
  opts <- opts[sapply(opts, function(x) x@type == "character")]
  for (opt in opts) {
    n <- length(args)
    values <- character(0)
    to.remove <- integer(0)
    x <- opt@short_flag
    if (!is.na(x)) {
      pattern <- paste0("^",x,"$")
      i <- grep(pattern, args)
      i <- i[i+1 <= n]
      values <- c(values, args[i+1])
      to.remove <- c(to.remove, c(i,i+1))
    }
    x <- opt@long_flag
    pattern <- paste0("^",x,"$")
    i <- grep(pattern, args)
    i <- i[i+1 <= n]
    values <- c(values, args[i+1])
    to.remove <- c(to.remove, c(i,i+1))
    pattern <- paste0("^",x,"=")
    i <- grep(pattern, args)
    values <- c(values,sub(pattern,"",args[i]))
    to.remove <- c(to.remove, c(i))
    if (length(values) > 1) {
      i = min(to.remove)-1
      args = args[-to.remove]
      n = length(args)
      newargs = c(opt@long_flag, paste0(values, collapse=sep))
      args = c(
        args[seq_len(i)],
        newargs,
        args[seq_len(n-i)+i]
      )
    }
  }
  args
}



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
RTscript = function(args = commandArgs(trailingOnly = TRUE)) {

  options <- list(
    make_option(c("-f","--file"), "store", default="", help="Input file", type="character"),
    make_option(c("-o","--out"), "store", default="", help="Output file", type="character"),
    make_option(c("-x","--kill"), "store_true", default=FALSE, help="Put the output in a file without \".Rt\" (make a \"dead\" file)"),
    make_option(c("-c","--code"), "store_true", default=FALSE, help="Generate R code"),
    make_option(c("-s","--shell"), "store_true", default=FALSE,help="Discart first '#!...' line"),
    make_option(c("-i","--include"), "store", default="", help="Include a .R file", type="character"),
    make_option(c("-I","--includedir"), "store", default="", help="Include directory", type="character"),
    make_option(c("-w","--workdir"), "store", default="", help="Set working directory", type="character"),
    make_option(c("-q","--quiet"), "store_true", default=FALSE,help="Quiet (print only errors)"),
    make_option(c("-t","--csv"), "store", default="", help="Read csv. use: \"-t example.csv:3\" the 3 record of example.csv", type="character"),
    make_option(c("-b","--code-fallback"), "store_true", default=FALSE, help="Fallback to code on error"),
    make_option(c("-k","--keep-code"), "store_true", default=FALSE,  help="Keep the generated .R file"),
    make_option(c("-l","--mark-lines"), "store_true", default=FALSE, help="Map lines of input to output (usefull for error marking in C)"),
    make_option(c("-p","--profile"), "store_true", default=FALSE, help="Run profiling"),
    make_option(c("--token"), "store",  default="__RT_*_RT__", help="Token format", type="character"),
    make_option(c("--token-file"), "store",  default="", help="Token file", type="character"),
    make_option(c("--tokenize"), "store_true", default=FALSE, help="Convert RT chunks to tokens"),
    make_option(c("--detokenize"), "store_true", default=FALSE, help="Convert tokens back to RT chunks"),
    make_option(c("--relative-to"), "store", default="", help="The path (in marklines) should be relative to this (default: out)", type="character")
  )
  parser = OptionParser(usage="Usage: RT [-x] -f inputfile [-o outputfile]", options)

  args = combine.repeated.opt(parser,args)
  opt <- parse_args(parser, args=args, positional_arguments=TRUE)
  args = opt$args
  opt = opt$options

  if (opt$file == "") stop("Input file not specified\nUsage: RT -f file\n");

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
      if (opt$code) {
	opt$out = paste(opt$out,".R",sep="");
	opt$codeout = opt$out;
      }
    } else {
      cat("Warning: output file name provided while killing\n");
    }
  }

  if (is.null(opt$out)) {
    if (opt$'keep-code' || opt$"code-fallback") {
      cat("Warning: output file name not provided but code requested\n");
      opt$'keep-code' = FALSE
      opt$"code-fallback" = FALSE
    }
  }	else {
    if (opt$code) {
      opt$codeout = opt$out;
    } else {
      opt$codeout = paste0(opt$out,".code.R");
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

  if (opt$'relative-to' == "") opt$'relative-to' = opt$out
  if (!is.null(opt$'relative-to')) {
    opt$relative = relativePath(opt$file, opt$'relative-to')
  } else {
    opt$relative = opt$file
  }
  if (!is.null(opt$out) && !(opt$quiet))
    cat("Parsing RT:",opt$file, "->", opt$out," (rel:",opt$relative,")\n");

  addcode = NULL

  if(opt$csv != "")
  {
    #        cat("Getting params from",opt$csv,"\n");
    csvs = strsplit(opt$csv,"[,|]")[[1]]
    csvs = strsplit(csvs,":")
    for (csv in csvs) {
      ind = "";
      if (length(csv) == 2) {ind = as.integer(csv[2]); csv = csv[1]; }
      if (length(csv) >  2) stop("Wrong format -t argument: file:record");
      addcode = c(addcode,
                  paste0("csvfile = \"", encodeString(csv), "\";"),
                  "tab = read.csv(csvfile);",
                  "tab[] = lapply(tab, function(x) if (all(x %in% c('True','TRUE','False','FALSE'))) as.logical(x) else x);",
                  paste0("tab = tab[", ind, ",,drop=FALSE]"),
                  "for (n in names(tab)) assign(n,tab[,n]);"
      )
    }
  }

  if (length(args) > 0) {
    #	cat("Evaluating commandline arguments\n");
    #	argp = parse(text=args)
    #	eval(argp,e)
    addcode = c(addcode,
                args
    )
  }

  opt$includedir = strsplit(opt$includedir,"[,|]")[[1]]
  if (length(opt$includedir) > 0) {
    addcode = c(addcode, paste("add.include.dir(\"",opt$includedir,"\")",sep=""))
  }


  if (opt$workdir != "") {
    addcode = c(addcode, paste("setwd(\"",opt$workdir,"\")",sep=""))
  }


  opt$include = strsplit(opt$include,"[,|]")[[1]]
  if (length(opt$include) > 0) {
    addcode = c(addcode, paste("source(\"", opt$include, "\")",sep=""))
  }

  if (opt$tokenize | opt$detokenize) {
    if (opt$tokenize & opt$detokenize) stop("cannot tokenize and detokenize at the same time")
    lines = RTtokenize(opt$file, tokenize=opt$tokenize, token=opt$token, tokenfile=opt$'token-file')
    if (is.null(opt$out)) {
      writeLines(lines, sep="");
    } else {
      writeLines(lines, con=opt$out, sep="");
    }
  } else {

    code = RTfile(opt$file, add=addcode, shell=opt$shell, mark.lines=opt$"mark-lines",filename=opt$relative, includedirs=opt$includedir)

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
      if (opt$'keep-code') {
        writeLines(code, con=opt$codeout);
      }
      if (opt$profile) {
        Rprof(paste0(opt$profile.file,".parse.Rprof"))
      }
      code.p = try(parse(text=code))
      if (inherits(code.p,"try-error")) {
        if (opt$"code-fallback") {
          writeLines(code, con=opt$codeout);
          stop("Failed to parse: writen code to ", opt$codeout)
        } else {
          stop("Failed to parse R code")
        }
      }
      if (opt$profile) {
        Rprof(NULL)
        Rprof(paste0(opt$profile.file,".run.Rprof"))
      }
      old.wd = getwd()
      if (! is.null(opt$out)) sink(opt$out);
      eval.res = try(eval(code.p, globalenv()))
      if (! is.null(opt$out)) sink()
      setwd(old.wd)
      if (inherits(eval.res,"try-error")) {
        if (opt$"code-fallback") {
          writeLines(code, con=opt$codeout);
          stop("Failed to execude R code: writen code to ", opt$codeout)
        } else {
          stop("Failed to execude R code")
        }
      }
      if (opt$profile) {
        Rprof(NULL)
      }
    }
  }
}
