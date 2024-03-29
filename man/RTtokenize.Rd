% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/RTtokenize.R
\name{RTtokenize}
\alias{RTtokenize}
\title{Change RT chunks into tokens}
\usage{
RTtokenize(
  infile = "stdin",
  token = "__RT_*_RT__",
  inverse = FALSE,
  tokenfile = "tmp.rds",
  outfile = stdout(),
  delete_tokenfile = TRUE
)
}
\description{
The function takes input file and changes all RT blocks into tokens. This can be reversed using inverse=TRUE
}
