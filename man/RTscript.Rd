% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/RTemplate.R
\name{RTscript}
\alias{RTscript}
\title{Main sript for usage of RTemplate as a command-line tool}
\usage{
RTscript(args = commandArgs(trailingOnly = TRUE))
}
\description{
You just call it, and it takes arguments from command-line and does stuff.
an example shell script would look like this
}
\details{
"#!/bin/bash"
"R -e "rtemplate:RTscript()" --args $@"
}
