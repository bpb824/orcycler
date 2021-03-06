% Generated by roxygen2 (4.1.1): do not edit by hand
% Please edit documentation in R/userAnalysis.R
\name{userAnalysis}
\alias{userAnalysis}
\title{User Analysis}
\usage{
userAnalysis(db = "test", db_path = "source_data/db_credentials.json")
}
\arguments{
\item{db}{String indicating which db to use, default is "test", also can be "release"}

\item{db_path}{Absolute or relative path to db_credentials.json file.}
}
\value{
User analysis results
}
\description{
Initializes database connection and calls series of functions used to analyze ORcycle user data.
}

