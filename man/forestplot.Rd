% Generated by roxygen2 (4.1.1): do not edit by hand
% Please edit documentation in R/forestplot.R
\name{forestplot}
\alias{forestplot}
\title{forestPlot}
\usage{
forestplot(model, xlab = "Odds Ratio", ylab = "Independent Variable", title,
  outputDir, rownames = NULL, xRng = NULL)
}
\arguments{
\item{model}{Cumulative logistic regression model object}

\item{xlab}{X-axis label, default = "Odds Ratio"}

\item{ylab}{Y-Axis label, default = "Independent Variable"}

\item{title}{Plot title}

\item{outputDir}{Where to plot the .png output}

\item{rownames}{Optional row names parameter}

\item{xRng}{X-axis range}
}
\value{
None
}
\description{
Create a forest plot to visualize cumulative logistic regression model coefficients
}

