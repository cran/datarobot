% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Validate.R
\name{ValidateParameterIn}
\alias{ValidateParameterIn}
\title{Ensure a parameter is valid}
\usage{
ValidateParameterIn(paramValue, paramPossibilities, allowNULL = TRUE)
}
\arguments{
\item{paramValue}{object. The parameter value to check.}

\item{paramPossibilities}{vector. A vector of possible values for the parameter.}

\item{allowNULL}{logical. Whether or not to allow NULL as a possibility.}
}
\value{
TRUE if \code{paramValue} is valid, otherwise it raises an error.
}
\description{
A valid parameter \code{paramValue} is either NULL or in the space
of \code{paramPossibilities}.
}
\examples{
\dontrun{
  ValidateParameterIn("all", DataSubset)
}
}
