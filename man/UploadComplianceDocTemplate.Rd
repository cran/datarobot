% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ComplianceDocumentation.R
\name{UploadComplianceDocTemplate}
\alias{UploadComplianceDocTemplate}
\title{Upload a compliance doc template.}
\usage{
UploadComplianceDocTemplate(name, filename = NULL, sections = NULL)
}
\arguments{
\item{name}{character. A name to identify the compliance doc template by.}

\item{filename}{character. Optional. Filename of file to save the compliance doc template to.}

\item{sections}{list. Optional. Section definitions for the compliance template.}
}
\value{
Nothing returned, but uploads the compliance doc template.
}
\description{
The structure of the compliance doc template can be specified by either a file specified by
\code{filename} or by specifying it with a list via \code{sections}.
}
\examples{
\dontrun{
 ## Create a compliance documentation template from uploading a file
 DownloadComplianceDocTemplate("template.json")
 # Edit template.json in your favorite editor
 UploadComplianceDocTemplate("myTemplate", "template.json")

## Create a compliance documentation template from a list.
sections <- list(list("title" = "Missing Values Report",
                      "highlightedText" = "NOTICE",
                      "regularText" = paste("This dataset had a lot of Missing Values.",
                                            "See the chart below: {{missingValues}}"),
                      "type" = "user"),
                 list("title" = "Blueprints",
                      "regularText" = "{{blueprintDiagram}} /n Blueprint for this model",
                      "type" = "user"))
}
}
