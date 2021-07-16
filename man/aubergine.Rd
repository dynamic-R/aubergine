\name{aubergine-package}
\alias{aubergine}
\title{Modelling Workshop - tutorials}
\description{
This package contains a number of tutorials and exercises, to accompany the
(virtual) workshop on dynamic modelling, 16 July 2021.

Thomas Petzoldt and Karline Soetaert:
Numerical simulation of dynamic systems with R: an introduction.

Organized by \href{https://aquaticdatasciopensci.github.io/}{AEMON-J}

The package contains interactive tutorials that can be run on recent versions
of RStudio or a shiny server. Independent of this, static versions of the tutorials
and additional material are also included as package vignettes.
}


\examples{
# To have a list of all available tutorials:
AUBtutorial("?")
\dontrun{
AUBtutorial(2)    # start tutorial 2

# show tutorials in the browser
browseURL(system.file("tutorials/", package="aubergine"))

# show all vignettes in the browser
browseVignettes(package="aubergine")
}
}

