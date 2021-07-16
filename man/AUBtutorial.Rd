\name{AUBtutorial}
\alias{AUBtutorial}
\title{Aquatic Modelling Workshop Tutorials}
\usage{
AUBtutorial (x =
  c("01-introduction",
    "02-chemostat",
    "03-npz-model",
    "04-npz-in-a-river",
    "05-npzd-model"
  )
)


}
\arguments{
  \item{x }{the name or the number of the tutorial, exercise or reader.
  When x = "?", a list of tutorials will be displayed, including a short description.
  }
}

\description{
The tutorials contain a link to the video(s) that introduce the workshop, followed by questions/quizzes (multiple choice) and small exercises to be written in R.
}

\details{The package contains interactive tutorials that can be run on recent versions
of RStudio or a shiny server. Independent of this, static versions of the tutorials
and additional material are also included as package vignettes.}

\note{
The tutorials are created on the fly, i.e. they are stored in the package as R markdown files (extension .Rmd), and still need to be converted, so you may need to have some patience!
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
