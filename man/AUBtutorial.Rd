\name{AUBtutorial}
\alias{AUBtutorial}
\title{Aquatic Modelling Workshop Tutorials}
\usage{
AUBtutorial (x =
  c("01-introduction",
    "02-chemostat",
    "03-npz-model",
    "04-npz-in-a-river",
    "05-npzd-model in a batch"
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

\note{
The tutorials are created on the fly, i.e. they are stored in the package as R markdown files (extension .Rmd), and still need to be converted, so you may need to have some patience!
}
\examples{
\dontrun{
 AUBtutorial("aubergine")
 AUBtutorial(2)
}
AUBtutorial ("?")
}
