\name{AUBtutorial}
\alias{AUBtutorial}
\alias{AUBexercise}
\title{Aquatic Modelling Workshop: tutorials and exercises}
\usage{
AUBtutorial (x = c("aubergine", "courgette"))
    
AUBexercise(x = c("npzd", "riverAnoxia"), type = c("HTML", "PDF", "RMD", "WORD"))  
}
\arguments{
  \item{x }{the name or the number of the tutorial, exercise or reader. When x = "?", a list of tutorials, exercises or readers will be displayed, including a short description.
  }
  \item{type }{ how the exercise or reader should be displayed. When selecting PDF, you need to have LaTeX installed.
  }
}

\description{
The tutorials contain a link to the video(s) that introduce the workshop, followed by questions/quizzes (multiple choice) and small exercises to be written in R. 

The exercises are short tasks to be completed in 2 to 4 hours. 
}

\note{
The tutorials and exercises are created on the fly, i.e. they are stored in the package as R markdown files (extension .Rmd), and still need to be converted, so you may need to have some patience!

The answers to the exercises are also stored in the package, but we did not include a public function to extract them. However, you may use the private function AUBanswer to do so.
}
\examples{
\dontrun{
 AUBtutorial("aubergine")
 AUBtutorial(2)
 AUBexercise("npzd")
}

AUBtutorial ("?")
AUBexercise ("?")
}
