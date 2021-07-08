

AUBtutorial <- function(x = c(
    "01-introduction", 
    "02-chemostat", 
    "03-npzd-model",
    "04-npz-in-a-river",
    "bacterial-kinetics")) {

  LL <- as.character(formals(AUBtutorial)$x[-1])

  if (x == "?") {
    tutorial <- data.frame(x=LL, description = c("About the authors and modelling",
      "Dynamic modelling in R"))
    return(tutorial)
  } else {
   if (is.character(x))
     Which <- LL[pmatch(tolower(x), LL)]
   else
     Which <- LL[x]
   if (length(Which) > 1)
     for (w in Which)
      run_tutorial(w, package = "aubergine")
   else
      run_tutorial(Which, package = "aubergine")
  }
}

openRmdFile <- function(file, type) {
  
  if (type == "RMD")
     browseURL(file)
  else if (type == "PDF")
     browseURL(rmarkdown::render(input = file, output_format = "pdf_document",
                       output_file=tempfile(fileext = ".pdf")))
  else if (type == "HTML")
     browseURL(rmarkdown::render(input = file, output_format = "html_document",
                       output_file=tempfile(fileext = ".html")))
  else if (type == "WORD")
     browseURL(rmarkdown::render(input = file, output_format = "word_document",
                       output_file=tempfile(fileext = ".doc")))
}


AUBexercise <- function(x = c("npzd", "riverAnoxia"), 
    type = c("HTML", "PDF", "RMD", "WORD")) {

  LL <- as.character(formals(AUBexercise)$x[-1])
  type <- match.arg(toupper(type), choices=c("HTML", "PDF", "RMD", "WORD"))

  if (x == "?") {
    exercise <- data.frame(x=LL, description = c( 
      "NPZD model (marine ecosystem model)",
      "Anoxia in a river (1-D reaction transport model)"))
    return(exercise)
  } else {
   if (is.character(x))
     Which <- LL[pmatch(tolower(x), tolower(LL))]
   else
     Which <- LL[x]
   Which <- paste(Which, "/", Which,"_Q", sep="")

  # The files are stored in RMD format 
   RMD <- paste0(system.file('exercises', package = 'aubergine'),"/",Which, ".Rmd", sep="")
   
   if (length(RMD) > 1) {
     for (file in RMD) openRmdFile(file, type)
   } else openRmdFile(RMD, type)
  }   
}

# A private function - to be used as aubergine:::AUBanswer
AUBanswer <- function(x = c("npzd", "riverAnoxia"), 
    type = c("HTML", "PDF", "RMD", "WORD")) {

  LL <- as.character(formals(AUBexercise)$x[-1])
  type <- match.arg(toupper(type), choices=c("HTML", "PDF", "RMD", "WORD"))

  if (x == "?") {
    exercise <- data.frame(x=LL, description = c( 
      "NPZD model (marine ecosystem model)",
      "Anoxia in a river (1-D reaction transport model)"
      ))
    return(exercise)
  } else {
   if (is.character(x))
     Which <- LL[pmatch(tolower(x), tolower(LL))]
   else
     Which <- LL[x]
   Which <- paste(Which, "/", Which, sep="")  # THIS DIFFERS FROM AUBexercise

  # The files are stored in RMD format 
   RMD <- paste0(system.file('exercises', package = 'aubergine'),"/",Which, ".Rmd", sep="")
   
   if (length(RMD) > 1) {
     for (file in RMD) openRmdFile(file, type)
   } else openRmdFile(RMD, type)
  }   
}

