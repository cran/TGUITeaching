ex_feedback <- function(eval=FALSE) {
	MainFrame <- feedb <- NULL
  exerciseTitle <- "Feedback: Imputation"
  q1 <- "YOUR OPINION COUNTS! \n \n Please answer the following questions in keywords:"
  q2 <- c("Do you think that important questions about 'imputation' have not been answered (if so, which)?",
      "Do you think that certain topics should have been presented in more detail? (if so, which)?",
      "Have you ever used imputation methods in your job? (if so which and for what purpose)?"
  )
  fb <- "feedbackEval"
  OpenWindow(title=exerciseTitle)
  if(!eval){
    Feedback(frame=MainFrame, 
        name=feedb,
        question1=q1,
        question2=q2,
        filename=fb)	
  }
  else{
    Feedback(frame=MainFrame, 
        name=feedb,
        question1=q1,
        question2=q2,
		Answer=TRUE,
        filename=fb)
  }
  tkgrid(feedb)
} 