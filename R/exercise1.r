exercise1 <- function(eval=FALSE) {
  exerciseTitle <- "Single-Choice Example"; 
  fileEx <- "exercise1"
  q1 <- "Who is the president of the United States?"
  q2 <- "If you do not know, who want you to be?"
  labs <- c("H. Fischer", "B. Obama", "A. Schwarzenegger")
  imagePath <- "bp"
  if(eval==FALSE) {	
    OpenWindow(title=exerciseTitle)
    SingleChoice(
        name=F1, 
        question1=q1,
        question2=q2,
        labels=labs, 
        image=imagePath,
        filename=fileEx)	
    tkgrid(F1)		
  } 
  else {
    OpenWindow(title=paste("Evaluation - ", exerciseTitle, sep=""))
    SingleChoiceAnswer(
        name=F1, 
        question1=q1,
        question2=q2,
        labels=labs, 
        image=imagePath,
        filename=fileEx)	
    tkgrid(F1)		
  }
}