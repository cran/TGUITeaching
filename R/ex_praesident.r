ex_praesident <- function(eval=FALSE) {
  exerciseTitle <- "Simple Single Choice Question"; 
  fileEx <- "ex_praesident"
  q1 <- "Who is the president of the United States?"
  labs <- c("Linus Torvalds", "B. Obama", "A. Schwarzenegger")
  imagePath <- "bp"
  if(eval==FALSE) {	
    OpenWindow(title=exerciseTitle)
    SingleChoice(
        name=F1, 
        question1=q1,
        labels=labs, 
        image=imagePath,
        filename=fileEx)	
    tkgrid(F1)		
  } 
  else {
    OpenWindow(title=paste("Evaluation - ", exerciseTitle, sep=""))
    SingleChoice(
        name=F1, 
        question1=q1,
        labels=labs, 
        image=imagePath,Answer=TRUE,
        filename=fileEx)	
    tkgrid(F1)		
  }
}