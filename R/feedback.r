feedback <- function(eval=FALSE){
	exerciseTitle <- "Course Feedback"
	qu1 <- "Please help us to improve the course"
	qu2 <- c("Do you have questions?",
			"Which topic is especially interesting for you?",
			"How was the organisation of the course?")
	fileExample <- "feedback"
	OpenWindow(title=exerciseTitle)
	if(!eval){
		Feedback(frame=MainFrame, 
				name=feed1,
				question1=qu1,
				question2=qu2,
				filename=fileExample)	
	}
	else{
		FeedbackAnswer(frame=MainFrame, 
				name=feed1,
				question1=qu1,
				question2=qu2,
				filename=fileExample)
	}
	tkgrid(feed1)	
}