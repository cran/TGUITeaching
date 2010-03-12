`ex_errors` <- function(eval=FALSE) {
	exerciseTitle <- "Exercise: errors and pitfalls"
	q1 <- "In a well known Austrian newspaper the headline\n describing the graph from above was as follows:"
	q2 <- c("'62 PERCENT OF ALL MALES DID NOT MAKE THE GRADE!'",
			"What do you think is correct?")
	labs <- c("Headline and graph are completely correct and the information is well-arranged",
			"The headline matches the graph, however the title of the graph is misleading",
			"The headline is completely wrong with regard to the content of the graph")
	notice <- "It is possible to choose multiple answers!"
	ex <- "ex_errors"
	
	PiePlot <- function() {
		par(bg="white")
		pie(c(62,38), labels=c("Male", "Female"),init.angle=90,
				main="Students who did not make the grade\n (classified by sex)", cex=1.4,col=c("skyblue","pink"),
        sub="Source: www.ifas.jku.at")
		text(x=c(-0.45, 0.34), y=c(0.091, 0.20), label=c("62 %", "38 %"), cex=1.8)
	}
	
	if(eval==FALSE) {		
		OpenWindow(title=exerciseTitle)
		MultipleChoice(frame=MainFrame, 
				err1,
				question1=q1,
				question2=q2,
				labels=labs,
				plotFunction=PiePlot,
				note=notice,
				filename=ex)	
		tkgrid(err1)		
	}
	else {		
		OpenWindow(title=paste("Evaluation - ", exerciseTitle, sep=""))
		MultipleChoice(frame=MainFrame, 
				err1,
				question1=q1,
				question2=q2,
				labels=labs,
				plotFunction=PiePlot,
				filename=ex,Answer=TRUE)	
		tkgrid(err1)		
	}
}
