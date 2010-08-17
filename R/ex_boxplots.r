`ex_boxplots` <- function(eval=FALSE) {
	BoxplotFrame <- MainFrame <- Boxplot1 <- Exit <- NULL
	plotBoxplot <- function() {
    require(lattice)
		par(bg="white")
		
		if(Texists("choiceData"))	
				choiceData <- evalq(tclvalue(choiceData), env = as.environment(which(search()=="TGUIenv")))
		else 	choiceData <- 1			

		if(Texists("horiz"))	
				horiz <- evalq(tclvalue(horiz), env = as.environment(which(search()=="TGUIenv")))
		else 	horiz <- 1	
		
		if(horiz==2)
				horiz <- TRUE
		else 	horiz <- FALSE
		
		if(Texists("logar"))	
				logar <- evalq(tclvalue(logar), env = as.environment(which(search()=="TGUIenv")))
		else 	logar <- FALSE	
		
		if(logar==2) 	logar <- TRUE
		else 			logar <- FALSE		
	
		# Singer-Data
		if( choiceData == 1 ) {
			if(logar==TRUE)
				boxplot(log(1+singer[,1]) ~ singer[,2], main="Heights of New York Choral Society singers", xlab="grouped according to voice part", ylab="Height (inches)", col = "cornflowerblue", horizontal=horiz)
			
			else
				boxplot(singer[,1] ~ singer[,2], main="Heights of New York Choral Society singers", xlab="grouped according to voice part", ylab="Height (inches)", col = "cornflowerblue", horizontal=horiz)
		}			
		
		# Orchard-Sprays
		if( choiceData == 2 ) {
			if(logar==TRUE)
				boxplot(log(1+OrchardSprays[,1]) ~ OrchardSprays[,4], main="Potency of Orchard Sprays (Experiment with 100 bees)", ylab="decrease in volume of the solutions in the various cells", xlab = "concentrations of lime sulphur  in sucrose solution (A=highest, H=lowest)", col = "cornflowerblue", horizontal=horiz)
			
			else
				boxplot(OrchardSprays[,1] ~ OrchardSprays[,4], main="Potency of Orchard Sprays (Experiment with 100 bees)", ylab="decrease in volume of the solutions in the various cells", xlab = "concentrations of lime sulphur  in sucrose solution (A=highest, H=lowest)", col = "cornflowerblue", horizontal=horiz)
		}				
		# Insect Sprays	
		if( choiceData == 3 ) {
			if(logar==TRUE)
				boxplot(log(1+InsectSprays[,1]) ~ InsectSprays[,2], main="Effectiveness of Insect Sprays", xlab = "Type of spray", ylab = "Insect count", col = "cornflowerblue", horizontal=horiz)
			else
				boxplot(InsectSprays[,1] ~ InsectSprays[,2], main="Effectiveness of Insect Sprays", xlab = "Type of spray", ylab = "Insect count", col = "cornflowerblue", horizontal=horiz)
		}			
		# Random Boxplots
		if( choiceData == 4 ) {
			mat <- cbind(Uni05 = (1:100)/21, Norm = rnorm(100), T5 = rt(100, df = 5), Gam2 = rgamma(100, shape = 2))
			if(logar==TRUE) {
				mat <- apply(mat, 2, function(x) { log(1+abs(x)) }) # FIXME!
				boxplot(data.frame(mat), xlab="Distribution", main = "Random Data", col = "cornflowerblue", horizontal=horiz)
			}
			else
				boxplot(data.frame(mat), xlab="Distribution", main = "Random Data", col = "cornflowerblue", horizontal=horiz)
		}			
		# Tooth-Growth
		if( choiceData == 5 ) {
			if(logar==TRUE) {
				boxplot(log(1+len) ~ supp + dose, data=ToothGrowth, col=c("orange", "cornflowerblue"), main = "Guinea Pigs' Tooth Growth", xlab = "Vitamin C dose mg", ylab = "tooth length", horizontal=horiz)
				legend("topleft", c("Orange juice", "Ascorbic acid"), fill = c("orange", "cornflowerblue"))
			}
			else {
				boxplot(len ~ supp + dose, data=ToothGrowth, col=c("orange", "cornflowerblue"), main = "Guinea Pigs' Tooth Growth", xlab = "Vitamin C dose mg", ylab = "tooth length", ylim=c(0, 35), horizontal=horiz)
				legend("topleft", c("Orange juice", "Ascorbic acid"), fill = c("orange", "cornflowerblue"))
			}				
		}
		# States
		if( choiceData == 6 ) {
			data(state)
			if(logar==TRUE)
				boxplot( log(1+state.area) ~ state.region, xlab="Region", ylab="Dimension (in square miles)", main="US State Facts and Figures", ylim=c(0, 15), col="cornflowerblue", horizontal=horiz)
			else
				boxplot( state.area ~ state.region, xlab="Region", ylab="Dimension (in square miles)", main="US State Facts and Figures", ylim=c(0, 600000), col="cornflowerblue", horizontal=horiz)
		}		
	}
#	plotBoxplotEval <- function() {
#		par(bg="white")
#		mat <- cbind(Uni05 = (1:100)/21, Norm = rnorm(100), T5 = rt(100, df = 5), Gam2 = rgamma(100, shape = 2))
#		boxplot(data.frame(mat), main = "Random Data", horizontal=TRUE, col = "cornflowerblue")
#	}	
#	
	exerciseTitle <- "Example: Boxplots (interactive graphic)"
	q1 <- "Which statements are correct?"
	q2 <- ""
	labs <- c("You can read the standard deviation from a boxplot",
				"In a boxplot, the mean is always larger than the median",
				"A boxplot visualizes robust estimators for scale and location",
				"The height of the singers increases with the gravity of the voice (Data: singers)",
				"The largest states are located in the south of the United States (Data: state)",
				"Alaska is an outlier (Data: state)")
	notice <- "Multiple answers!"
	ex <- "ex_boxplots"	
	
	if(eval==FALSE) {
		OpenWindow(title=exerciseTitle)
		InteractivePlot(
				plot.function=plotBoxplot, 
				name=BoxplotFrame,
				header="Data Visualization - Boxplots",
				radio_header=c("Select a data set?", "Horizontal?", "Log."),
				radio=c("choiceData", "horiz", "logar"),
				radio_label=list(
						c(	"singer", "OrchardSprays", "InsectSprays", 
							"random", "ToothGrowth", "states"),
						c("no", "yes"),
						c("no", "yes")),
				radio_value=list(
						1:6,
						1:2,
						1:2),
				design="leftright", hscale=1.2, vscale=1.2
		)	
		
		MultipleChoice(frame=MainFrame, 
				Boxplot1,
				question1=q1,
				labels=labs,
				note=notice,
				filename=ex)	
		
		tkgrid(BoxplotFrame)
		tkgrid(Boxplot1)
		ExitButton(name=Exit)
		tkgrid(Exit)			
	}
	else {
		exerciseTitle <- paste("Evaluation - ", exerciseTitle, sep="")
		OpenWindow(title=exerciseTitle)
		InteractivePlot(
				plot.function=plotBoxplot, name=BoxplotFrame,
        header="Data Visualization - Boxplots",
        radio_header=c("Select a data set?", "Horizontal?", "Log."),
				radio=c("choiceData", "horiz", "logar"),
				radio_label=list(
						c(	"singer", "OrchardSprays", "InsectSprays", 
							"random", "ToothGrowth", "states"),
						c("no", "yes"),
						c("no", "yes")),
				radio_value=list(
						1:6,
						1:2,
						1:2),
				design="leftright", hscale=1.3, vscale=1.3
		)	
				
		MultipleChoice(frame=MainFrame, 
				Boxplot1,
				question1=q1,
				labels=labs,
				filename=ex,
				Answer=TRUE)	
		tkgrid(BoxplotFrame)
		tkgrid(Boxplot1)			
	}	
}