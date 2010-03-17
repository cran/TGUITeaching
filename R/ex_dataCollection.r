`ex_dataCollection` <- function() {
	OnOK <- function() {
		correct <- TRUE
		name <- tclvalue(anonym)
		msg <- NULL
		if(name=="Anonym") {
			correct <- FALSE	
			msg <- paste(msg, "Please choose a different name!\n", sep="")
		}			
		
		sex <- tclvalue(sex)
		
		height <- tclvalue(height)
		if(as.integer(height) < 50 | as.integer(height) > 210) {
			correct <- FALSE
			msg <- paste(msg, "Please check your statements (height)!!\n", sep="")
		}
		
		shoeSize <- tclvalue(shoeSize)
		if(as.integer(shoeSize) < 20 | as.integer(shoeSize) > 55) {
			correct <- FALSE
			msg <- paste(msg, "Please check your statements (shoeSize)!!\n", sep="")
		}		
		
		
		TravelHour <- tclvalue(timeHH)
		TravelMinutes <- tclvalue(timeMM)		
		TravelTime <- as.numeric(TravelHour)*60 + as.numeric(TravelMinutes)
		if(TravelTime < 0 | TravelTime > 600) {
			correct <- FALSE
			msg <- paste(msg, "Please check your statements (time)!!", sep="")
		}
		
		randExp <- tclvalue(randExp)
		if(as.integer(randExp) < 0 | as.integer(randExp) > 5) {
			correct <- FALSE
			msg <- paste(msg, "Please check your statements (randExp)!!\n", sep="")
		}	
		
		if(correct == TRUE) {
			erg <- data.frame( 
					name = substring(name, 1, 9), 
					sex = sex,
					height = as.character(as.numeric(height)),
					shoeSize=as.character(as.numeric(shoeSize)),
					TravelTime=as.character(round(as.numeric(TravelTime))),
					randExp = as.character(randExp), 
					kurs="Training Session"
			)
			write.table(erg, file="data.txt", append=TRUE, col.names=FALSE, row.names=FALSE)
			tkdestroy(tt)
			msg <- paste("Your data has been transmitted!")
			tkmessageBox(message=msg)			
		}
		else {
			print(msg)
			tkmessageBox(message=msg)
		}
	}
	
	OnCancel <- function() {
		tkdestroy(tt)
		msg <- paste("Your data has not been transmitted!")
		tkmessageBox(message=msg)
	}
	
	OpenWindow(title="Data collection for a basic statistic course")
	
	anonym 			<- tclVar(paste("Anonym",sample(seq(100),1),sep=""))
	sex 			<- tclVar("female")
	height 			<- tclVar(180)
	shoeSize 		<- tclVar(40)
	timeHH			<- tclVar(0)
	timeMM			<- tclVar(50)	
	randExp 		<- tclVar(3)
		
	fontLarge 	<- setFont(size="extralarge", italic=TRUE)
	fontLargeB 	<- setFont(size="extralarge", bold=TRUE)
	fontHuge 	<- setFont(size="huge", bold=TRUE)
	fontNormal 	<- setFont(size="normal")	
	
	tkgrid(tklabel(MainFrame, text=""))	
	
	header1	<- tklabel(MainFrame, text="Data collection for a basic statistic course", font=fontHuge)
	header2	<- tklabel(MainFrame, text="For the first examples we would like to use user-generated data.\n Please answer the following questions (anonymously)!", font=fontLarge)			
	header3	<- tklabel(MainFrame, text="1) Questions about yourself...", font=fontLarge)			
	header4	<- tklabel(MainFrame, text="2) Even more questions about yourself...", font=fontLarge)		
	header5	<- tklabel(MainFrame, text="3) and still not finished...", font=fontLarge)	
	
	text1 	<- tklabel(MainFrame, text="Name:", font=fontLargeB)
	
	text2 	<- tklabel(MainFrame, text="Sex:", font=fontLargeB)
	t21		<- tklabel(MainFrame, text="Female", font=fontNormal)
	t22 	<- tklabel(MainFrame, text="Male", font=fontNormal)
	
	text3 	<- tklabel(MainFrame, text="Height:", font=fontLargeB)
	t3 		<- tklabel(MainFrame, text="cm", font=fontNormal)
	
	text4 	<- tklabel(MainFrame, text="Shoe Size:", font=fontLargeB)
	t4 		<- tklabel(MainFrame, text="Number", font=fontNormal)
	
	text5 	<- tklabel(MainFrame, text="Amount of time:", font=fontLargeB)			
	t51 	<- tklabel(MainFrame, text="Hours", font=fontNormal)
	t52 	<- tklabel(MainFrame, text="Minutes", font=fontNormal)
	
	text6 	<- tklabel(MainFrame, text="Random experiment:", font=fontLargeB)
	t6 		<- tklabel(MainFrame, text="   number", font=fontNormal)
	t6val 	<- tklabel(MainFrame,text=as.character(tclvalue(randExp)), font=fontNormal)
	tkconfigure(t6val, textvariable=randExp)
	
	# Explainations
	q1 <- tklabel(MainFrame, text="Please enter an alias (maximal length 9):", font=fontNormal)
	q2 <- tklabel(MainFrame, text="Please tell us your sex:", font=fontNormal)
	q3 <- tklabel(MainFrame, text="What is your body height?", font=fontNormal)
	q4 <- tklabel(MainFrame, text="What is your shoe size?", font=fontNormal)
	q5 <- tklabel(MainFrame, text="How long was your travel from home to here?", font=fontNormal)
	q6 <- tklabel(MainFrame, text="Please flip a coin five times\n and count the number of the realisation 'head':", font=fontNormal)
	
	# Input-fields
	a1 	<- tkentry(MainFrame, width="10", textvariable=anonym, font=fontNormal)
	a21 <- tkradiobutton(MainFrame)
	a22 <- tkradiobutton(MainFrame)
	tkconfigure(a21, variable=sex, value="female")
	tkconfigure(a22, variable=sex, value="male")	
	a3 	<- tkentry(MainFrame, width="10", textvariable=height, font=fontNormal)
	a4 	<- tkentry(MainFrame, width="10", textvariable=shoeSize, font=fontNormal)
	
	a51 <- tkentry(MainFrame, width="3", textvariable=timeHH, font=fontNormal)
	a52 <- tkentry(MainFrame, width="3", textvariable=timeMM, font=fontNormal)
	
 	a6 	<- tkscale(MainFrame, from=0, to=5, showvalue=FALSE, variable=randExp, resolution=1, orient="horizontal")
	
	tkgrid(header1, columnspan=4)
	tkgrid(header2, columnspan=4)
	
	tkgrid(tklabel(MainFrame, text=""))
	
	# 1) Questions about course-participants
	tkgrid(header3, columnspan=4)
	tkgrid(tklabel(MainFrame, text=""))	
	
	tkgrid(text1, text2)
	tkgrid.configure(text1, column=0, columnspan=2)
	tkgrid.configure(text2, column=2, columnspan=2)
	
	tkgrid(q1, q2)
	tkgrid.configure(q1, column=0, columnspan=2, rowspan=2)
	tkgrid.configure(q2, column=2, columnspan=2, rowspan=2)
	
	tkgrid(tklabel(MainFrame, text=""))
	
	tkgrid(a1, a21, t21)
	tkgrid.configure(a1, columnspan=2, column=0)
	tkgrid.configure(a21, columnspan=1, column=2, sticky="e")	
	tkgrid.configure(t21, columnspan=1, column=3, sticky="w")
	
	tkgrid(a22, t22)
	tkgrid.configure(a22, columnspan=1, column=2, sticky="e")
	tkgrid.configure(t22, columnspan=1, column=3, sticky="w")
	
	tkgrid(tklabel(MainFrame, text="")); tkgrid(tklabel(MainFrame, text=""))
	
	# 2) Questions on course-leader
	tkgrid(header4, columnspan=4)
	tkgrid(tklabel(MainFrame, text=""))
	
	tkgrid(text3, text4)
	tkgrid.configure(text3, column=0, columnspan=2)
	tkgrid.configure(text4, column=2, columnspan=2)
	
	tkgrid(q3, q4)
	tkgrid.configure(q3, column=0, columnspan=2, rowspan=2)
	tkgrid.configure(q4, column=2, columnspan=2, rowspan=2)
	
	tkgrid(tklabel(MainFrame, text=""))
	
	tkgrid(a3,t3,a4,t4)
	tkgrid.configure(a3,  sticky="e")
	tkgrid.configure(t3,  sticky="w")
	tkgrid.configure(a4,  sticky="e")
	tkgrid.configure(t4,  sticky="w")	
	
	tkgrid(tklabel(MainFrame, text="")); tkgrid(tklabel(MainFrame, text=""))
	
	# 3) Other questions
	tkgrid(header5, columnspan=4)
	tkgrid(tklabel(MainFrame, text=""))
	
	tkgrid(text5, text6)
	tkgrid.configure(text5, column=0, columnspan=2)
	tkgrid.configure(text6, column=2, columnspan=2)
	
	tkgrid(q5, q6)
	tkgrid.configure(q5, column=0, columnspan=2, rowspan=2)
	tkgrid.configure(q6, column=2, columnspan=2, rowspan=2)
	
	tkgrid(tklabel(MainFrame, text=""));	
	
	tkgrid(a51, t51, a6)
	tkgrid.configure(a51, sticky="e")
	tkgrid.configure(t51, sticky="w")
	tkgrid.configure(a6, column=2, columnspan=2)
	
	labelsRandExp <- tklabel(MainFrame, text="# head: ", font=fontNormal)
	randExpValue <- tklabel(MainFrame,text=as.character(tclvalue(randExp)), font=fontNormal)
	tkconfigure(randExpValue, textvariable=randExp)
	
	tkgrid(a52, t52, labelsRandExp, randExpValue)
	tkgrid.configure(a52, sticky="e")
	tkgrid.configure(t52, sticky="w")
	tkgrid.configure(labelsRandExp, sticky="e")
	tkgrid.configure(randExpValue, sticky="w")
	
	tkgrid(tklabel(MainFrame, text="")); tkgrid(tklabel(MainFrame, text=""))
	
	OK.Button <- tkbutton(MainFrame, text="Send", command=OnOK, font=setFont(size="normal"))
	RESET.Button <- tkbutton(MainFrame, text="Cancel", command=OnCancel, font=setFont(size="normal"))
	
	tkgrid(OK.Button, RESET.Button)
	tkgrid.configure(OK.Button, columnspan=2)
	tkgrid.configure(RESET.Button, columnspan=2, column=2)
}
