`ex_eda` <- function(eval=FALSE) {
	plotscatter2 <- MainFrame <- scatter2 <- Exit <- NULL
	useExampleData <- function() {
		V1 <- c("Obelix", "Karli", "Susi", "RAPID", "Lisa", "Froq", "Gilbert",
				"Bidl", "Anid", "nobody1", "Kurti", "Karfiol", "Me", "Moritz",
				"Quasi", "Linus", "JoJo")
		V2 <- c("male","male","female","male","female", "female","male","female",
				"male","male", "male","female","male","male", "male","male","female")
		V3 <- c(187, 180, 170, 183, 168, 170, 164, 174, 183, 175, 172, 160, 178, 180, 173, 187, 167)
		V4 <- c(53, 43, 39, 44, 38, 37, 41, 38, 41, 45, 42, 36, 41, 46, 43, 45, 36)
		V5 <- c(15,25,150,25,50,30,20,80,180,50,110,35,140,30,60, 100,45)
		V6 <- c(4, 3, 4, 2, 2, 0 ,2, 5, 3, 2, 1, 2, 3, 3, 1, 2, 3)
		V7 <- rep("Trainings Course", length(V1))
		out <- data.frame(V1,V2,V3,V4,V5,V6,V7)		
		out
	}
	plotScatterplotMatrix <- function() {
		par(bg="white")
		exDat <- useExampleData()
		if(!file.exists("data.txt"))
			KursDat <- exDat
		else {
			origDat <- read.table("data.txt")	
			rr <- nrow(origDat)
			if(rr < 16) {
				ind <- sample(2:17, rr)
				KursDat <- exDat[-ind,]
				KursDat <- rbind(KursDat, origDat)
			}				
		}
				
		colnames(KursDat) <- c("Alias", "Sex", "Height","shoeSize", "TravelTime", "randExp","Session")
		KursDat <- data.frame(Height=as.numeric(KursDat$Height), randExp=as.numeric(KursDat$randExp), shoeSize=as.numeric(KursDat$shoeSize),Time=as.numeric(KursDat$TravelTime))
		
		diagLabels <- c("histogram", "density", "boxplot", "qqplot")			
		
		ifelse(Texists("aa"), aa <- as.numeric(evalq(tclvalue(aa), env = as.environment(which(search()=="TGUIenv")))), aa <- 0)
		ifelse(Texists("bb"), bb <- as.numeric(evalq(tclvalue(bb), env = as.environment(which(search()=="TGUIenv")))), bb <- 1)
		ifelse(Texists("cc"), cc <- as.numeric(evalq(tclvalue(cc), env = as.environment(which(search()=="TGUIenv")))), cc <- 1)
		ifelse(Texists("dd"), dd <- as.numeric(evalq(tclvalue(dd), env = as.environment(which(search()=="TGUIenv")))), dd <- 0)
		
		ifelse(Texists("diagonal"),	diagonal <- evalq(tclvalue(diagonal), env = as.environment(which(search()=="TGUIenv"))), diagonal <- 1)		
		diagLabels <- c("histogram", "density", "boxplot", "qqplot")	
		ergDiag <- diagLabels[as.integer(diagonal)]			
		
		ifelse(Texists("smoothing"), smoothing <- evalq(tclvalue(smoothing), env = as.environment(which(search()=="TGUIenv"))), smoothing <- FALSE)	
		
		colsVals <- c(aa,bb,cc,dd)
		existCols <- which(colsVals==1)
		
		m <- KursDat[, c(which(colsVals == 1))]
		
		ifelse(smoothing==1, rob <- TRUE, rob <- FALSE)
		
		if(length(existCols)==1)
			hist(m, main=paste("Histogramm: ", colnames(KursDat)[existCols], sep=""), col=2)
		else {
			scatterplot.matrix(m, diagonal=ergDiag, smooth=rob, reg.line=FALSE, ellipse=FALSE, robust=TRUE, cex=1) 
		}
	}	
	plotScatterplotMatrixEval <- function() {
		exDat <- useExampleData()
		if(!file.exists("data.txt"))
			KursDat <- exDat
		else {
			origDat <- read.table("data.txt")	
			rr <- nrow(origDat)
			if(rr < 16) {
				ind <- sample(2:17, rr)
				KursDat <- exDat[-ind,]
				KursDat <- rbind(KursDat, origDat)
			}				
		}
		colnames(KursDat) <- c("Alias", "Sex", "Height","shoeSize", "TravelTime", "randExp","Session")
		KursDat <- data.frame(Height=as.numeric(KursDat$Height), randExp=as.numeric(KursDat$randExp), shoeSize=as.numeric(KursDat$shoeSize),Time=as.numeric(KursDat$TravelTime))
	
		scatterplot.matrix(KursDat, diagonal="histogram", reg.line=lm, robust=TRUE, cex=1) 
	}	
	
	exerciseTitle <- "Example: EDA (interactiv analysis of course data)"
	q1 <- "Please take a look at the data created by the participants of the course!"
	q2 <- "Mark the correct answers:"
	labs <- c("'height' contains at least one outlier",
			  "The mean of 'time' is larger than the median",
			  "The 2-dim. plot of 'shoe size' vs. 'height' shows a correlation",
			  "'randExp' is approximately normally distributed"
         )
	notice <- "Multiple answers are possible!"
	ex <- "ex_eda"	
	
	if(eval==FALSE) {
		OpenWindow(title=exerciseTitle)
		InteractivePlot(
				plot.function=plotScatterplotMatrix, 
				name=plotscatter2,
				header="EXPLORATIVE DATA ANALYSIS \nSource: Data by course",
				checkbox_header=c("Select variables for the scatterplot:", "Smoothing?"),
				checkbox=list(c("aa","bb","cc","dd"), "smoothing"),
				checkbox_start=list(c(0,1,1,0),0),
				checkbox_label=list(c("Height", "randExp", "shoeSize","Time"),	c("yes")),
				checkbox_twoCols=TRUE,
				radio_header=c(	"Kind of plot for the diagonal?"),		
				radio=c("diagonal"),
				radio_label=c("Histogram", "est. Density", "Boxplot", "QQ-Plot"),
				radio_value=1:4, 				
				click.function=NULL,
				design="leftright", hscale=1.4, vscale=1.4
		)			
		MultipleChoice(frame=MainFrame, 
				scatter2,
				question1=q1,
				question2=q2,
				labels=labs,
				note=notice,
				filename=ex)	
		tkgrid(plotscatter2)
		tkgrid(scatter2)
		ExitButton(name=Exit)
		tkgrid(Exit)			
	}
	else {
		exerciseTitle <- paste("Evaluation - ", exerciseTitle, sep="")
		OpenWindow(title=exerciseTitle)
		
		InteractivePlot(
				plot.function=plotScatterplotMatrixEval, 
				name=plotscatter2,
				header="EXPLORATIVE DATA ANALYSIS \nSource: Data by course",
				checkbox_header=c("Select variables for the scatterplot:", "Smoothing?"),
				checkbox=list(c("aa","bb","cc","dd"), "smoothing"),
				checkbox_start=list(c(0,1,1,0),0),
				checkbox_label=list(c("Height", "randExp", "shoeSize","Time"),	c("yes")),
				checkbox_twoCols=TRUE,
				radio_header=c("Kind of plot for the diagonal?"),		
				radio=c("diagonal"),
				radio_label=c("Histogram", "est. Density", "Boxplot", "QQ-Plot"),
				radio_value=1:4, 				
				click.function=NULL,
				design="leftright", hscale=1.2, vscale=1.2
		)			
		
		MultipleChoice(frame=MainFrame, 
				scatter2,
				question1=q1,
				question2=q2,
				labels=labs,Answer=TRUE,
				filename=ex)	
		tkgrid(plotscatter2)
		tkgrid(scatter2)			
	}
}	