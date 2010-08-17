`ex_clt` <- function(eval=FALSE) {
	nrSamples <- nrDraws <- nrClasses <- userColor <- NewFrame <- MainFrame <- qCLT <- NULL
	clt.examp <- function () {
		if(Texists("nrSamples")) 
			n <- as.numeric(evalq(tclvalue(nrSamples), env = as.environment(which(search()=="TGUIenv"))))
		else 
			n <- 1
		if(Texists("nrDraws")) 
			reps <- as.numeric(evalq(tclvalue(nrDraws), env = as.environment(which(search()=="TGUIenv"))))
		else 
			reps <- 3000		
		if(Texists("nrClasses")) 
			nclass <- as.numeric(evalq(tclvalue(nrClasses), env = as.environment(which(search()=="TGUIenv"))))
		else 
			nclass <- 16		
		if(Texists("userColor")) {
			userCol <- as.numeric(evalq(tclvalue(userColor), env = as.environment(which(search()=="TGUIenv"))))
			if(userCol==1) color <- "cornflowerblue"
			if(userCol==2) color <- "red"
			if(userCol==3) color <- "yellow"
			if(userCol==4) color <- "white"
		}
		else 
			color <- "cornflowerblue"			
		
		par(mfrow=c(2,2),bg="white")

		# Daten generieren
		norm.mat <- matrix(rnorm(n * reps), ncol = n)
		exp.mat <- matrix(rexp(n * reps, 1/3), ncol = n)
		unif.mat <- matrix(runif(n * reps), ncol = n)
		beta.mat <- matrix(rbeta(n * reps, 0.35, 0.25), ncol = n)
		
		norm.mean <- rowMeans(norm.mat)
		exp.mean <- rowMeans(exp.mat)
		unif.mean <- rowMeans(unif.mat)
		beta.mean <- rowMeans(beta.mat)
		
		x <- seq(min(norm.mean), max(norm.mean), length = 50)
		normmax <- max(dnorm(x, mean(norm.mean), sd(norm.mean)))
		tmp.hist <- hist(norm.mean, plot = FALSE, prob = TRUE, nclass = nclass)
		normmax <- max(tmp.hist$density, normmax) * 1.05
		hist(norm.mean, main = "Normal", xlab = "x", col = color,
				prob = TRUE, ylim = c(0, normmax), nclass = nclass)
		lines(x, dnorm(x, mean(norm.mean), sd(norm.mean)))		
		
		x <- seq(min(exp.mean), max(exp.mean), length = 50)
		expmax <- max(dnorm(x, mean(exp.mean), sd(exp.mean)))
		tmp.hist <- hist(exp.mean, plot = FALSE, prob = TRUE, nclass = nclass)
		expmax <- max(tmp.hist$density, expmax) * 1.05
		hist(exp.mean, main = "Exponential", xlab = "x", col = color,
				prob = TRUE, ylim = c(0, expmax), nclass = nclass)
		lines(x, dnorm(x, mean(exp.mean), sd(exp.mean)))		
		
		x <- seq(min(unif.mean), max(unif.mean), length = 50)
		unimax <- max(dnorm(x, mean(unif.mean), sd(unif.mean)))
		tmp.hist <- hist(unif.mean, plot = FALSE, prob = TRUE, nclass = nclass)
		unimax <- max(tmp.hist$density, unimax) * 1.05
		hist(unif.mean, main = "Uniform", xlab = "x", col = color,
				prob = TRUE, ylim = c(0, unimax), nclass = nclass)
		lines(x, dnorm(x, mean(unif.mean), sd(unif.mean)))		
		
		x <- seq(min(beta.mean), max(beta.mean), length = 50)
		betamax <- max(dnorm(x, mean(beta.mean), sd(beta.mean)))
		tmp.hist <- hist(beta.mean, plot = FALSE, prob = TRUE, nclass = nclass)
		betamax <- max(tmp.hist$density, betamax)
		hist(beta.mean, main = "Beta", xlab = "x", col = color,
				prob = TRUE, ylim = c(0, betamax), nclass = nclass)
		lines(x, dnorm(x, mean(beta.mean), sd(beta.mean)))
		mtext(paste("sample size =", n), outer = TRUE, cex = 2)
	}	
	
	exerciseTitle <- "Example: Central Limit Theorem (interactive graphic)"
	q1 <- "Mark the correct claims:"
	labs=c(	"For large n, the sample mean is approx. normal, independent of the distribution of the population",
			"The approximation of the normal distribution depends on the number of classes in the histogram.",
			"The sample means of a beta distribution converge extremly slow towards a normal distribution.")
	ex <- "ex_clt"		
	if(eval==FALSE) {
		OpenWindow(title=exerciseTitle)
		InteractivePlot(plot.function=clt.examp, name=NewFrame,
				header="Visualization of the distribution of the sample means.",
				slider=c("nrSamples","nrDraws", "nrClasses"),
				slider_start=c(1,100,16),slider_res=c(1,100,1),
				slider_lim=list(c(1,70),c(100,10000),c(1,70)),			
				slider_label=c("# Sample size (n):","# Draws:", "# Classes in Histogram:"),
				radio_header="color:",
				radio="userColor",
				radio_label=c("blue", "red", "yellow", "transparent"),
				radio_value=1:4, 
				design="leftright",hscale=1.7,vscale=1.7
		)
		MultipleChoice(frame=MainFrame, 
				qCLT,
				question1=q1,
				labels=labs,
				plotFunction=NULL,
				filename=ex)
		tkgrid(NewFrame)
		tkgrid(qCLT)		
	}
	else {
		exerciseTitle <- paste("Evaluation - ", exerciseTitle, sep="")
		OpenWindow(title=exerciseTitle)
		InteractivePlot(plot.function=clt.examp, name=NewFrame,
				header="Visualization of the distribution of the sample means.",
				slider=c("nrSamples","nrDraws", "nrClasses"),
				slider_start=c(1,100,16),slider_res=c(1,100,1),
				slider_lim=list(c(1,70),c(100,10000),c(1,70)),			
				slider_label=c("# Sample size (n):","# Draws:", "# Classes in Histogram:"),
				radio_header="color:",
				radio="userColor",
				radio_label=c("blue", "red", "yellow", "transparent"),
				radio_value=1:4, 
				click.function=NULL,design="leftright",hscale=1.4,vscale=1.4
		)
		MultipleChoice(frame=MainFrame, 
				qCLT,
				question1=q1,
				labels=labs,
				Answer=TRUE,
				filename=ex)
		tkgrid(NewFrame)
		tkgrid(qCLT)	
	}	
}
