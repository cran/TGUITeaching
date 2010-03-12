.First.lib <- function(lib, pkg) {
    ### Part of the .First.lib to be change by a package writer
   # if(!file.exists("adminExercise.txt")){
    #  fco <- file("adminExercise.txt","w")
     # close(fco)
    #}
	packageName <- "TGUITeaching" 			###Package name
    mainTitle 	<- "TGUI Teaching" 			### title for GUI
    mainSub 	<- "Teaching GUI system"	###subtitle for GUI
	subTitle 	<- "Interactive Feedback- and Training-Tool for Teaching & Learning" ##subsubtitle
	usertitle 	<- "Student Interface" #Name for the user interface
    admintitle 	<- "Trainer Interface" # Name for the admin interface
    developed1 	<- "TGUI-Basic-System developed by G. Dinges, M. Templ (2005)"#comments
    developed2 	<- "Redesigned by G. Dinges, A. Kowarik, B. Meindl, M. Templ (2009)"#comments 
    notice 		<- "Feedback is welcome! Visit www.statistik.at"	#comments
    
    #### End custom part
    version <- citation(packageName)$note
    version <- substr(version, (nchar(version)-5), nchar(version))

  	pathGUI <- getwd()
    aTassign("pathGUI",pathGUI)
    if(Sys.info()[1]=="Windows") {
      pathEtc <- paste(searchpaths()[grep(packageName, searchpaths())], "\\etc", sep="")
	  pathEtcCore <- paste(searchpaths()[grep("TGUICore", searchpaths())], "\\etc", sep="")
	  pathData <- paste(searchpaths()[grep(packageName, searchpaths())], "\\data\\", sep="")
      pathDoc <- paste(searchpaths()[grep(packageName, searchpaths())], "\\doc\\", sep="")
      pathContents <- paste(pathEtc, "\\contents.csv", sep="")
    }			
    else {
      pathEtc <- paste(searchpaths()[grep(packageName, searchpaths())], "/etc", sep="")
	  pathEtcCore <- paste(searchpaths()[grep("TGUICore", searchpaths())], "/etc", sep="")
	  pathData <- paste(searchpaths()[grep(packageName, searchpaths())], "/data/", sep="")
	  pathDoc <- paste(searchpaths()[grep(packageName, searchpaths())], "/doc/", sep="")
      pathContents <- paste(pathEtc, "/contents.csv", sep="")
    }

    contents <- read.table(pathContents,header=FALSE,sep=";",stringsAsFactors=FALSE)
    colnames(contents) <- c("Kurs","Teil","Funktion","Auswertung","Titel","Antwort")
    aTassign("contents",contents)
    aTassign("pathEtc", pathEtc);			aTassign("pathDoc", pathDoc)
	aTassign("pathEtcCore", pathEtcCore);
	aTassign("pathData", pathData);			
    aTassign("mainTitle",mainTitle);		aTassign("subTitle", subTitle)
	aTassign("mainSub", mainSub);
    aTassign("usertitle", usertitle); 		aTassign("admintitle", admintitle)	
    aTassign("developed1", developed1); 	aTassign("developed2", developed2)
    aTassign("notice", notice)
    aTassign("alwaysOn","Data Collection")
	
	# activate all examples if adminExercise is not found
	# current contents of the adminExercises File
	aa <- as.character(read.table(paste(aTget("pathGUI"),"/adminExercise.txt",sep=""))[,1])		
	
	# exercises available from TGUITeaching
	cc <- unlist(strsplit(aTget("contents")[,3],"\\("))
	cc <- cc[seq(1,length(cc),2)]
	# if no overlapping exercises are found, adminExercises is from TGUICore
	# and needs to be replaced with the examples from TGUITeaching
	if(length(which(aa %in% cc))==0) {
		dat <- as.character(read.table(file=paste(aTget("pathEtc"), "/contents.csv", sep=""), sep=";")[,3])
		dat <- as.character(sapply(dat, function(x) { substr(x, 1, nchar(x)-2) } ))
		write.table(dat, file=paste(aTget("pathGUI"),"/adminExercise.txt",sep=""), row.names=FALSE, col.names=FALSE, quote=FALSE)			
	}		
  	cat("The window of the R console should be minimized while using the GUI.\n")
  	runGUI()
}