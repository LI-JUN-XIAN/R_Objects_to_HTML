
#手刻簡易HTML
#字體統一

FA <- "微軟正黑體"

####################################
####################################
####################################

HTML_Finish <- function(HTMLName="0",ObjList,HTMLPath="0"){

	if(HTMLPath != "0" & HTMLName != "0"){
	
		HTML_Final <- c('<HTML>','<head><title>',HTMLName,'</title></head>','<body>',ObjList,'</body></HTML>')
		write.table(HTML_Final,file=paste0(HTMLPath,'/',HTMLName,'.html'),quote = F,row.names = F,col.names = F)
		
		}
	}

####################################

HTML_Text <- function(TX,size=16,color="black",bius="0",link="0",warp=F){ 

	if (sum(bius %in% c("b","i","u","s","B","I","U","S")) == length(bius)){

		for (x in bius){ TX <- paste0('<',x,'>',TX,'</',x,'>') }

	}

	if (link != "0" & length(link)==1){

		TX <- paste0('<a href="',link,'">',TX,'</a>')

	}

	FT <- paste0('<FONT FACE="',FA,'"><span style="font-size:',size,'pt;color:',color,'">',TX,'</span></FONT>')

	if(warp){

		FT <- paste0(FT,'<br>')
	}

	return(FT)

	}

###################################

HTML_Table <- function(DF){

#
#以規格/條件算出打色矩陣的單元，待製作
#

 DF[] <- lapply(DF,as.character)
 
 ST1 <- paste0('<TABLE BORDER=1 style="font-family:',FA,'">')
#
#ST1 <- c(ST1,'<style type="text/css">')
#
#.OOC{background-color:red;color:white}等打色規則與ccs指令
#
#ST1 <- c(ST1,'</style>')
#
 ST1 <- c(ST1,'<tr style="background-color:#787878;color:white">')
 for (x in colnames(DF)){
	ST1 <- c(ST1,'<td>')
	ST1 <- c(ST1,x)
	ST1 <- c(ST1,'</td>')
 }
 ST1 <- c(ST1,'</tr>')

for (y in (1:length(DF[[1]]))){
	 ST1 <- c(ST1,'<tr>')
	 for (x in DF[y,]){
		ST1 <- c(ST1,'<td>')
		ST1 <- c(ST1,x)
		ST1 <- c(ST1,'</td>')
 	 }
 	ST1 <- c(ST1,'</tr>')
}

 ST1 <- c(ST1,'</TABLE>')
 return(ST1)

}

####################################

HTML_Plot <- function(PlotRecod=NA,PlotName=NA,x=480,y=320,HTMLPath=NA,warp=F){

	if ((class(PlotRecod)[1] %in% c("recordedplot","gg")) & !(is.na(PlotRecod)|is.na(PlotName)|is.na(HTMLPath)) & file.exists(HTMLPath)){

		ImgDir <- paste0(HTMLPath,'/img')
		if(!file.exists(ImgDir)){ dir.create(ImgDir) }

		ImgFile <- paste0(ImgDir,'/',PlotName,'.jpg')

		jpeg(file=ImgFile, width = x, height = y, bg='white', quality = 90)
		print(PlotRecod)
		dev.off()

		HTML_Code <- paste0('<img src="',ImgFile,'" alt="',PlotName,'">')

		if (warp){paste0(HTML_Code,'<br')}

		return(HTML_Code)

	}

}

####################################
####################################
