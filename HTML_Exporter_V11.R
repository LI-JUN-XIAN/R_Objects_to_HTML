
#手刻簡易HTML
#字體統一

FA <- "Calibri"
library(magrittr)

####################################
####################################
####################################

##################Elementary Functions##################

HTML_Text <- function(TX,size=16,color="black",bius="0",link="0",warp=F){ 

	bius <- unlist(strsplit(toupper(bius),split=""))
	bius <- unique(bius[bius %in% c("B","I","U","S")])

	if (length(bius)>0){

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
#
#依條件打顏色請在此框架外以"HTML_Text_Composition"函數完成後取代該行在塞入此函數產生表格
#

HTML_DFtoTable <- function(DF){

 DF[] <- lapply(DF,as.character)
 
 ST1 <- paste0('<TABLE BORDER=1 style="font-family:',FA,'">')
 ST1 <- c(ST1,'<tr style="background-color:#787878;color:white">')

 #
 # Haeder <- sapply(colnames(DF),function(x){ paste0('<td>',x,"</td>") })
 # ST1 <- c(ST1,Header)
 #
	
 for (x in colnames(DF)){
 
	ST1 <- c(ST1,'<td>',x,'</td>')
 }
 
 ST1 <- c(ST1,'</tr>')
	
for (y in (1:length(DF[[1]]))){

	 ST1 <- c(ST1,'<tr>')

	# ST1 <- c(ST1,sapply(iris[y,],function(x){ paste0('<td>',x,"</td>") }))
	
	 for (x in DF[y,]){

		ST1 <- c(ST1,'<td>',x,'</td>')

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

HTML_TableLayout <- function(objList,cols=2){

	if (length(objList)<=1){
		
		return(objList)
		
	}else{
		
		ST <- '<table>'
		write.csv(objList,file='d:/error.csv')
		for (x in 1:length(objList)){
		
			if (x %% cols==1){
			
				ST <- c(ST,'<tr><td>',objList[[x]],'</td>')
			
			}else if (x %% cols==0 | x == length(objList)){
			
				ST <- c(ST,'<td>',objList[[x]],'</td></tr>')
			
			}else{
			
				ST <- c(ST,'<td>',objList[[x]],'</td>')
			
			}
				
		}
		
		ST <- c(ST,'</table>')
		return(ST)
	}
}

#######################Combine Function####################

HTML_Finish <- function(HTMLName="0",objList,HTMLPath="0"){

	if(HTMLPath != "0" & HTMLName != "0"){
		if(class(objList)=="list"){ objList <- unlist(objList) }
		HTML_Final <- c('<HTML>','<head><title>',HTMLName,'</title></head>','<body>',objList,'</body></HTML>')
		write.table(HTML_Final,file=paste0(HTMLPath,'/',HTMLName,'.html'),quote = F,row.names = F,col.names = F)
		
		}
	}


###########################
#Texts = Texts. .................................................character vector
#Styles = Stylesettings mapping to elements of Texts.............list

HTML_Text_Composition <- function(Texts,Styles){

	if (class(Styles)=="character"){

		Styles <- strsplit(Styles,',')
		Styles <- lapply(Styles,function(x){
				Style <- lapply(strsplit(x,'='),'[',2)
				names(Style) <- sapply(strsplit(x,'='),'[',1)
				return(Style)
			})
		}

	defaultStyle <- list(size=16,color="black",bius="0",link="0",warp=F)

	finalComposition <- sapply(c(1:length(Texts)),function(x){#### 

		if(x <= length(Styles)){###

			Style <- sapply(names(defaultStyle),function(y){##

				if (is.null(Styles[[x]][[y]])){#
					return(defaultStyle[[y]])
					}else{#
					return(Styles[[x]][[y]])
					}#
				}##
				)


			return(HTML_Text(TX=Texts[x],size=as.numeric(Style[[1]]),color=Style[[2]],bius=Style[[3]],link=Style[[4]],warp=as.logical(Style[[5]])))


			}else{###

			return(HTML_Text(TX=Texts[x]))

			}###
		}####
		)

	return(finalComposition)

}
