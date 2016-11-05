gethashtag <- function(x){
	
	hashback <- gsub(" .*","",gsub(".*#","",x))
	
	return(hashback)
	
}


#tester <- c("dsaf;jkldsaf #hi adflkj","asdfljk;dfsjl; #hi adflkadfs"," adsf;kldsaf #Transform",

#		"adlkjadfs dafsladfsjf #analyze", "adslkjdas #analyze dfasfd", "adsf;jklafds #read aljkdfa",
		
#		"jladsfldfj;as adsffdsa #report")
		
		
#tester <- sample(tester,50,replace=TRUE)	


#avs <- abbreviate(toupper(gethashtag(tester)),minlength=1)

#guidedf <- unique(data.frame(Symbol=avs,Tag=toupper(gethashtag(tester))))

#ibrary(ggplot2)

#ggplot(cars,aes(x=speed,y=dist))+geom_point()+annotate("text",x=cars[,1],y=cars[,2],label=avs,family="serif")





