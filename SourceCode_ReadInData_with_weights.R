# R-function to read in social contact data
# 
# Authors: Yannick Vandendijck and Giancarlo Camarda (updated by Oswaldo Gressani)
# File last updated on 01/03/2022
#_____________________________________________________________________

Load_Social_Contact_Data = function(){

	# Read in social contact data
	dir="Data/Polymod Version/"
	partdata=read.table(paste(dir,"participants_be_final.txt",sep=""),
	                    header=T,sep="\t")
	contdata=read.table(paste(dir,"contacts_be_final.txt",sep=""),
	                    header=T,sep="\t")
	
	dim(contdata)
	dim(partdata)
	
	#edit(partdata)
	#sum(partdata$diary_weight)

	# A closer look at the participants data
	#' participant's maximum age is 84, not sure why this is 77
	#' needs to be adapted later?
	
	partdata = partdata[partdata$participant_age < 77,]
	#str(partdata)
	range(partdata$participant_age)
	mean(partdata$participant_age)
	# hist(partdata$participant_age,nclass=100)
	table(partdata$participant_age)
	table(partdata$participant_gender)

	# contact data
	contdata = contdata[contdata$global_id %in% partdata$global_id,]
	#' contact's maximum age is 98 adapt later...
	contdata = contdata[contdata$cnt_age_mean < 77,]
	contdata = contdata[!is.na(contdata$cnt_age_mean),]
	
	#str(contdata)


	# Read in demography: 
	# population size from 0 to 80 years of age (the last category does not 
	# include 81 and over)
	dir="Data/Demography/"
	pop=read.table(paste(dir,"age-specific pop Contact Countries 2005.txt",
	                     sep=""),header=T)
	P=pop[,"be"]
	# plot(0:76,P[1:77],xlab="Age (years)",ylab="Population size",cex.lab=1.6, 
	# cex.axis=1.6, pch=16,cex=1.25)

	# Exposure (mind the age-values) # ages 0-76 # number of participants per age
	tilde.e=hist(partdata$participant_age,breaks=seq(0,100,1)-0.5,
	             plot=F)$counts[1:77]
	
	#' before ---> ... * 745
	#' now ---> ... * nrow(partdata)
	partdata$diary_weight2 = partdata$diary_weight/sum(partdata$diary_weight)*nrow(partdata)
	tilde.e.2 = tapply(partdata$diary_weight2, partdata$participant_age, sum)
	sum(tilde.e.2)
	cbind(tilde.e, tilde.e.2)


	# Total number of contacts by age of participants and contacts
	mat.id=NULL
	un.id=sort(unique(partdata$local_id))
	for (i in 1:length(un.id)){
	  sel.id=contdata$local_id==un.id[i]
	  vec.id=rep(0,100)
	  if (sum(sel.id)>0){
	    vec.id=hist(contdata$cnt_age_mean[sel.id],breaks=seq(0,100,1)-0.5,
	                plot=F)$counts
	  }
	  mat.id=rbind(mat.id,vec.id)
	}
	# Debug - 20 counts less because of missing age of contact
	sum(mat.id)
	dim(mat.id)

	mat.id2 = mat.id
	for (i in 1:length(un.id)){
	  mat.id2[i,] = mat.id[i,]*partdata$diary_weight2[i]
	}
	#edit(mat.id2)
	sum(mat.id2)
	dim(mat.id2)

	# Per participant's age
	mat.cont=NULL
	weight=NULL
	for (i in 0:100){
	  sel.tmp=partdata$participant_age[order(partdata$local_id)]==i
	  vec.cont=rep(0,100)
	  if(sum(sel.tmp)==1){
	    vec.cont=mat.id2[sel.tmp,]
	  }
	  if(sum(sel.tmp)>1){
	    vec.cont=apply(mat.id2[sel.tmp,],2,sum)
	  }
	  mat.cont=rbind(mat.cont,vec.cont)
	}
	# Debug
	sum(mat.cont)
	dim(mat.cont)


return(list(mat_cont=mat.cont, tilde_e=tilde.e.2, P=P))
}











