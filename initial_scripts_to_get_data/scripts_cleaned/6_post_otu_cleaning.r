date<-"24_June_2018"
master<-read.csv(file="working_dir/MASTER_accession_list.csv")
master$Organism_Acc_No<-as.character(master$Organism_Acc_No)

otufiles<-list.files("working_dir/", pattern=c("blastclust_*","*_otu_assignment.txt"), full.names=TRUE)

for(i in 1:length(otufiles)){
	bl<-read.table(file=otufiles[i],sep="\t",header=FALSE,fill=TRUE,na.strings = "", quote="\"")
	thresh=regmatches(otufiles[i],regexpr("blastclust_.*_otu_assignment",otufiles[i]))
	thresh=gsub("[A-z]","",thresh)
	colnames(bl)<-c("OTUraw","Taxon")
	newotu<-paste("OTU",thresh,"Raw",date,sep="_")
		for(z in 1:nrow(bl)){
			master[[newotu]][master$Organism_Acc_No==bl$Taxon[z]]<-as.character(bl$OTUraw[z])
		}	
}

write.csv(master,file="working_dir/MASTER_accession_list.csv",row.names=FALSE)

#ff<-as.data.frame(table(master$OTU_100_Raw))
#notsinglehap<-ff[which(ff$Freq>1),]
#masternotsinglehap<-master[master$OTU_100_Raw %in% notsinglehap$Var1,]
#length(unique(na.omit(masternotsinglehap$OTU_95_Raw)))
#length(unique(na.omit(masternotsinglehap$OTU_97.5_Raw)))
#length(unique(na.omit(master$OTU_97.5_Raw)))
#vv<-as.data.frame(table(master$OTU_97.5_Raw))
#notsinglehap<-vv[which(vv$Freq>1),]
#length(unique(na.omit(notsinglehap$Var1)))


