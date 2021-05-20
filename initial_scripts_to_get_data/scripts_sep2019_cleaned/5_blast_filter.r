date<-"16_Sep_2019"
bl<-read.table(file="working_dir_sep2019/blast_for_edit_clean.txt",sep="\t",header=FALSE,fill=TRUE,na.strings = "", quote="\"")
colnames(bl)<-c("Query","BlastHit")
bl$BlastHit<-as.character(bl$BlastHit)
bl$SubCladeHit<-NA
bl$CladeHit<-NA
#head(bl)
bl$SubCladeHit<-bl$BlastHit
bl$SubCladeHit<-gsub("Trebouxia_CladeA.*","Trebouxia_CladeA",bl$SubCladeHit)
bl$SubCladeHit<-gsub("Trebouxia_CladeI.*","Trebouxia_CladeI",bl$SubCladeHit)
bl$SubCladeHit<-gsub("Trebouxia_CladeG.*","Trebouxia_CladeG",bl$SubCladeHit)
bl$SubCladeHit<-gsub("Trebouxia_CladeS.*","Trebouxia_CladeS",bl$SubCladeHit)
bl$SubCladeHit<-gsub("Asterochloris.*","Asterochloris",bl$SubCladeHit)
bl$SubCladeHit<-gsub("Palma.*","Palma",bl$SubCladeHit)
bl$SubCladeHit<-gsub("Dictyochloropsis.*","Dictyochloropsis",bl$SubCladeHit)
bl$SubCladeHit<-gsub("Coccomyxa.*","Coccomyxa",bl$SubCladeHit)
bl$SubCladeHit<-gsub("Diplosphaera.*","Diplosphaera",bl$SubCladeHit)
bl$SubCladeHit<-gsub("Chloroideum.*","Chloroideum",bl$SubCladeHit)
bl$SubCladeHit<-gsub("Dilabifilum.*","Dilabifilum",bl$SubCladeHit)
bl$SubCladeHit<-gsub("Trentepohliales.*","Trentepohliales",bl$SubCladeHit)
bl$SubCladeHit<-gsub("Ulva.*","Ulva",bl$SubCladeHit)
bl$SubCladeHit<-gsub("Volvox.*","Volvox",bl$SubCladeHit)
bl$SubCladeHit<-gsub("Chlamydomonas.*","Chlamydomonas",bl$SubCladeHit)
bl$SubCladeHit<-gsub("Fungi.*","Fungi",bl$SubCladeHit)
bl$CladeHit<-bl$SubCladeHit
bl$CladeHit<-gsub("Trebouxia.*","Trebouxia",bl$CladeHit)
bl$CladeHit<-gsub("Asterochloris.*","Asterochloris",bl$CladeHit)
bl$CladeHit<-gsub("Palma.*","Palma",bl$CladeHit)
bl$CladeHit<-gsub("Dictyochloropsis.*","Dictyochloropsis",bl$CladeHit)
bl$CladeHit<-gsub("Coccomyxa.*","Coccomyxa",bl$CladeHit)
bl$CladeHit<-gsub("Diplosphaera.*","Diplosphaera",bl$CladeHit)
bl$CladeHit<-gsub("Chloroideum.*","Chloroideum",bl$CladeHit)
bl$CladeHit<-gsub("Dilabifilum.*","Dilabifilum",bl$CladeHit)
bl$CladeHit<-gsub("Trentepohliales.*","Trentepohliales",bl$CladeHit)
bl$CladeHit<-gsub("Ulva.*","Ulva",bl$CladeHit)
bl$CladeHit<-gsub("Volvox.*","Volvox",bl$CladeHit)
bl$CladeHit<-gsub("Chlamydomonas.*","Chlamydomonas",bl$CladeHit)
bl$CladeHit<-gsub("Fungi.*","Fungi",bl$CladeHit)
#head(bl)
master<-read.csv(file="working_dir_sep2019/MASTER_accession_list.csv",header=TRUE,stringsAsFactors=FALSE)
master$Organism_Acc_No<-as.character(master$Organism_Acc_No)
bl$Query<-as.character(bl$Query)
for(z in 1:nrow(bl)){
	master$LocalBlast[master$Organism_Acc_No==bl$Query[z]]<-bl$BlastHit[z]
	master$SubCladeBlast[master$Organism_Acc_No==bl$Query[z]]<-bl$SubCladeHit[z]
	master$CladeBlast[master$Organism_Acc_No==bl$Query[z]]<-bl$CladeHit[z]
}
#length(master$Organism_Acc_No[which(master$LocalBlast=='Trebouxia')])
#unique(master$LocalBlast)
master$LocalBlast<-as.character(master$LocalBlast)
master$Blast_Date<-NA
for(q in 1:nrow(master)){
	if(!is.na(master$LocalBlast[q])){
		master$Blast_Date[q]<-date
	}	
}
target.list<-master[which(master$ITSx_PASS=='PASS' & master$Quality_PASS=='PASS' & master$CladeBlast=='Trebouxia' & (is.na(master$Manual_Fail) | master$Manual_Fail=='No')),]
target.list.clade.a<-master[which(master$ITSx_PASS=='PASS' & master$Quality_PASS=='PASS' & master$SubCladeBlast=='Trebouxia_CladeA' & (is.na(master$Manual_Fail) | master$Manual_Fail=='No')),]
target.list.clade.i<-master[which(master$ITSx_PASS=='PASS' & master$Quality_PASS=='PASS' & master$SubCladeBlast=='Trebouxia_CladeI' & (is.na(master$Manual_Fail) | master$Manual_Fail=='No')),]
target.list.clade.g<-master[which(master$ITSx_PASS=='PASS' & master$Quality_PASS=='PASS' & master$SubCladeBlast=='Trebouxia_CladeG' & (is.na(master$Manual_Fail) | master$Manual_Fail=='No')),]
target.list.clade.s<-master[which(master$ITSx_PASS=='PASS' & master$Quality_PASS=='PASS' & master$SubCladeBlast=='Trebouxia_CladeS' & (is.na(master$Manual_Fail) | master$Manual_Fail=='No')),]
#nrow(target.list)
write(target.list$Organism_Acc_No,file="working_dir_sep2019/target.taxa.combined.txt")
write(target.list.clade.a$Organism_Acc_No,file="working_dir_sep2019/target.taxa.A.combined.txt")
write(target.list.clade.i$Organism_Acc_No,file="working_dir_sep2019/target.taxa.I.combined.txt")
write(target.list.clade.g$Organism_Acc_No,file="working_dir_sep2019/target.taxa.G.combined.txt")
write(target.list.clade.s$Organism_Acc_No,file="working_dir_sep2019/target.taxa.S.combined.txt")
write.csv(master,file="working_dir_sep2019/MASTER_accession_list.csv",row.names=FALSE)