library(seqinr)
haps<-read.fasta(file="xxxx/24june2018/H_sequence_file.fasta")
otu.reps<-read.fasta(file="xxxx/24june2018/working_dir/individual_97.5_clusters/repseq_alignments_by_clade/repseqs_97.5_w_outs_no_singles_profilealigned_ginsi.fasta")
drops<-names(otu.reps)
missing.drops<-drops[!drops %in% names(haps)]
missing.drops
#illustrates that outgroups AND some OTU_97.5 reps not identified as OTU_Haplotype reps...likely that order varied or something

missing.haps<-names(haps)[!names(haps) %in% names(otu.reps)]
haps.to.add<-haps[names(haps) %in% missing.haps]
write.fasta(sequences=haps.to.add,names=names(haps.to.add),file="xxxx/24june2018/working_dir/H_sequence_file_HAPLOS_TO_ADD_TO975_ALIGN.fasta")


df<-read.csv(file="xxxx/24june2018/working_dir/MASTER_accession_list_haps_added.csv",stringsAsFactors=FALSE)

#this identifies the OTU97.5 repseq that is not an OTU100 repseq
alt.rep<-df$OTU_97.5_Raw_24_June_2018[df$Organism_Acc_No %in% missing.drops[7]]

#this gives the organism_acc_no's in this OTU97.5
df$Organism_Acc_No[df$OTU_97.5_Raw_24_June_2018 %in% alt.rep]

#this gives the unique OTU_Haplotype's in this OTU_97.5
unique(df$OTU_Haplotype_Raw_24_June_2018[df$OTU_97.5_Raw_24_June_2018 %in% alt.rep])


#this identifies the OTU_Haplotype that this accession belongs to
alt.hap<-df$OTU_Haplotype_Raw_24_June_2018[df$Organism_Acc_No %in% missing.drops[7]]


#this identifies the organism_acc_no that is the OTU_Haplotype type
df$Organism_Acc_No[df$OTU_Haplotype_Raw_24_June_2018 %in% alt.hap & df$RepSeq_100_Raw_24_June_2018 %in% "Yes"]

df$Organism_Acc_No[df$OTU_Haplotype_Raw_24_June_2018 %in% alt.hap]

#ah...ok...blastclust was for over 85% of sequence, so a 100, is not a haplotype, but 100 over 85% of sequence.
#No...I think it is sequence overlap...so 100 means there has to be perfect overlap, and if not, then it is a separate OTU.  and 85 means sequence must have 85% overlap.  Gaps not accounted for either.




haps<-read.fasta(file="xxxx/24june2018/working_dir/975_ALIGN_w_outs_and_haplos_added.fasta.subregionsrealignedmafftginsi.fasta")
df<-read.csv(file="xxxx/24june2018/working_dir/MASTER_accession_list_haps_added.csv",stringsAsFactors=FALSE)
keeps<-names(haps)[grep("Vulcanochloris_",names(haps))]
keeps<-c(keeps,names(haps)[grep("Asterochloris_",names(haps))])
keeps<-c(keeps,df$Organism_Acc_No[df$RepSeq_Haplotype_Raw_24_June_2018 %in% "Yes"])
drops<-names(haps)[!names(haps) %in% keeps]
new.haps<-haps[names(haps) %in% keeps]
write.fasta(sequences=new.haps,names=names(new.haps),file="xxxx/24june2018/working_dir/975_ALIGN_w_outs_and_haplos_added.fasta.subregionsrealignedmafftginsi_hapsoutsonly.fasta")
