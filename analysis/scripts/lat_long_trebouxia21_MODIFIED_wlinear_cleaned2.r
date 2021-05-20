#Modified from Kati Heller's Master accession rerun.Rmd



#Work on Leavitt et al. refs

#Leavitt et al. 2013b
#Info from Leavitt et. al. 2013b - match column with Herbarium ID to that in other table and match site and substrate info to correct species to input into missing info sheet
#Combine SI1 and SI2 from Leavitt et al. 2013b
require(openxlsx)
leavitt2013_1<-readWorkbook(xlsxFile="xxxx/leavitt_et_al_2013b_SI1.xlsx")
leavitt2013_2<-readWorkbook(xlsxFile="xxxx/leavitt_et_al_2013b_SI2.xlsx")

#change Voucher # to match Herbarium Accesion No. in other data frame
require(plyr)
colnames(leavitt2013_2)[3]<-"Herbarium.Accession.No."

#need to also change accessions in SI2 from BRY-C Leavitt to BRY-Leavitt
leavitt2013_2$Herbarium.Accession.No.<-gsub("BRY-C Leavitt","BRY-Leavitt",leavitt2013_2$Herbarium.Accession.No.)

#join them
leavitt2013<-join(leavitt2013_1,leavitt2013_2,by="Herbarium.Accession.No.",type="full")

#keep only the BRY ones
leavitt2013<-leavitt2013[grep("BRY",leavitt2013$Herbarium.Accession.No.),]

#add columns for lat long
leavitt2013[,c("Lat","Long")]<-NA

#add lat long to them based on sites
for(x in 1:nrow(leavitt2013)){
	if(leavitt2013$Site[x]==1){
		leavitt2013$Lat[x]<-"38.145"
		leavitt2013$Long[x]<-"-111.536"
	}
	if(leavitt2013$Site[x]==2){
		leavitt2013$Lat[x]<-"38.121"
		leavitt2013$Long[x]<-"-111.508"
	}
	if(leavitt2013$Site[x]==3){
		leavitt2013$Lat[x]<-"38.132"
		leavitt2013$Long[x]<-"-111.470"
	}
}

write.csv(leavitt2013,file="xxxx/leavitt_et_al_2013_mpn18sep2019.csv",row.names=FALSE)







#Onut-Braennstroem downloaded KML or KMZ file and converted online to xlsx file
ob<-read.csv(file='xxxx/onut-braennstroem_et_al_2017_Thamnolia_chemotypes_and_morphotypes.xlsx.csv',stringsAsFactors=FALSE)
ob$LatDeg<-NA
ob$LongDeg<-NA

ob$GPS_coordinates<-gsub("\t",", ",ob$GPS_coordinates)
ob$GPS_coordinates<-gsub(",  ",", ",ob$GPS_coordinates)
ob$GPS_coordinates<-gsub(",   ",", ",ob$GPS_coordinates)

for(x in 1:nrow(ob)){
	ob$LatDeg[x]<-strsplit(ob$GPS_coordinates[x],", ")[[1]][1]
	ob$LongDeg[x]<-strsplit(ob$GPS_coordinates[x],", ")[[1]][2]
}

ob$LatDeg = gsub('°', ' ', ob$LatDeg)
ob$LongDeg = gsub('°', ' ', ob$LongDeg)

ob$Lat<-ob$LatDeg
ob$Long<-ob$LongDeg

#manually convert 61°04.70000', 065°31.21667' to 61.078333, 65.520278
ob$Lat[4:5]<-61.078333
ob$Long[4:5]<-65.520278


write.csv(ob,file="xxxx/onut-braennstroem_et_al_2017_Thamnolia_chemotypes_and_morphotypes_llconverted_needs_accs.csv",row.names=FALSE)

#Must have manually added in accessions then to make ...llconverted_accs_added




#Leavitt et al. 2015...compare new seqs to those of Leavitt et al. Rhizoplaca, Xanthoparmelia, Melanelia (sl), Oropogon, and Alors Parmelina
require(openxlsx)
leavitt_mod<-readWorkbook(xlsxFile="xxxx/Leavitt_et_al_2015_SI.xlsx")

# trim spaces at end of accessions in Leavitt et al 2015
trim.trailing<-function (x) sub('\\s+$','', x)
leavitt_mod$GenBank.ITS<-trim.trailing(leavitt_mod$GenBank.ITS)

leavitt_mod$Lat<-NA
leavitt_mod$Long<-NA
leavitt_mod$LL_Publication<-NA
colnames(leavitt_mod)[16]<-"DNA_ID"
leavitt_mod$additional_location_info<-NA





#add in Leavitt et al. 2011 Xanthoparmelia
leavitt_et_al_2011<-readWorkbook(xlsxFile='xxxx/MASTER_rerun_LLPB.xlsx',sheet=7)

#remove herbarium code from DNA_ID
leavitt_et_al_2011$ID<-gsub(" \\(F\\)","",leavitt_et_al_2011$ID)

#adjust a few
leavitt_et_al_2011$ID<-gsub("131\r\n","131",leavitt_et_al_2011$ID)
leavitt_et_al_2011$ID<-gsub("226f ","226f",leavitt_et_al_2011$ID)

#and remove leading zeroes https://stackoverflow.com/questions/23538576/removing-leading-zeros-from-alphanumeric-characters-in-r
leavitt_et_al_2011$ID<-gsub("(?<![0-9])0+", "", leavitt_et_al_2011$ID, perl = TRUE)

for(i in 1:nrow(leavitt_et_al_2011)) {
  if(any(leavitt_mod$Long[leavitt_mod$DNA_ID %in% leavitt_et_al_2011$ID[i]] %in% NA)) {
    leavitt_mod$LL_Publication[leavitt_mod$DNA_ID %in% leavitt_et_al_2011$ID[i] & leavitt_mod$host.GENUS %in% "Xanthoparmelia"]<- 'Leavitt et al. (2015), Leavitt et al. (2011)'
    leavitt_mod$additional_location_info[leavitt_mod$DNA_ID %in% leavitt_et_al_2011$ID[i] & leavitt_mod$host.GENUS %in% "Xanthoparmelia"]<-leavitt_et_al_2011$Location[i]
    leavitt_mod$Long[leavitt_mod$DNA_ID %in% leavitt_et_al_2011$ID[i] & leavitt_mod$host.GENUS %in% "Xanthoparmelia"]<-leavitt_et_al_2011$Lon[i]
    leavitt_mod$Lat[leavitt_mod$DNA_ID %in% leavitt_et_al_2011$ID[i] & leavitt_mod$host.GENUS %in% "Xanthoparmelia"]<-leavitt_et_al_2011$Lat[i]  
  }
}

#leavitt_mod[leavitt_mod$host.GENUS %in% "Xanthoparmelia",]



#add in Leavitt et al. 2011 Rhizoplaca
leavitt_et_al_2011rhiz<-read.csv(file='xxxx/leavitt_et_al_2011b_rhizoplaca_SI.csv',stringsAsFactors=FALSE)

#remove herbarium code from DNA_ID
leavitt_et_al_2011rhiz$ID<-gsub("f","",leavitt_et_al_2011rhiz$ID)
leavitt_et_al_2011rhiz$ID<-gsub("[*]","",leavitt_et_al_2011rhiz$ID)
leavitt_et_al_2011rhiz$ID<-gsub("094","94",leavitt_et_al_2011rhiz$ID)

for(i in 1:nrow(leavitt_et_al_2011rhiz)) {
  if(any(leavitt_mod$Long[leavitt_mod$DNA_ID %in% leavitt_et_al_2011rhiz$ID[i] & leavitt_mod$host.GENUS %in% c("Rhizoplaca","Lecanora")] %in% NA)) {
    leavitt_mod$LL_Publication[leavitt_mod$DNA_ID %in% leavitt_et_al_2011rhiz$ID[i] & leavitt_mod$host.GENUS %in% c("Rhizoplaca","Lecanora")]<- 'Leavitt et al. (2015), Leavitt et al. (2011)'
    leavitt_mod$additional_location_info[leavitt_mod$DNA_ID %in% leavitt_et_al_2011rhiz$ID[i] & leavitt_mod$host.GENUS %in% c("Rhizoplaca","Lecanora")]<-leavitt_et_al_2011rhiz$Location[i]
    leavitt_mod$Long[leavitt_mod$DNA_ID %in% leavitt_et_al_2011rhiz$ID[i] & leavitt_mod$host.GENUS %in% c("Rhizoplaca","Lecanora")]<-leavitt_et_al_2011rhiz$Lon[i]
    leavitt_mod$Lat[leavitt_mod$DNA_ID %in% leavitt_et_al_2011rhiz$ID[i] & leavitt_mod$host.GENUS %in% c("Rhizoplaca","Lecanora")]<-leavitt_et_al_2011rhiz$Lat[i]  
  }
}

#leavitt_mod[leavitt_mod$host.GENUS %in% "Rhizoplaca",]







#add in Leavitt et al. 2013 Rhizoplaca
leavitt_et_al_2013<-readWorkbook(xlsxFile='xxxx/MASTER_rerun_LLPB.xlsx',sheet=8)

#remove herbarium code from DNA_ID
leavitt_et_al_2013$ID<-gsub(" \\(F\\)","",leavitt_et_al_2013$ID)

for(i in 1:nrow(leavitt_et_al_2013)) {
  if(any(leavitt_mod$Long[leavitt_mod$DNA_ID %in% leavitt_et_al_2013$ID[i]] %in% NA)) {
    leavitt_mod$LL_Publication[leavitt_mod$DNA_ID %in% leavitt_et_al_2013$ID[i] & leavitt_mod$host.GENUS %in% c("Rhizoplaca","Lecanora")]<- 'Leavitt et al. (2015), Leavitt et al. (2013)'
    leavitt_mod$additional_location_info[leavitt_mod$DNA_ID %in% leavitt_et_al_2013$ID[i] & leavitt_mod$host.GENUS %in% c("Rhizoplaca","Lecanora")]<-leavitt_et_al_2013$Location[i]
    leavitt_mod$Long[leavitt_mod$DNA_ID %in% leavitt_et_al_2013$ID[i] & leavitt_mod$host.GENUS %in% c("Rhizoplaca","Lecanora")]<-leavitt_et_al_2013$Lon[i]
    leavitt_mod$Lat[leavitt_mod$DNA_ID %in% leavitt_et_al_2013$ID[i] & leavitt_mod$host.GENUS %in% c("Rhizoplaca","Lecanora")]<-leavitt_et_al_2013$Lat[i]  
  }
}

#leavitt_mod[leavitt_mod$host.GENUS %in% "Rhizoplaca",]


#write.csv(leavitt_mod[leavitt_mod$host.GENUS %in% "Rhizoplaca",],file="~/Desktop/rhiz.csv")




#Add Leavitt et al. Oropogon data in
oropogon<-readWorkbook(xlsxFile='xxxx/AJB_2012_Oropogon.xlsx')

#remove herbarium code from DNA_ID
oropogon$DNA_ID<-gsub(" \\(F\\)","",oropogon$DNA_ID)

for(i in 1:nrow(oropogon)) {
  if(any(leavitt_mod$Lat[leavitt_mod$DNA_ID %in% oropogon$DNA_ID[i]] %in% NA)) {
    if(any(grep('Site 1',oropogon$Collection_locality[i]))) {
      leavitt_mod$Lat[leavitt_mod$DNA_ID %in% oropogon$DNA_ID[i] & leavitt_mod$host.GENUS %in% "Oropogon"]<-'17.5511'
      leavitt_mod$Long[leavitt_mod$DNA_ID %in% oropogon$DNA_ID[i] & leavitt_mod$host.GENUS %in% "Oropogon"]<-'-96.5116'
      leavitt_mod$additional_location_info[leavitt_mod$DNA_ID %in% oropogon$DNA_ID[i] & leavitt_mod$host.GENUS %in% "Oropogon"]<-'Estado de Oaxaca, Mexico'
      leavitt_mod$LL_Publication[leavitt_mod$DNA_ID %in% oropogon$DNA_ID[i] & leavitt_mod$host.GENUS %in% "Oropogon"]<-'Leavitt et al. (2015), Leavitt et al. (2012)'
    }else if(any(grep('Site 2',oropogon$Collection_locality[i]))){
      leavitt_mod$Lat[leavitt_mod$DNA_ID %in% oropogon$DNA_ID[i] & leavitt_mod$host.GENUS %in% "Oropogon"]<-'17.1579'
      leavitt_mod$Long[leavitt_mod$DNA_ID %in% oropogon$DNA_ID[i] & leavitt_mod$host.GENUS %in% "Oropogon"]<-'-96.0690'
      leavitt_mod$additional_location_info[leavitt_mod$DNA_ID %in% oropogon$DNA_ID[i] & leavitt_mod$host.GENUS %in% "Oropogon"]<-'Estado de Oaxaca, Mexico'
      leavitt_mod$LL_Publication[leavitt_mod$DNA_ID %in% oropogon$DNA_ID[i] & leavitt_mod$host.GENUS %in% "Oropogon"]<-'Leavitt et al. (2015), Leavitt et al. (2012)'      
    }else if(any(grep('Site 3',oropogon$Collection_locality[i]))){
      leavitt_mod$Lat[leavitt_mod$DNA_ID %in% oropogon$DNA_ID[i] & leavitt_mod$host.GENUS %in% "Oropogon"]<-'17.3870'
      leavitt_mod$Long[leavitt_mod$DNA_ID %in% oropogon$DNA_ID[i] & leavitt_mod$host.GENUS %in% "Oropogon"]<-'-96.4685'
      leavitt_mod$additional_location_info[leavitt_mod$DNA_ID %in% oropogon$DNA_ID[i] & leavitt_mod$host.GENUS %in% "Oropogon"]<-'Estado de Oaxaca, Mexico'
      leavitt_mod$LL_Publication[leavitt_mod$DNA_ID %in% oropogon$DNA_ID[i] & leavitt_mod$host.GENUS %in% "Oropogon"]<-'Leavitt et al. (2015), Leavitt et al. (2012)'      
    }else if(any(grep('Site 4',oropogon$Collection_locality[i]))){
      leavitt_mod$Lat[leavitt_mod$DNA_ID %in% oropogon$DNA_ID[i] & leavitt_mod$host.GENUS %in% "Oropogon"]<-'17.4069'
      leavitt_mod$Long[leavitt_mod$DNA_ID %in% oropogon$DNA_ID[i] & leavitt_mod$host.GENUS %in% "Oropogon"]<-'-96.5006'
      leavitt_mod$additional_location_info[leavitt_mod$DNA_ID %in% oropogon$DNA_ID[i] & leavitt_mod$host.GENUS %in% "Oropogon"]<-'Estado de Oaxaca, Mexico'
      leavitt_mod$LL_Publication[leavitt_mod$DNA_ID %in% oropogon$DNA_ID[i] & leavitt_mod$host.GENUS %in% "Oropogon"]<-'Leavitt et al. (2015), Leavitt et al. (2012)'      
    }else if(any(grep('Site 5',oropogon$Collection_locality[i]))){
      leavitt_mod$Lat[leavitt_mod$DNA_ID %in% oropogon$DNA_ID[i] & leavitt_mod$host.GENUS %in% "Oropogon"]<-'17.5226'
      leavitt_mod$Long[leavitt_mod$DNA_ID %in% oropogon$DNA_ID[i] & leavitt_mod$host.GENUS %in% "Oropogon"]<-'-96.5112'
      leavitt_mod$additional_location_info[leavitt_mod$DNA_ID %in% oropogon$DNA_ID[i] & leavitt_mod$host.GENUS %in% "Oropogon"]<-'Estado de Oaxaca, Mexico'
      leavitt_mod$LL_Publication[leavitt_mod$DNA_ID %in% oropogon$DNA_ID[i] & leavitt_mod$host.GENUS %in% "Oropogon"]<-'Leavitt et al. (2015), Leavitt et al. (2012)'      
    }else if(any(grep('Site 6',oropogon$Collection_locality[i]))){
      leavitt_mod$Lat[leavitt_mod$DNA_ID %in% oropogon$DNA_ID[i] & leavitt_mod$host.GENUS %in% "Oropogon"]<-'17.2944'
      leavitt_mod$Long[leavitt_mod$DNA_ID %in% oropogon$DNA_ID[i] & leavitt_mod$host.GENUS %in% "Oropogon"]<-'-96.3102'
      leavitt_mod$additional_location_info[leavitt_mod$DNA_ID %in% oropogon$DNA_ID[i] & leavitt_mod$host.GENUS %in% "Oropogon"]<-'Estado de Oaxaca, Mexico'
      leavitt_mod$LL_Publication[leavitt_mod$DNA_ID %in% oropogon$DNA_ID[i] & leavitt_mod$host.GENUS %in% "Oropogon"]<-'Leavitt et al. (2015), Leavitt et al. (2012)'     
    }else if(any(grep('Site 7',oropogon$Collection_locality[i]))){
      leavitt_mod$Lat[leavitt_mod$DNA_ID %in% oropogon$DNA_ID[i] & leavitt_mod$host.GENUS %in% "Oropogon"]<-'17.2648'
      leavitt_mod$Long[leavitt_mod$DNA_ID %in% oropogon$DNA_ID[i] & leavitt_mod$host.GENUS %in% "Oropogon"]<-'-96.3593'
      leavitt_mod$additional_location_info[leavitt_mod$DNA_ID %in% oropogon$DNA_ID[i] & leavitt_mod$host.GENUS %in% "Oropogon"]<-'Estado de Oaxaca, Mexico'
      leavitt_mod$LL_Publication[leavitt_mod$DNA_ID %in% oropogon$DNA_ID[i] & leavitt_mod$host.GENUS %in% "Oropogon"]<-'Leavitt et al. (2015), Leavitt et al. (2012)'     
    }else if(any(grep('Site 8',oropogon$Collection_locality[i]))){
      leavitt_mod$Lat[leavitt_mod$DNA_ID %in% oropogon$DNA_ID[i] & leavitt_mod$host.GENUS %in% "Oropogon"]<-'17.1133'
      leavitt_mod$Long[leavitt_mod$DNA_ID %in% oropogon$DNA_ID[i] & leavitt_mod$host.GENUS %in% "Oropogon"]<-'-96.4818'
      leavitt_mod$additional_location_info[leavitt_mod$DNA_ID %in% oropogon$DNA_ID[i] & leavitt_mod$host.GENUS %in% "Oropogon"]<-'Estado de Oaxaca, Mexico'
      leavitt_mod$LL_Publication[leavitt_mod$DNA_ID %in% oropogon$DNA_ID[i] & leavitt_mod$host.GENUS %in% "Oropogon"]<-'Leavitt et al. (2015), Leavitt et al. (2012)'     
    }else if(any(grep('Site 9',oropogon$Collection_locality[i]))){
      leavitt_mod$Lat[leavitt_mod$DNA_ID %in% oropogon$DNA_ID[i] & leavitt_mod$host.GENUS %in% "Oropogon"]<-'17.1211'
      leavitt_mod$Long[leavitt_mod$DNA_ID %in% oropogon$DNA_ID[i] & leavitt_mod$host.GENUS %in% "Oropogon"]<-'-96.4461'
      leavitt_mod$additional_location_info[leavitt_mod$DNA_ID %in% oropogon$DNA_ID[i] & leavitt_mod$host.GENUS %in% "Oropogon"]<-'Estado de Oaxaca, Mexico'
      leavitt_mod$LL_Publication[leavitt_mod$DNA_ID %in% oropogon$DNA_ID[i] & leavitt_mod$host.GENUS %in% "Oropogon"]<-'Leavitt et al. (2015), Leavitt et al. (2012)'      
    }else if(any(grep('Site 10',oropogon$Collection_locality[i]))){
      leavitt_mod$Lat[leavitt_mod$DNA_ID %in% oropogon$DNA_ID[i] & leavitt_mod$host.GENUS %in% "Oropogon"]<-'17.3571'
      leavitt_mod$Long[leavitt_mod$DNA_ID %in% oropogon$DNA_ID[i] & leavitt_mod$host.GENUS %in% "Oropogon"]<-'-96.4706'
      leavitt_mod$additional_location_info[leavitt_mod$DNA_ID %in% oropogon$DNA_ID[i] & leavitt_mod$host.GENUS %in% "Oropogon"]<-'Estado de Oaxaca, Mexico'
      leavitt_mod$LL_Publication[leavitt_mod$DNA_ID %in% oropogon$DNA_ID[i] & leavitt_mod$host.GENUS %in% "Oropogon"]<-'Leavitt et al. (2015), Leavitt et al. (2012)'      
    }
  }
}

#leavitt_mod[leavitt_mod$host.GENUS %in% "Oropogon",]




#Add Alors et al. Parmlina data to Leavitt et al. 2015
alors<-readWorkbook(xlsxFile='xxxx/alors_et_al_2017_parmelina_SI.xlsx')

for(i in 1:nrow(alors)){
  if(any(grep(' N',alors$Latitude[i]))) {
    alors$Latitude[i]<-gsub(' N','',alors$Latitude[i]) 
  }
  if(any(grep(' E',alors$Longitude[i]))) {
   alors$Longitude[i]<-gsub(' E','',alors$Longitude[i])   
  }
  if(any(grep(' S',alors$Latitude[i]))) {
    alors$Latitude[i]<-paste('-',gsub(' S','',alors$Latitude[i]),sep='')
  }
  if(any(grep(' W',alors$Longitude[i]))) {
    alors$Longitude[i]<-paste('-',gsub(' W','',alors$Longitude[i]),sep='')
  }
}



#remove herbarium code from DNA_ID
alors$DNA<-gsub(" \\(MAF-Lich\\)","",alors$DNA)
alors$DNA<-gsub(" \\(F\\)","",alors$DNA)
alors[duplicated(alors$DNA),]


for(i in 1:nrow(alors)) {
  if(any(leavitt_mod$Lat[leavitt_mod$DNA_ID %in% alors$DNA[i]] %in% NA)) {
      leavitt_mod$Lat[leavitt_mod$DNA_ID %in% alors$DNA[i] & leavitt_mod$host.GENUS %in% "Parmelina"]<-alors$Latitude[i]
      leavitt_mod$Long[leavitt_mod$DNA_ID %in% alors$DNA[i] & leavitt_mod$host.GENUS %in% "Parmelina"]<-alors$Longitude[i]
      leavitt_mod$LL_Publication[leavitt_mod$DNA_ID %in% alors$DNA[i] & leavitt_mod$host.GENUS %in% "Parmelina"]<-'Leavitt et al. (2015), Alors et al. (2017)'
    }
    if(any(leavitt_mod$additional_location_info[leavitt_mod$DNA_ID %in% alors$DNA[i]] %in% NA)) {
      leavitt_mod$additional_location_info[leavitt_mod$DNA_ID %in% alors$DNA[i] & leavitt_mod$host.GENUS %in% "Parmelina"]<-alors$Population[i]
	}
}  


#leavitt_mod[leavitt_mod$host.GENUS %in% "Parmelina",]

#make sure look ok
#unique(leavitt_mod$LL_Publication[leavitt_mod$host.GENUS %in% "Xanthoparmelia"])
#unique(leavitt_mod$LL_Publication[leavitt_mod$host.GENUS %in% c("Lecanora","Rhizoplaca")])
#unique(leavitt_mod$LL_Publication[leavitt_mod$host.GENUS %in% "Oropogon"])
#unique(leavitt_mod$LL_Publication[leavitt_mod$host.GENUS %in% "Parmelina"])


#handful need longitude turned negative
accs<-c("KR913050","KR913054","KR913035","KR913267","KR912508","KR913057","KR912548")
for(x in 1:length(accs)){
	leavitt_mod$Long[leavitt_mod$GenBank.ITS %in% accs[x]]<-(-1)*as.numeric(leavitt_mod$Long[leavitt_mod$GenBank.ITS %in% accs[x]])
}
#save updated Leavitt et al. 2015
write.csv(leavitt_mod,file="xxxx/leavitt_et_al_2015_updated.csv",row.names=FALSE)














#######################
#######################


require(openxlsx)


#take original file that had GenBank citation and lat long added to it.

missing<-read.csv("/xxxx/MASTER_accession_list.csv",stringsAsFactors=FALSE)

#merge together with earlier version in which lat/long and publications were searched for in NCBI
missing.old<-read.csv("/xxxx/MASTER_accession_list_haps_added_NCBI_LLPB.csv",stringsAsFactors=FALSE)

missing[,c("NCBI_Lat","NCBI_Lon","NCBI_Title","NCBI_Journal","NCBI_First_Author")]<-NA
for(x in 1:nrow(missing)){
	missing[x,c("NCBI_Lat","NCBI_Lon","NCBI_Title","NCBI_Journal","NCBI_First_Author")]<-missing.old[missing.old$Acc_No %in% missing$Acc_No[x],c("NCBI_Lat","NCBI_Lon","NCBI_Title","NCBI_Journal","NCBI_First_Author")]
}


#correct some entries (KP314641,KP314669,MG098140,KU318576) that have Parmeliaceae\tLecanorales\tLecanoromycetes for family and lack order/class info
missing$Host_Class[missing$Host_Family %in% "Parmeliaceae\tLecanorales\tLecanoromycetes"]<-"Lecanoromycetes"
missing$Host_Order[missing$Host_Family %in% "Parmeliaceae\tLecanorales\tLecanoromycetes"]<-"Lecanorales"
missing$Host_Family[missing$Host_Family %in% "Parmeliaceae\tLecanorales\tLecanoromycetes"]<-"Parmeliaceae"



#add substrate and culture columns
#add a column for host information source
missing$Host_Publication<-NA
missing$Host_Publication[missing$Host_Man_Cur %in% "No"]<-"NCBI"
missing$Host_Publication[missing$Host_Man_Cur %in% "Yes"]<-missing$NCBI_First_Author[missing$Host_Man_Cur %in% "Yes"]
missing$Culture<-NA
missing$Substrate<-NA
missing$Substrate_Publication<-NA
missing$Substrate_Man_Cur<-"No"
missing$Lat<-NA
missing$Long<-NA
missing$LL_Publication<-NA
missing$LL_Publication[!is.na(missing$NCBI_Lat)]<-"NCBI"
missing$LL_Man_Cur<-"No"

#previous scripts missed some areas where host was included in GB (had previously filled in manually) - add these to GB columns, and in future, include terms "trebouxioid photobiont..." and one for Usnea ceratina.  Added notes that these were in GB and not added manually
accs<-c("MG687501","MG687508","MG687506","MG687507","MG687504","MG687505","MG687502","MG687503","MG687500","MG687518","MG687509","MG687519","MG687511","MG687510","MG687513","MG687512","MG687515","MG687514","MG687517","MG687516","KY033354")
missing$Host_GB_Raw[missing$Acc_No %in% accs]<-missing$Host_Detailed[missing$Acc_No %in% accs]
missing$Host_GB_RawMod[missing$Acc_No %in% accs]<-missing$Host_Detailed[missing$Acc_No %in% accs]
missing$Host_GB_Genus_Mod[missing$Acc_No %in% accs]<-missing$Host_Genus[missing$Acc_No %in% accs]
missing$Host_GB_Species_Mod[missing$Acc_No %in% accs]<-missing$Host_Species[missing$Acc_No %in% accs]
missing$Host_Man_Cur[missing$Acc_No %in% accs]<-"No"

#move some substrate info from GenBank and correct that this was included from there and not the refs.
accs<-c("FM945343","FM945345","FM945344","FM945346","JF831903","JF831904","JF831923","JF831921","JF831919","JF831917","JF831915","JF831913","JF831911","JF831909","JF831907","JF831905","JF831922","JF831920","JF831918","JF831916","JF831914","JF831912","JF831910","JF831908","JF831906","EF432561","AY842262","AY842263","AY842269","AY842268","AY842267","AY842264","AY842265","AY842273","AY842270","AY842266","AY842275","AY842272","AY842271","AY842274","AY667580","AJ511355","AJ511359","AJ511357","AJ511356","AJ511360","AJ511361","AJ511362","AJ511366","AJ511365","AJ511353","AJ511351","AJ511352","AJ511350","AJ511354","AJ511363","AJ511364","AJ511358","AY928201","AY928200","AY928199","EU795079","EU795082","EU795054","EU795063","EU795055","EU795060","EU795062","EU795053","EU795058","EU795075","EU795057","EU795061","EU795056","EU795052","EU795064","EU795059","EU795081","EU795078","EU795077","EU795074","EU795080","EU795072","EU795065","EU795068","EU795067","EU795066","EU795071","EU795070","EU795073","EU795076","EU795069")
missing$Substrate[missing$Acc_No %in% accs]<-missing$Isol_GB_Raw[missing$Acc_No %in% accs]
missing$Substrate_Publication[missing$Acc_No %in% accs]<-"NCBI"
missing$Substrate_Man_Cur[missing$Acc_No %in% accs]<-"No"



#Add Clade name
missing$Clade[x]<-NA
for(x in 1:nrow(missing)){
	if(!is.na(missing$OTU_97.5_Raw_28_Oct_2019[x])){
		missing$Clade[x]<-strsplit(missing$OTU_97.5_Raw_28_Oct_2019[x],"_")[[1]][1]
	}
}


#NEED TO CORRECT SOME OTHER SVALBARD ACCESSIONS FROM NCBI TO BE FROM NORWAY

missing$Country[missing$Country_Detailed %in% "Svalbard"]<-"Norway"
missing$Geo_Man_Cur[missing$Country_Detailed %in% "Svalbard"]<-"Yes"
missing$DB_Notes[missing$Country_Detailed %in% "Svalbard"]<-""
missing$ISO3Code[missing$Country_Detailed %in% "Svalbard"]<-"NOR"
missing$GEO3SubRegion[missing$Country_Detailed %in% "Svalbard"]<-"Western Europe"
missing$GEO3MajorRegion[missing$Country_Detailed %in% "Svalbard"]<-"Europe"
missing$Country_Detailed[missing$Country_Detailed %in% "Svalbard"]<-"Norway"


#take off NSEW and put negative for SW
for(i in 1:nrow(missing)){
  if(any(grep(' N',missing$NCBI_Lat[i]))) {
    missing$NCBI_Lat[i]<-gsub(' N','',missing$NCBI_Lat[i]) 
  }
  if(any(grep(' E',missing$NCBI_Lon[i]))) {
   missing$NCBI_Lon[i]<-gsub(' E','',missing$NCBI_Lon[i])   
  }
  if(any(grep(' S',missing$NCBI_Lat[i]))) {
    missing$NCBI_Lat[i]<-paste('-',gsub(' S','',missing$NCBI_Lat[i]),sep='')
  }
  if(any(grep(' W',missing$NCBI_Lon[i]))) {
    missing$NCBI_Lon[i]<-paste('-',gsub(' W','',missing$NCBI_Lon[i]),sep='')
  }
}

#do some standardization
for(i in 1:nrow(missing)) {
  if(missing$Isol_GB_Raw[i] %in% c('none','None')) {
    missing$Isol_GB_Raw[i]<-NA
  }
  if(missing$Isolation_Source[i] %in% c('none','None')){
    missing$Isolation_Source<-NA
  }
  if(missing$Host[i] %in% c('none','None')){
    missing$Host[i]<-NA
    missing$Host_Genus[i]<-NA
    missing$Host_Species[i]<-NA
  }
  if(missing$Country[i] %in% c('none','None')){
    missing$Country[i]<-NA
  }
}


#NEED TO THEN ADD Leavitt et al. 2013 IN
leavitt2013<-read.csv(file="xxxx/leavitt_et_al_2013_mpn18sep2019.csv",stringsAsFactors=FALSE)
for(x in 1:nrow(leavitt2013)){
	if(!is.na(leavitt2013$Lat[x])){
		missing$Lat[missing$Acc_No %in% leavitt2013$GenBank.Accession..[x]]<-leavitt2013$Lat[x]
		missing$Long[missing$Acc_No %in% leavitt2013$GenBank.Accession..[x]]<-leavitt2013$Long[x]
		missing$LL_Publication[missing$Acc_No %in% leavitt2013$GenBank.Accession..[x]]<-"Leavitt et al. (2013)"
		missing$LL_Man_Cur[missing$Acc_No %in% leavitt2013$GenBank.Accession..[x]]<-"Yes"
	}
}

for(x in 1:nrow(leavitt2013)){
	if(!is.na(leavitt2013$Substrate[x])){
		missing$Substrate[missing$Acc_No %in% leavitt2013$GenBank.Accession..[x]]<-leavitt2013$Substrate[x]
		missing$Substrate_Publication[missing$Acc_No %in% leavitt2013$GenBank.Accession..[x]]<-"Leavitt et al. (2013)"
		missing$Substrate_Man_Cur[missing$Acc_No %in% leavitt2013$GenBank.Accession..[x]]<-"Yes"
	}
}









#NEED TO THEN ADD THIS IN
leavitt_mod<-read.csv(file="xxxx/leavitt_et_al_2015_updated.csv",stringsAsFactors=FALSE)
for(x in 1:nrow(leavitt_mod)){
	if(!is.na(leavitt_mod$Lat[x])){
		missing$Lat[missing$Acc_No %in% leavitt_mod$GenBank.ITS[x]]<-leavitt_mod$Lat[x]
		missing$Long[missing$Acc_No %in% leavitt_mod$GenBank.ITS[x]]<-leavitt_mod$Long[x]
		missing$LL_Publication[missing$Acc_No %in% leavitt_mod$GenBank.ITS[x]]<-leavitt_mod$LL_Publication[x]
		missing$LL_Man_Cur[missing$Acc_No %in% leavitt_mod$GenBank.ITS[x]]<-"Yes"
	}
}


#adds substrate info to columns and if cultures, add that in.
for(i in 1:nrow(leavitt_mod)){
  if(any(missing$Substrate[missing$Acc_No %in% leavitt_mod$GenBank.ITS[i]] %in% c('None',NA))){
    if(leavitt_mod$SUBSTRATE[i] %in% c('saxicolous','terricolous','corticolous')){
        missing$Substrate[missing$Acc_No %in% leavitt_mod$GenBank.ITS[i]]<-leavitt_mod$SUBSTRATE[i]
		missing$Substrate_Publication[missing$Acc_No %in% leavitt_mod$GenBank.ITS[i]]<-"Leavitt et al. (2015)"
		missing$Substrate_Man_Cur[missing$Acc_No %in% leavitt_mod$GenBank.ITS[i]]<-"Yes"
    }else if((any(grep(c('SAG|UTEX|CCAP'),leavitt_mod$SUBSTRATE[i]))) %in% 'TRUE'){
        #Have not made separate column for info on adding culture info/publication
        missing$Culture[missing$Acc_No %in% leavitt_mod$GenBank.ITS[i]]<-leavitt_mod$SUBSTRATE[i]
    }
  }
} 

#write.csv(missing,"~/Desktop/stuff.csv",row.names=FALSE)







#Ruprecht et al. 2014
ruprecht<-read.csv(file="xxxx/ruprecht_et_al_2014_psora_photobionts_SI1.csv",stringsAsFactors=FALSE)

#For Trebouxia
for(i in 1:nrow(ruprecht)) {
  if(any(missing$Lat[missing$Acc_No %in% ruprecht$Trebouxia_ITS[i]] %in% NA)) {
    missing$Substrate[missing$Acc_No %in% ruprecht$Trebouxia_ITS[i]]<-ruprecht$Substrate[i]
	missing$Substrate_Publication[missing$Acc_No %in% ruprecht$Trebouxia_ITS[i]]<-"Ruprecht et al. (2014)"
	missing$Substrate_Man_Cur[missing$Acc_No %in% ruprecht$Trebouxia_ITS[i]]<-"Yes"
  }
  if(any(missing$Lat[missing$Acc_No %in% ruprecht$Trebouxia_ITS[i]] %in% NA)) {
    if(ruprecht$Investigation_sites[i] %in% 'Tabernas/Spain') {
      missing$Lat[missing$Acc_No %in% ruprecht$Trebouxia_ITS[i]]<-'37.0127'
      missing$Long[missing$Acc_No %in% ruprecht$Trebouxia_ITS[i]]<-'-2.4356'
	  missing$LL_Publication[missing$Acc_No %in% ruprecht$Trebouxia_ITS[i]]<-"Ruprecht et al. (2014)"
	  missing$LL_Man_Cur[missing$Acc_No %in% ruprecht$Trebouxia_ITS[i]]<-"Yes"
    }else if(ruprecht$Investigation_sites[i] %in% 'Gynge Alvar/Sweden') {
      missing$Lat[missing$Acc_No %in% ruprecht$Trebouxia_ITS[i]]<-'56.5421'
      missing$Long[missing$Acc_No %in% ruprecht$Trebouxia_ITS[i]]<-'16.4784'
	  missing$LL_Publication[missing$Acc_No %in% ruprecht$Trebouxia_ITS[i]]<-"Ruprecht et al. (2014)"
	  missing$LL_Man_Cur[missing$Acc_No %in% ruprecht$Trebouxia_ITS[i]]<-"Yes"
    }else if(ruprecht$Investigation_sites[i] %in% 'Hochtor/Austria') {
      missing$Lat[missing$Acc_No %in% ruprecht$Trebouxia_ITS[i]]<-'47.0833'
      missing$Long[missing$Acc_No %in% ruprecht$Trebouxia_ITS[i]]<-'12.85'
	  missing$LL_Publication[missing$Acc_No %in% ruprecht$Trebouxia_ITS[i]]<-"Ruprecht et al. (2014)"
	  missing$LL_Man_Cur[missing$Acc_No %in% ruprecht$Trebouxia_ITS[i]]<-"Yes"
    }else if(ruprecht$Investigation_sites[i] %in% 'Ruine Homburg/Germany') {
      missing$Lat[missing$Acc_No %in% ruprecht$Trebouxia_ITS[i]]<-'50.0167'
      missing$Long[missing$Acc_No %in% ruprecht$Trebouxia_ITS[i]]<-'9.8'
	  missing$LL_Publication[missing$Acc_No %in% ruprecht$Trebouxia_ITS[i]]<-"Ruprecht et al. (2014)"
	  missing$LL_Man_Cur[missing$Acc_No %in% ruprecht$Trebouxia_ITS[i]]<-"Yes"
    }
  }
}

#For Asterochloris
for(i in 1:nrow(ruprecht)) {
  if(any(missing$Lat[missing$Acc_No %in% ruprecht$Asterochloris_ITS[i]] %in% NA)) {
    missing$Substrate[missing$Acc_No %in% ruprecht$Asterochloris_ITS[i]]<-ruprecht$Substrate[i]
	missing$Substrate_Publication[missing$Acc_No %in% ruprecht$Asterochloris_ITS[i]]<-"Ruprecht et al. (2014)"
	missing$Substrate_Man_Cur[missing$Acc_No %in% ruprecht$Asterochloris_ITS[i]]<-"Yes"
  }
  if(any(missing$Lat[missing$Acc_No %in% ruprecht$Asterochloris_ITS[i]] %in% NA)) {
    if(ruprecht$Investigation_sites[i] %in% 'Tabernas/Spain') {
      missing$Lat[missing$Acc_No %in% ruprecht$Asterochloris_ITS[i]]<-'37.0127'
      missing$Long[missing$Acc_No %in% ruprecht$Asterochloris_ITS[i]]<-'-2.4356'
	  missing$LL_Publication[missing$Acc_No %in% ruprecht$Asterochloris_ITS[i]]<-"Ruprecht et al. (2014)"
	  missing$LL_Man_Cur[missing$Acc_No %in% ruprecht$Asterochloris_ITS[i]]<-"Yes"
    }else if(ruprecht$Investigation_sites[i] %in% 'Gynge Alvar/Sweden') {
      missing$Lat[missing$Acc_No %in% ruprecht$Asterochloris_ITS[i]]<-'56.5421'
      missing$Long[missing$Acc_No %in% ruprecht$Asterochloris_ITS[i]]<-'16.4784'
	  missing$LL_Publication[missing$Acc_No %in% ruprecht$Asterochloris_ITS[i]]<-"Ruprecht et al. (2014)"
	  missing$LL_Man_Cur[missing$Acc_No %in% ruprecht$Asterochloris_ITS[i]]<-"Yes"
    }else if(ruprecht$Investigation_sites[i] %in% 'Hochtor/Austria') {
      missing$Lat[missing$Acc_No %in% ruprecht$Asterochloris_ITS[i]]<-'47.0833'
      missing$Long[missing$Acc_No %in% ruprecht$Asterochloris_ITS[i]]<-'12.85'
	  missing$LL_Publication[missing$Acc_No %in% ruprecht$Asterochloris_ITS[i]]<-"Ruprecht et al. (2014)"
	  missing$LL_Man_Cur[missing$Acc_No %in% ruprecht$Asterochloris_ITS[i]]<-"Yes"
    }else if(ruprecht$Investigation_sites[i] %in% 'Ruine Homburg/Germany') {
      missing$Lat[missing$Acc_No %in% ruprecht$Asterochloris_ITS[i]]<-'50.0167'
      missing$Long[missing$Acc_No %in% ruprecht$Asterochloris_ITS[i]]<-'9.8'
	  missing$LL_Publication[missing$Acc_No %in% ruprecht$Asterochloris_ITS[i]]<-"Ruprecht et al. (2014)"
	  missing$LL_Man_Cur[missing$Acc_No %in% ruprecht$Asterochloris_ITS[i]]<-"Yes"
    }
  }
}

#For Chloroidium
for(i in 1:nrow(ruprecht)) {
  if(any(missing$Lat[missing$Acc_No %in% ruprecht$Chloroidium_ITS[i]] %in% NA)) {
    missing$Substrate[missing$Acc_No %in% ruprecht$Chloroidium_ITS[i]]<-ruprecht$Substrate[i]
	missing$Substrate_Publication[missing$Acc_No %in% ruprecht$Chloroidium_ITS[i]]<-"Ruprecht et al. (2014)"
	missing$Substrate_Man_Cur[missing$Acc_No %in% ruprecht$Chloroidium_ITS[i]]<-"Yes"
  }
  if(any(missing$Lat[missing$Acc_No %in% ruprecht$Chloroidium_ITS[i]] %in% NA)) {
    if(ruprecht$Investigation_sites[i] %in% 'Tabernas/Spain') {
      missing$Lat[missing$Acc_No %in% ruprecht$Chloroidium_ITS[i]]<-'37.0127'
      missing$Long[missing$Acc_No %in% ruprecht$Chloroidium_ITS[i]]<-'-2.4356'
	  missing$LL_Publication[missing$Acc_No %in% ruprecht$Chloroidium_ITS[i]]<-"Ruprecht et al. (2014)"
	  missing$LL_Man_Cur[missing$Acc_No %in% ruprecht$Chloroidium_ITS[i]]<-"Yes"
    }else if(ruprecht$Investigation_sites[i] %in% 'Gynge Alvar/Sweden') {
      missing$Lat[missing$Acc_No %in% ruprecht$Chloroidium_ITS[i]]<-'56.5421'
      missing$Long[missing$Acc_No %in% ruprecht$Chloroidium_ITS[i]]<-'16.4784'
	  missing$LL_Publication[missing$Acc_No %in% ruprecht$Chloroidium_ITS[i]]<-"Ruprecht et al. (2014)"
	  missing$LL_Man_Cur[missing$Acc_No %in% ruprecht$Chloroidium_ITS[i]]<-"Yes"
    }else if(ruprecht$Investigation_sites[i] %in% 'Hochtor/Austria') {
      missing$Lat[missing$Acc_No %in% ruprecht$Chloroidium_ITS[i]]<-'47.0833'
      missing$Long[missing$Acc_No %in% ruprecht$Chloroidium_ITS[i]]<-'12.85'
	  missing$LL_Publication[missing$Acc_No %in% ruprecht$Chloroidium_ITS[i]]<-"Ruprecht et al. (2014)"
	  missing$LL_Man_Cur[missing$Acc_No %in% ruprecht$Chloroidium_ITS[i]]<-"Yes"
    }else if(ruprecht$Investigation_sites[i] %in% 'Ruine Homburg/Germany') {
      missing$Lat[missing$Acc_No %in% ruprecht$Chloroidium_ITS[i]]<-'50.0167'
      missing$Long[missing$Acc_No %in% ruprecht$Chloroidium_ITS[i]]<-'9.8'
	  missing$LL_Publication[missing$Acc_No %in% ruprecht$Chloroidium_ITS[i]]<-"Ruprecht et al. (2014)"
	  missing$LL_Man_Cur[missing$Acc_No %in% ruprecht$Chloroidium_ITS[i]]<-"Yes"
    }
  }
}

#For Other_Algae
for(i in 1:nrow(ruprecht)) {
  if(any(missing$Lat[missing$Acc_No %in% ruprecht$Other_Algae_ITS[i]] %in% NA)) {
    missing$Substrate[missing$Acc_No %in% ruprecht$Other_Algae_ITS[i]]<-ruprecht$Substrate[i]
	missing$Substrate_Publication[missing$Acc_No %in% ruprecht$Other_Algae_ITS[i]]<-"Ruprecht et al. (2014)"
	missing$Substrate_Man_Cur[missing$Acc_No %in% ruprecht$Other_Algae_ITS[i]]<-"Yes"
  }
  if(any(missing$Lat[missing$Acc_No %in% ruprecht$Other_Algae_ITS[i]] %in% NA)) {
    if(ruprecht$Investigation_sites[i] %in% 'Tabernas/Spain') {
      missing$Lat[missing$Acc_No %in% ruprecht$Other_Algae_ITS[i]]<-'37.0127'
      missing$Long[missing$Acc_No %in% ruprecht$Other_Algae_ITS[i]]<-'-2.4356'
	  missing$LL_Publication[missing$Acc_No %in% ruprecht$Other_Algae_ITS[i]]<-"Ruprecht et al. (2014)"
	  missing$LL_Man_Cur[missing$Acc_No %in% ruprecht$Other_Algae_ITS[i]]<-"Yes"
    }else if(ruprecht$Investigation_sites[i] %in% 'Gynge Alvar/Sweden') {
      missing$Lat[missing$Acc_No %in% ruprecht$Other_Algae_ITS[i]]<-'56.5421'
      missing$Long[missing$Acc_No %in% ruprecht$Other_Algae_ITS[i]]<-'16.4784'
	  missing$LL_Publication[missing$Acc_No %in% ruprecht$Other_Algae_ITS[i]]<-"Ruprecht et al. (2014)"
	  missing$LL_Man_Cur[missing$Acc_No %in% ruprecht$Other_Algae_ITS[i]]<-"Yes"
    }else if(ruprecht$Investigation_sites[i] %in% 'Hochtor/Austria') {
      missing$Lat[missing$Acc_No %in% ruprecht$Other_Algae_ITS[i]]<-'47.0833'
      missing$Long[missing$Acc_No %in% ruprecht$Other_Algae_ITS[i]]<-'12.85'
	  missing$LL_Publication[missing$Acc_No %in% ruprecht$Other_Algae_ITS[i]]<-"Ruprecht et al. (2014)"
	  missing$LL_Man_Cur[missing$Acc_No %in% ruprecht$Other_Algae_ITS[i]]<-"Yes"
    }else if(ruprecht$Investigation_sites[i] %in% 'Ruine Homburg/Germany') {
      missing$Lat[missing$Acc_No %in% ruprecht$Other_Algae_ITS[i]]<-'50.0167'
      missing$Long[missing$Acc_No %in% ruprecht$Other_Algae_ITS[i]]<-'9.8'
	  missing$LL_Publication[missing$Acc_No %in% ruprecht$Other_Algae_ITS[i]]<-"Ruprecht et al. (2014)"
	  missing$LL_Man_Cur[missing$Acc_No %in% ruprecht$Other_Algae_ITS[i]]<-"Yes"
    }
  }
}

#write.csv(missing,"~/Desktop/stuff.csv",row.names=FALSE)



#Paul et al.
paul<-read.csv(file="xxxx/paul_et_al_2018_SI2.csv",stringsAsFactors=FALSE)
paul$Location<-NA

#read dal Grande et al. (2018) to add site info to Paul et al.
dg<-readWorkbook(xlsxFile='xxxx/dal_grande_et_al_2018_SI.xlsx')

for(x in 1:nrow(paul)){
	paul$Location[x]<-dg$location[dg$Sample %in% paul$Sample[x]]
}


for(i in 1:nrow(paul)) {
  if(any(missing$NCBI_Lat[missing$Acc_No %in% paul$ITS_GenBank[i]] %in% NA)) {
    if(paul$Location[i]=='I') {
      missing$Lat[missing$Acc_No %in% paul$ITS_GenBank[i]]<-'40.203'
      missing$Long[missing$Acc_No %in% paul$ITS_GenBank[i]]<-'-5.233'
      missing$Substrate[missing$Acc_No %in% paul$ITS_GenBank[i]]<-'Saxicolous'
      missing$LL_Publication[missing$Acc_No %in% paul$ITS_GenBank[i]]<-'Paul et al. (2018), dal Grande et al. (2018)'
      missing$Substrate_Man_Cur[missing$Acc_No %in% paul$ITS_GenBank[i]]<-'Yes'
      missing$Substrate_Publication[missing$Acc_No %in% paul$ITS_GenBank[i]]<-'Paul et al. (2018), dal Grande et al. (2018)'
      missing$LL_Man_Cur[missing$Acc_No %in% paul$ITS_GenBank[i]]<-'Yes'
    }else if(paul$Location[i]=='II') {
      missing$Lat[missing$Acc_No %in% paul$ITS_GenBank[i]]<-'40.207'
      missing$Long[missing$Acc_No %in% paul$ITS_GenBank[i]]<-'-5.233'
      missing$Substrate[missing$Acc_No %in% paul$ITS_GenBank[i]]<-'Saxicolous'
      missing$LL_Publication[missing$Acc_No %in% paul$ITS_GenBank[i]]<-'Paul et al. (2018), dal Grande et al. (2018)'
      missing$Substrate_Man_Cur[missing$Acc_No %in% paul$ITS_GenBank[i]]<-'Yes'
      missing$Substrate_Publication[missing$Acc_No %in% paul$ITS_GenBank[i]]<-'Paul et al. (2018), dal Grande et al. (2018)'
      missing$LL_Man_Cur[missing$Acc_No %in% paul$ITS_GenBank[i]]<-'Yes'
    }else if(paul$Location[i]=='III') {
      missing$Lat[missing$Acc_No %in% paul$ITS_GenBank[i]]<-'40.212'
      missing$Long[missing$Acc_No %in% paul$ITS_GenBank[i]]<-'-5.234' 
      missing$Substrate[missing$Acc_No %in% paul$ITS_GenBank[i]]<-'Saxicolous'
      missing$LL_Publication[missing$Acc_No %in% paul$ITS_GenBank[i]]<-'Paul et al. (2018), dal Grande et al. (2018)'
      missing$Substrate_Man_Cur[missing$Acc_No %in% paul$ITS_GenBank[i]]<-'Yes'
      missing$Substrate_Publication[missing$Acc_No %in% paul$ITS_GenBank[i]]<-'Paul et al. (2018), dal Grande et al. (2018)'
      missing$LL_Man_Cur[missing$Acc_No %in% paul$ITS_GenBank[i]]<-'Yes'
    }else if(paul$Location[i]=='IV') {
      missing$Lat[missing$Acc_No %in% paul$ITS_GenBank[i]]<-'40.218'
      missing$Long[missing$Acc_No %in% paul$ITS_GenBank[i]]<-'-5.233' 
      missing$Substrate[missing$Acc_No %in% paul$ITS_GenBank[i]]<-'Saxicolous'
      missing$LL_Publication[missing$Acc_No %in% paul$ITS_GenBank[i]]<-'Paul et al. (2018), dal Grande et al. (2018)'
      missing$Substrate_Man_Cur[missing$Acc_No %in% paul$ITS_GenBank[i]]<-'Yes'
      missing$Substrate_Publication[missing$Acc_No %in% paul$ITS_GenBank[i]]<-'Paul et al. (2018), dal Grande et al. (2018)'
      missing$LL_Man_Cur[missing$Acc_No %in% paul$ITS_GenBank[i]]<-'Yes'
    }else if(paul$Location[i]=='V') {
      missing$Lat[missing$Acc_No %in% paul$ITS_GenBank[i]]<-'40.225'
      missing$Long[missing$Acc_No %in% paul$ITS_GenBank[i]]<-'-5.238' 
      missing$Substrate[missing$Acc_No %in% paul$ITS_GenBank[i]]<-'Saxicolous'
      missing$LL_Publication[missing$Acc_No %in% paul$ITS_GenBank[i]]<-'Paul et al. (2018), dal Grande et al. (2018)'
      missing$Substrate_Man_Cur[missing$Acc_No %in% paul$ITS_GenBank[i]]<-'Yes'
      missing$Substrate_Publication[missing$Acc_No %in% paul$ITS_GenBank[i]]<-'Paul et al. (2018), dal Grande et al. (2018)'
      missing$LL_Man_Cur[missing$Acc_No %in% paul$ITS_GenBank[i]]<-'Yes'
    }else if(paul$Location[i]=='VI') {
      missing$Lat[missing$Acc_No %in% paul$ITS_GenBank[i]]<-'40.232'
      missing$Long[missing$Acc_No %in% paul$ITS_GenBank[i]]<-'-5.239'  
      missing$Substrate[missing$Acc_No %in% paul$ITS_GenBank[i]]<-'Saxicolous'
      missing$LL_Publication[missing$Acc_No %in% paul$ITS_GenBank[i]]<-'Paul et al. (2018), dal Grande et al. (2018)'
      missing$Substrate_Man_Cur[missing$Acc_No %in% paul$ITS_GenBank[i]]<-'Yes'
      missing$Substrate_Publication[missing$Acc_No %in% paul$ITS_GenBank[i]]<-'Paul et al. (2018), dal Grande et al. (2018)'
      missing$LL_Man_Cur[missing$Acc_No %in% paul$ITS_GenBank[i]]<-'Yes'
    }else if(paul$Location[i]=='VII') {
      missing$Lat[missing$Acc_No %in% paul$ITS_GenBank[i]]<-'40.240'
      missing$Long[missing$Acc_No %in% paul$ITS_GenBank[i]]<-'-5.242'
      missing$Substrate[missing$Acc_No %in% paul$ITS_GenBank[i]]<-'Saxicolous'
      missing$LL_Publication[missing$Acc_No %in% paul$ITS_GenBank[i]]<-'Paul et al. (2018), dal Grande et al. (2018)'
      missing$Substrate_Man_Cur[missing$Acc_No %in% paul$ITS_GenBank[i]]<-'Yes'
      missing$Substrate_Publication[missing$Acc_No %in% paul$ITS_GenBank[i]]<-'Paul et al. (2018), dal Grande et al. (2018)'
      missing$LL_Man_Cur[missing$Acc_No %in% paul$ITS_GenBank[i]]<-'Yes'
    }else if(paul$Location[i]=='VIII') {
      missing$Lat[missing$Acc_No %in% paul$ITS_GenBank[i]]<-'40.245'
      missing$Long[missing$Acc_No %in% paul$ITS_GenBank[i]]<-'-5.242' 
      missing$Substrate[missing$Acc_No %in% paul$ITS_GenBank[i]]<-'Saxicolous'
      missing$LL_Publication[missing$Acc_No %in% paul$ITS_GenBank[i]]<-'Paul et al. (2018), dal Grande et al. (2018)'
      missing$Substrate_Man_Cur[missing$Acc_No %in% paul$ITS_GenBank[i]]<-'Yes'
      missing$Substrate_Publication[missing$Acc_No %in% paul$ITS_GenBank[i]]<-'Paul et al. (2018), dal Grande et al. (2018)'
      missing$LL_Man_Cur[missing$Acc_No %in% paul$ITS_GenBank[i]]<-'Yes'
    }else if(paul$Location[i]=='IX') {
      missing$Lat[missing$Acc_No %in% paul$ITS_GenBank[i]]<-'40.251'
      missing$Long[missing$Acc_No %in% paul$ITS_GenBank[i]]<-'-5.254'  
      missing$Substrate[missing$Acc_No %in% paul$ITS_GenBank[i]]<-'Saxicolous'
      missing$LL_Publication[missing$Acc_No %in% paul$ITS_GenBank[i]]<-'Paul et al. (2018), dal Grande et al. (2018)'
      missing$Substrate_Man_Cur[missing$Acc_No %in% paul$ITS_GenBank[i]]<-'Yes'
      missing$Substrate_Publication[missing$Acc_No %in% paul$ITS_GenBank[i]]<-'Paul et al. (2018), dal Grande et al. (2018)'
      missing$LL_Man_Cur[missing$Acc_No %in% paul$ITS_GenBank[i]]<-'Yes'
    }
  }
}




#Difficult to discern lat/long for dal Grande et al. (2018) but can say saxicolous
missing$Substrate[missing$Acc_No %in% c("MF432447","MF432445","MF432451","MF432448","MF432450","MF432449","MF432446")]<-"Saxicolous"
missing$Substrate_Man_Cur[missing$Acc_No %in% c("MF432447","MF432445","MF432451","MF432448","MF432450","MF432449","MF432446")]<-'Yes'
missing$Substrate_Publication[missing$Acc_No %in% c("MF432447","MF432445","MF432451","MF432448","MF432450","MF432449","MF432446")]<-'dal Grande et al. (2018)'




#Werth & Sork (2014) AJB
#Four seqs submitted, and can trace the location of the actual accession for 3 of them (can't figure out KF556652)
#KF556651 from Site BA11
missing$Lat[missing$Acc_No %in% "KF556651"]<-'27.6423'
missing$Long[missing$Acc_No %in% "KF556651"]<-'-113.4315'  
missing$Substrate[missing$Acc_No %in% "KF556651"]<-'Corticolous'
missing$LL_Publication[missing$Acc_No %in% "KF556651"]<-'Werth & Sork (2014)'
missing$Substrate_Man_Cur[missing$Acc_No %in% "KF556651"]<-'Yes'
missing$Substrate_Publication[missing$Acc_No %in% "KF556651"]<-'Werth & Sork (2014)'
missing$LL_Man_Cur[missing$Acc_No %in% "KF556651"]<-'Yes'

#KF556650 from Site OR2
missing$Lat[missing$Acc_No %in% "KF556650"]<-'44.0882'
missing$Long[missing$Acc_No %in% "KF556650"]<-'-124.1183'  
missing$Substrate[missing$Acc_No %in% "KF556650"]<-'Corticolous'
missing$LL_Publication[missing$Acc_No %in% "KF556650"]<-'Werth & Sork (2014)'
missing$Substrate_Man_Cur[missing$Acc_No %in% "KF556650"]<-'Yes'
missing$Substrate_Publication[missing$Acc_No %in% "KF556650"]<-'Werth & Sork (2014)'
missing$LL_Man_Cur[missing$Acc_No %in% "KF556650"]<-'Yes'

#KF556649 from Site OR2
missing$Lat[missing$Acc_No %in% "KF556649"]<-'44.0882'
missing$Long[missing$Acc_No %in% "KF556649"]<-'-124.1183'  
missing$Substrate[missing$Acc_No %in% "KF556649"]<-'Corticolous'
missing$LL_Publication[missing$Acc_No %in% "KF556649"]<-'Werth & Sork (2014)'
missing$Substrate_Man_Cur[missing$Acc_No %in% "KF556649"]<-'Yes'
missing$Substrate_Publication[missing$Acc_No %in% "KF556649"]<-'Werth & Sork (2014)'
missing$LL_Man_Cur[missing$Acc_No %in% "KF556649"]<-'Yes'



#Werth & Sork (2010) - all are from site discussed in Werth & Sork (2008) as 34°42′N, 120°02′W...I've converted to 34.700000, -120.033333 and are corticolous
ws2010<-c("FJ705175","FJ705176","FJ705177","FJ705178","FJ705179","FJ705180","FJ705181","FJ705182","FJ705183","FJ705184","FJ705185","FJ705186","FJ705187","FJ705188","FJ705189","FJ705190","FJ705191","FJ705192","FJ705193","FJ705194","FJ705195","FJ705196","FJ705197","FJ705198","FJ705199","FJ705200","FJ705201","FJ705202","FJ705203","FJ705204","FJ705205","FJ705206")
missing$Lat[missing$Acc_No %in% ws2010]<-'34.700000'
missing$Long[missing$Acc_No %in% ws2010]<-'-120.033333'  
missing$Substrate[missing$Acc_No %in% ws2010]<-'Corticolous'
missing$LL_Publication[missing$Acc_No %in% ws2010]<-'Werth & Sork (2010)'
missing$Substrate_Man_Cur[missing$Acc_No %in% ws2010]<-'Yes'
missing$Substrate_Publication[missing$Acc_No %in% ws2010]<-'Werth & Sork (2010)'
missing$LL_Man_Cur[missing$Acc_No %in% ws2010]<-'Yes'



#Werth (2012) EU717911- EU717936 sampled corticolous taxa...LL already included in GenBank
w12<-paste("EU7179",11:36,sep="")
missing$Substrate[missing$Acc_No %in% w12]<-'Corticolous'
missing$Substrate_Man_Cur[missing$Acc_No %in% w12]<-'Yes'
missing$Substrate_Publication[missing$Acc_No %in% w12]<-'Werth (2012)'





#Onut-Braennstrom
ob<-read.csv(file="xxxx/onut-braennstroem_et_al_2017_Thamnolia_chemotypes_and_morphotypes_llconverted_accs_added.csv",stringsAsFactors=FALSE)
#get rid of some weird formatting with ¬† 015.2307889 ¬† 099.7118889 
ob$Long<-gsub('  ', '', ob$Long)
ob$Long<-gsub("099.7118889 ","99.7118889",ob$Long)
ob$Long<-gsub("015.2307889 ","15.2307889",ob$Long)
nrow(ob)
ob<-ob[!ob$Acc_No_1 %in% "",]
nrow(ob)
for(x in 1:nrow(ob)){
	missing$Lat[missing$Acc_No %in% ob$Acc_No_1[x]]<-ob$Lat[x]
	missing$Lat[missing$Acc_No %in% ob$Acc_No_2[x]]<-ob$Lat[x]
	missing$Long[missing$Acc_No %in% ob$Acc_No_1[x]]<-ob$Long[x]
	missing$Long[missing$Acc_No %in% ob$Acc_No_2[x]]<-ob$Long[x]
	missing$LL_Publication[missing$Acc_No %in% ob$Acc_No_1[x]]<-"Onut-Brannstrom et al. (2017)"
	missing$LL_Publication[missing$Acc_No %in% ob$Acc_No_2[x]]<-"Onut-Brannstrom et al. (2017)"
	missing$LL_Man_Cur[missing$Acc_No %in% ob$Acc_No_1[x]]<-'Yes'
	missing$LL_Man_Cur[missing$Acc_No %in% ob$Acc_No_2[x]]<-'Yes'
}


#some are listed in NCBI as being Sweden, but their coords are coming back as NOrway
#> missing$Acc_No[missing$Country %in% "Sweden" & missing$closest %in% "NORWAY"]
# [1] "KY559164" "KY559165" "KY559162" "KY559163" "KY559221" "KY559220"
# [7] "KY559210" "KY559211" "KY559212" "KY559213" "KY559132" "KY559141"
#[13] "KY559142" "KY559209" "KY559125" "KY559231" "KY559235"

#these are listed as being from Sweden, but return coords consistent with Norway, some are legit, some are not.
#KY559162, KY559163, KY559164, KY559165, KY559125
#66.2898667 14.1398833	
#listed as Sweden_MoiRana in Onut-Braennstroem map, but coords are definitely Norway, and Mo i Rana is right by coords in Norway.
#Change country to Norway
accs<-c("KY559162", "KY559163", "KY559164", "KY559165", "KY559125")
for(x in 1:length(accs)){
	missing$Country_Detailed[missing$Acc_No %in% accs[x]]<-"Norway"
	missing$Country[missing$Acc_No %in% accs[x]]<-"Norway"
	missing$Geo_Man_Cur[missing$Acc_No %in% accs[x]]<-"Yes"
	missing$DB_Notes[missing$Acc_No %in% accs[x]]<-"Listed as from Sweden in NCBI, but from Norway"
	missing$ISO3Code[missing$Acc_No %in% accs[x]]<-"NOR"
	missing$GEO3SubRegion[missing$Acc_No %in% accs[x]]<-"Western Europe"
	missing$GEO3MajorRegion[missing$Acc_No %in% accs[x]]<-"Europe"
}

#KY559220, KY559221
#68.3601167	18.72
#Coords are Sweden, but close to border w Norway - leave as Sweden

#KY559210,KY559211,KY559212,KY559132,KY559209,KY559231
#69.99602 29.47961, 69.9514889 29.2200583, 69.99602 29.47961, 70.2339222 29.8948667, 69.99602 29.47961, 70.172996 28.55444
#coords are all for Finmark area of Norway, and are listed on page as being from Sweden on NCBI, and map and internal naming has them as being from Norway - change to Norway
accs<-c("KY559210","KY559211","KY559212","KY559132","KY559209","KY559231")
for(x in 1:length(accs)){
	missing$Country_Detailed[missing$Acc_No %in% accs[x]]<-"Norway"
	missing$Country[missing$Acc_No %in% accs[x]]<-"Norway"
	missing$Geo_Man_Cur[missing$Acc_No %in% accs[x]]<-"Yes"
	missing$DB_Notes[missing$Acc_No %in% accs[x]]<-"Listed as from Sweden in NCBI, but from Norway"
	missing$ISO3Code[missing$Acc_No %in% accs[x]]<-"NOR"
	missing$GEO3SubRegion[missing$Acc_No %in% accs[x]]<-"Western Europe"
	missing$GEO3MajorRegion[missing$Acc_No %in% accs[x]]<-"Europe"
}

#KY559213, KY559235
#63	12
#coords are just on Norwegian side of Sweden/Norway border - leave as Sweden, as name is from region of Sweden, too.

#KY559141, KY559142
#68.3601167	18.72
#Coords are Sweden, but near but near Norwegian border - leave as is.





#add in Nelsen & Gargas 2009 Trebouxia (Thamnolia)
nelgar<-read.csv(file="xxxx/nelsen_gargas_2009b_thamnolia_lat_long/nelsen_gargas_2009_trebouxia.csv",stringsAsFactors=FALSE)
for(x in 1:nrow(nelgar)){
	missing$Lat[missing$Acc_No %in% nelgar$Algae[x]]<-nelgar$Lat[x]
	missing$Long[missing$Acc_No %in% nelgar$Algae[x]]<-nelgar$Long[x]
	missing$Substrate[missing$Acc_No %in% nelgar$Algae[x]]<-"Ground"
	missing$LL_Publication[missing$Acc_No %in% nelgar$Algae[x]]<-"Nelsen & Gargas (2009)"
	missing$LL_Man_Cur[missing$Acc_No %in% nelgar$Algae[x]]<-'Yes'
	missing$Substrate_Man_Cur[missing$Acc_No %in% nelgar$Algae[x]]<-'Yes'
	missing$Substrate_Publication[missing$Acc_No %in% nelgar$Algae[x]]<-"Nelsen & Gargas (2009)"
}


#Add in Alvarez et al. (2015) - all isoloated from Quercus rotundifolia at El Escorial (Madrid, Spain) 40 34' 54.3N 4 07' 35.1"W OR 40.5817500 -004.1264167
alvacs<-c("KJ413046", "KJ413047", "KJ413048", "KJ413049", "KJ413050", "KJ413051")
missing$Lat[missing$Acc_No %in% alvacs]<-"40.5817500"
missing$Long[missing$Acc_No %in% alvacs]<-"-004.1264167"
missing$Substrate[missing$Acc_No %in% alvacs]<-"Bark"
missing$LL_Publication[missing$Acc_No %in% alvacs]<-"Alvarez et al. (2015)"
missing$LL_Man_Cur[missing$Acc_No %in% alvacs]<-'Yes'
missing$Substrate_Man_Cur[missing$Acc_No %in% alvacs]<-'Yes'
missing$Substrate_Publication[missing$Acc_No %in% alvacs]<-"Alvarez et al. (2015)"





#Add in Backor et al. (2010) - all collected from soil and rocks
#accs<-c("FM945343","FM945345","FM945344","FM945346")
#missing$Substrate[missing$Acc_No %in% accs]<-"Ground"
#already in there from GenBank


#Tibell & Beck (2002)
accs<-c("AF453259","AF453260","AF453261","AF453262","AF453263","AF453264","AF453265")
sub<-c("wood","bark","bark","bark","bark","bark","bark")

#Lat Long from UPS Herbarium Website
lat<-c("63.31667","63.25","63.25","63.21667","63.25","63.21667","63.21667")
long<-c("14.08333","12.51667","12.51667","12.51667","12.45","12.51667","12.51667")

for(x in 1:length(accs)){
	missing$Substrate[missing$Acc_No %in% accs[x]]<-sub[x]
	missing$Lat[missing$Acc_No %in% accs[x]]<-lat[x]
	missing$Long[missing$Acc_No %in% accs[x]]<-long[x]
	missing$LL_Publication[missing$Acc_No %in% accs[x]]<-"Tibell & Beck (2002), UPS Herbarium"
	missing$LL_Man_Cur[missing$Acc_No %in% accs[x]]<-'Yes'
	missing$Substrate_Man_Cur[missing$Acc_No %in% accs[x]]<-'Yes'
	missing$Substrate_Publication[missing$Acc_No %in% accs[x]]<-"Tibell & Beck (2002)"
}



#Beck et al. (2002) from rock, and could georeference locations "France, Alpes Maritimes, 4 km northeast of Gourdon, on a 45° inclined, west-exposed boulder, which is elevated about 30 cm above the ground."
accs<-c("AF344175","AF344176","AF344177")
sub<-c("rock","rock","rock")
for(x in 1:length(accs)){
	missing$Substrate[missing$Acc_No %in% accs[x]]<-sub[x]
	missing$Substrate_Man_Cur[missing$Acc_No %in% accs[x]]<-'Yes'
	missing$Substrate_Publication[missing$Acc_No %in% accs[x]]<-"Beck et al. (2002)"
}



#Beck et al. (2012)
accs<-c("JF831903","JF831904","JF831923","JF831914","JF831921","JF831922","JF831919","JF831920","JF831905","JF831915","JF831917","JF831906","JF831912","JF831913","JF831916","JF831909","JF831908","JF831907","JF831910","JF831911","JF831918","JF831884","JF831885","JF831886","JF831887","JF831888","JF831889","JF831890","JF831891","JF831892","JF831893","JF831894","JF831895","JF831896","JF831897","JF831899","JF831900","JF831901","JF831902","JF831898")
#sub<-c("NA","NA","Bark","Bark","Bark","Bark","Stone","Stone","Stone","Stone","Bark","Bark","Stone","Bark","Stone","Bark","Stone","Bark","Bark","Stone","Bark","Stone","Bark","Bark","Stone","Bark","Bark","Stone","Stone","Bark","Stone","Stone","Bark","Bark","Bark","Stone","Stone","Bark","Bark","Bark")
lat<-c("NA","NA","47.9666667","NA","47.9166667","47.9166667","47.9166667","47.9166667","47.9166667","47.9166667","47.9166667","47.9166667","47.7000000","47.7000000","48.0500000","NA","47.9166667","47.9166667","47.5333333","47.5333333","48.0500000","47.9166667","47.9166667","47.9166667","47.9166667","NA","47.5333333","47.5333333","47.7000000","47.7000000","47.9166667","48.0500000","47.9166667","48.0500000","47.9666667","47.9166667","47.9166667","47.9166667","47.9166667","NA")
long<-c("NA","NA","011.2833333","NA","011.1833333","011.1833333","011.1833333","011.1833333","011.1833333","011.1833333","011.1833333","011.1833333","011.9333333","011.9333333","011.4000000","NA","011.1833333","011.1833333","011.1333333","011.1333333","011.4000000","011.1833333","011.1833333","011.1833333","011.1833333","NA","011.1333333","011.1333333","011.9333333","011.9333333","011.1833333","011.4000000","011.1833333","011.4000000","011.2833333","011.1833333","011.1833333","011.1833333","011.1833333","NA")
#lat<-c("NA","NA","Maising","Munchen","Pahl","Pahl","Pahl","Pahl","Pahl","Pahl","Pahl","Pahl","Fischbachau","Fischbachau","Gauting","Munchen","Pahl","Pahl","Farchant","Farchant","Gauting","Pahl","Pahl","Pahl","Pahl","Munchen","Farchant","Farchant","Fischbachau","Fischbachau","Pahl","Gauting","Pahl","Gauting","Maising","Pahl","Pahl","Pahl","Pahl","Munchen")
#long<-c("NA","NA","Maising","Munchen","Pahl","Pahl","Pahl","Pahl","Pahl","Pahl","Pahl","Pahl","Fischbachau","Fischbachau","Gauting","Munchen","Pahl","Pahl","Farchant","Farchant","Gauting","Pahl","Pahl","Pahl","Pahl","Munchen","Farchant","Farchant","Fischbachau","Fischbachau","Pahl","Gauting","Pahl","Gauting","Maising","Pahl","Pahl","Pahl","Pahl","Munchen")
#Maising 11°17'E,47°58'N; 47.9666667, 011.2833333
#Munchen
#Pahl 11°11'E,47°55'N; 47.9166667, 011.1833333
#Fischbachau 11°56'E,47°42'N; 47.7000000, 011.9333333
#Gauting 11°24'E,48°03'N; 48.0500000, 011.4000000
#Farchant 11°08'E,47°32'N; 47.5333333, 011.1333333
for(x in 1:length(accs)){
	#missing$Substrate[missing$Acc_No %in% accs[x]]<-sub[x]
	if(!lat[x] %in% "NA"){
		missing$Lat[missing$Acc_No %in% accs[x]]<-lat[x]
		missing$Long[missing$Acc_No %in% accs[x]]<-long[x]	
		missing$LL_Publication[missing$Acc_No %in% accs[x]]<-"Beck et al. (2012)"
		missing$LL_Man_Cur[missing$Acc_No %in% accs[x]]<-'Yes'
	}
}


#Beck 1999 saxicolous and could georeference "All algal strains examined were isolated from lichen thalli derived from a single rock, containing iron sulphate, at the 'Schwarze Wand' near Hiittschlag, Salzburger Land, Austria (1700 m alt.)."
#lat long derived from Beck's (2002) dissertation (p.20) 47 10'N 13 11'E = 47.1666667 013.1833333
accs<-c("AF128270","AF128271")
sub<-c("rock","rock")
for(x in 1:length(accs)){
	missing$Substrate[missing$Acc_No %in% accs[x]]<-sub[x]
	missing$Lat[missing$Acc_No %in% accs[x]]<-"47.1666667"
	missing$Long[missing$Acc_No %in% accs[x]]<-"013.1833333"	
	missing$LL_Publication[missing$Acc_No %in% accs[x]]<-"Beck (1999), Beck (2002)"
	missing$LL_Man_Cur[missing$Acc_No %in% accs[x]]<-'Yes'
	missing$Substrate_Man_Cur[missing$Acc_No %in% accs[x]]<-'Yes'
	missing$Substrate_Publication[missing$Acc_No %in% accs[x]]<-"Beck (1999), Beck (2002)"
}


#Beck et al. (1998): "All algal species were taken from lichen thalli derived from sixteen test squares of the Physcietum adscendentis (Ochsner, 1928 ; Hofmann, 1993) at four sites along the river Isar north-east of Mu6 nchen, Germany." on bark (they mention quadrats were on phorophytes)
#these are the accs listed in paper, but don't match with NCBI
#accs<-c("AZ007387","AZ007385","AZ007384","AZ007386","AZ007383","AZ007388")
#used those derived from NCBI instead, and the cultures listed in NCBI
accs<-c("AJ007385","AJ007388","AJ007386","AJ007383","AJ007384","AJ007387")
sub<-c("bark","bark","bark","bark","bark","bark")
for(x in 1:length(accs)){
	missing$Substrate[missing$Acc_No %in% accs[x]]<-sub[x]
	missing$Substrate_Man_Cur[missing$Acc_No %in% accs[x]]<-'Yes'
	missing$Substrate_Publication[missing$Acc_No %in% accs[x]]<-"Beck et al. (1998)"
	#missing$LL_Publication[missing$Acc_No %in% accs[x]]<-"Beck et al. (1998)"
}

#Beiggi & Piercey-Normore is an Asterochloris
#Substrate and Lat/Long lacking from Bhattacharya et al. 1996

#Blaha et al. note that "The diversity and phylogenetic position of photobionts in the widespread saxicolous, crustose lichen-forming asco- mycete Lecanora rupicola s.l. is presented."
#Blaha et al. note that "Lecanora rupicola and closely related species are well circumscribed by their saxicolous growth on siliceous and intermediary rocks and by the presence of the unique chromone sordidone (Leuckert & Poelt, 1989)."
#Interesting: "Recent studies elucidated some ecological prefer- ences of trebouxioid photobionts. It is known that T. simplex is common in lichens growing on acid and heavy metal rich substrates (Beck, 2002), whereas lichen species of Physciaceae from calcareous rocks appear to be restricted to clade A (Helms, 2003: par- ticularly subclades A5–A11). In the present study, we found representatives of this clade in many samples. Lecanora rupicola is generally not known from calcar- eous rocks, but may readily occur on acid to subneu- tral to base-rich rocks, which may host lichens containing diverse lineages of clade A. The absence of T. simplex from lowland Mediterranean regions (i.e. in samples collected at altitudes lower than c. 1000 m) could be due to unfavourable climatic conditions on the sun-exposed rock surfaces in European- Mediterranean habitats. It is possible that tempera- tures are too high at these sites for the species. Studies of cultured T. simplex demonstrated that the optimal conditions for reproduction by zoo- and apl- anospores are at comparatively low temperatures, c. −2 to 10 °C (Tschermak-Woess, 1988, 1989). In more temperate climates, this photobiont species appears less selective for the substrate and can also occur in corticolous parmelioid lichens from Switzerland or Germany (on Abies, Acer, Betula, Juglans, and Quer- cus; Friedl, 1989)."
#all are from L. rupicola group, listed as saxicolous...
accs<-c("DQ166580","DQ166576","DQ166581","DQ166579","DQ166577","DQ166583","DQ166582","DQ166586","DQ166584","DQ166585","DQ166611","DQ166599","DQ166607","DQ166589","DQ166591","DQ166592","DQ166609","DQ166587","DQ166610","DQ166615","DQ166613","DQ166612","DQ166614","DQ166618","DQ166608","DQ166596","DQ166588","DQ166590","DQ166603","DQ166601","DQ166597","DQ166595","DQ166604","DQ166602","DQ166600","DQ166598","DQ166594","DQ166616","DQ166620","DQ166619","DQ166617","DQ166593","DQ166578","DQ166606","DQ166605")
for(x in 1:length(accs)){
	missing$Substrate[missing$Acc_No %in% accs[x]]<-"rock"
	missing$Substrate_Man_Cur[missing$Acc_No %in% accs[x]]<-'Yes'
	missing$Substrate_Publication[missing$Acc_No %in% accs[x]]<-"Blaha et al. (2006)"
}

#Brunauer et al. 2007...
#Blaha et al. (2006) mention these two as growing on rock "This clade is also present in thalli of Lecidella and Protoparmelia (data not shown), which grew adjacent to L. rupicola on the same piece of rock."
#rupicola notes as saxicolous
accs<-c("DQ660907","DQ660908")
for(x in 1:length(accs)){
	missing$Substrate[missing$Acc_No %in% accs[x]]<-"rock"
	missing$Substrate_Man_Cur[missing$Acc_No %in% accs[x]]<-'Yes'
	missing$Substrate_Publication[missing$Acc_No %in% accs[x]]<-"Brunauer et al. (2007)"
}

#EF432561 in Brunauer unpublished already has LL and substrate (gneiss)


#Cao et al. 2015
accs<-c("KP954302","KP954305","KP954311","KP954312","KP954310","KP954303","KP954304","KP954306","KP954307","KP954309","KP954308")
sub<-c("soil","rock","rock","rock","rock","rock","rock","rock","rock","moss","rock")
lat<-c("-62.2161306","-62.2160306","-62.2159194","-62.2117056","-62.2176583","-62.2176611","-62.2212083","-62.2117000","-62.2113000","-62.2261667","-62.2295833")
long<-c("-058.9626694","-058.9626833","-058.9625111","-058.9272833","-058.9659250","-058.9659167","-058.9533250","-058.9272917","-059.0078472","-058.9540417","-058.9590250")
for(x in 1:length(accs)){
	missing$Substrate[missing$Acc_No %in% accs[x]]<-sub[x]
	missing$Lat[missing$Acc_No %in% accs[x]]<-lat[x]
	missing$Long[missing$Acc_No %in% accs[x]]<-long[x]	
	missing$LL_Publication[missing$Acc_No %in% accs[x]]<-"Cao et al. (2015)"
	missing$LL_Man_Cur[missing$Acc_No %in% accs[x]]<-'Yes'
	missing$Substrate_Man_Cur[missing$Acc_No %in% accs[x]]<-'Yes'
	missing$Substrate_Publication[missing$Acc_No %in% accs[x]]<-"Cao et al. (2015)"
}


#Cao et al. 2018
accs<-c("KX147254","KX147255","KX147256","KX147257","KX147258","KX147259","KX147260","KX147261","KX147262","KX147263","KX147264","KX147265","KX147266","KX147267","KX147268","KX147269")
sub<-c("Moss","Stone","Stone","Stone","Stone","Stone","Stone","Stone","Stone","Stone","Stone","Moss","Stone","Stone","Stone","Stone")
lat<-c("-62.2116750","-62.2109417","-62.2116750","-62.2116750","-62.2113000","-62.2113000","-62.2113000","-62.2113000","-62.2654639","-62.2654639","-62.2715028","-62.2715028","-62.1749167","-62.1749167","-62.2463333","-62.2463333")
long<-c("-058.9283528","-058.9841944","-058.9283528","-058.9283528","-059.0078472","-059.0078472","-059.0078472","-059.0078472","-058.8767472","-058.8767472","-058.8742611","-058.8742611","-058.9719361","-058.9719361","-059.0096250","-059.0096250")
for(x in 1:length(accs)){
	missing$Substrate[missing$Acc_No %in% accs[x]]<-sub[x]
	missing$Lat[missing$Acc_No %in% accs[x]]<-lat[x]
	missing$Long[missing$Acc_No %in% accs[x]]<-long[x]	
	missing$LL_Publication[missing$Acc_No %in% accs[x]]<-"Cao et al. (2018)"
	missing$Country_Detailed[missing$Acc_No %in% accs[x]]<-"Antarctica"
	missing$Country[missing$Acc_No %in% accs[x]]<-"Antarctica"
	missing$Geo_Man_Cur[missing$Acc_No %in% accs[x]]<-"Yes"
	missing$DB_Notes[missing$Acc_No %in% accs[x]]<-""
	missing$ISO3Code[missing$Acc_No %in% accs[x]]<-"ATA"
	missing$GEO3SubRegion[missing$Acc_No %in% accs[x]]<-"Antarctic"
	missing$GEO3MajorRegion[missing$Acc_No %in% accs[x]]<-"Polar"
	missing$LL_Man_Cur[missing$Acc_No %in% accs[x]]<-'Yes'
	missing$Substrate_Man_Cur[missing$Acc_No %in% accs[x]]<-'Yes'
	missing$Substrate_Publication[missing$Acc_No %in% accs[x]]<-"Cao et al. (2018)"
}



#Casano et al. (2011)
#Ve is AS-228 site in Asturias with 3 samples 43.0758333 -006.0316667
#Me is AS-311 site in Asturias with 5 samples 43.2171472 -006.1332389
#Ca is Castellon 39.9589833 -000.7765306
#Cr is Ciudad Real 38.6780250 -002.6545778
#Le is Leon 42.7587694 -005.1485250
#Re is Realejos Tenerife 28.3475000 -016.5908333
#Gu is Pinar de la Guancha Tenerife 28.3466667 -016.6619444
#MH is Mount Hamilton 37.3230556 -121.6691667
#SF is SFBG 37.7666667 -122.4688889

accs<-c("GU252203","GU252195","GU252193","GU252204","GU252202","GU252196","GU252194","GU252192","GU252175","GU252173","GU252171","GU252176","GU252174","GU252172","GU252181","GU252179","GU252177","GU252180","GU252178","GU252191","GU252189","GU252187","GU252190","GU252188","GU252201","GU252199","GU252197","GU252185","GU252183","GU252200","GU252198","GU252186","GU252184","GU252182","GU252205","GU252206")
sub<-"bark"
lat<-c("43.0758333","43.2171472","43.2171472","43.0758333","43.0758333","43.2171472","43.2171472","43.2171472","39.9589833","39.9589833","39.9589833","39.9589833","39.9589833","39.9589833","38.6780250","38.6780250","38.6780250","38.6780250","38.6780250","42.7587694","42.7587694","42.7587694","42.7587694","42.7587694","28.3475000","28.3475000","28.3475000","28.3466667","28.3466667","28.3475000","28.3475000","28.3466667","28.3466667","28.3466667","37.3230556","37.7666667")
long<-c("-006.0316667","-006.1332389","-006.1332389","-006.0316667","-006.0316667","-006.1332389","-006.1332389","-006.1332389","-000.7765306","-000.7765306","-000.7765306","-000.7765306","-000.7765306","-000.7765306","-002.6545778","-002.6545778","-002.6545778","-002.6545778","-002.6545778","-005.1485250","-005.1485250","-005.1485250","-005.1485250","-005.1485250","-016.5908333","-016.5908333","-016.5908333","-016.6619444","-016.6619444","-016.5908333","-016.5908333","-016.6619444","-016.6619444","-016.6619444","-121.6691667","-122.4688889")

for(x in 1:length(accs)){
	missing$Substrate[missing$Acc_No %in% accs[x]]<-"Bark"
	missing$Lat[missing$Acc_No %in% accs[x]]<-lat[x]
	missing$Long[missing$Acc_No %in% accs[x]]<-long[x]	
	missing$LL_Publication[missing$Acc_No %in% accs[x]]<-"Casano et al. (2011)"
	missing$LL_Man_Cur[missing$Acc_No %in% accs[x]]<-'Yes'
	missing$Substrate_Man_Cur[missing$Acc_No %in% accs[x]]<-'Yes'
	missing$Substrate_Publication[missing$Acc_No %in% accs[x]]<-"Casano et al. (2011)"
}


#Cestaro et al. (2016)
#Could likely georeference those w missing lat long
accs<-c("KX181243","KX181244","KX181245","KX181246","KX181247","KX181248","KX181249","KX181250","KX181251","KX181252","KX181253","KX181254","KX181255","KX181256","KX181257","KX181258","KX181259","KX181260","KX181261","KX181284","KX181285","KX181286","KX181287","KX181262","KX181263","KX181264","KX181265","KX181266","KX181267","KX181268","KX181269","KX181270","KX181271","KX181272","KX181273","KX181274","KX181275","KX181276","KX181277","KX181278","KX181279","KX181280","KX181281","KX181282","KX181283")
sub<-c("bark","bark","bark","bark","bark","bark","bark","bark","bark","bark","bark","bark","bark","bark","bark","bark","bark","bark","bark","bark","bark","bark","bark","bark","bark","bark","bark","bark","bark","bark","bark","bark","rock","rock","rock","rock","bark","bark","rock","rock","rock","rock","rock","rock","rock")
lat<-c("39.8980556","39.8980556","39.8980556","39.8980556","39.8980556","39.8980556","NA","NA","NA","NA","40.0472222","40.0472222","40.0472222","40.0472222","40.0472222","40.0361111","40.0361111","40.0361111","40.0361111","46.2980556","46.2050000","46.2388889","46.2388889","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA")
long<-c("020.6297222","020.6297222","020.6297222","020.6297222","020.6297222","020.6297222","NA","NA","NA","NA","020.6702778","020.6702778","020.6702778","020.6702778","020.6702778","020.6063889","020.6063889","020.6063889","020.6063889","-122.4786111","-122.5038889","-122.4525000","-122.4525000","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA","NA")
for(x in 1:length(accs)){
	missing$Substrate[missing$Acc_No %in% accs[x]]<-sub[x]
	missing$Lat[missing$Acc_No %in% accs[x]]<-lat[x]
	missing$Long[missing$Acc_No %in% accs[x]]<-long[x]	
	missing$Substrate_Man_Cur[missing$Acc_No %in% accs[x]]<-'Yes'
	missing$Substrate_Publication[missing$Acc_No %in% accs[x]]<-"Cestaro et al. (2016)"
	if(!lat[x] %in% "NA"){
		missing$LL_Publication[missing$Acc_No %in% accs[x]]<-"Cestaro et al. (2016)"
		missing$LL_Man_Cur[missing$Acc_No %in% accs[x]]<-'Yes'
	}
}



#Cordeiro et al. (2005)
#Cladina confusa
#AY842279
#25°32'05"S/48°20'30"W
#-25.5347222 -048.3416667

#Cladonia crinita
#AY842277
#20°05'S/43°29'W
#-20.0833333 -043.4833333

#Cladonia fissidens
#AY842278
#20°05'S/43°29'W
#-20.0833333 -043.4833333

#Cladonia perforata
#AY842280

#Cladonia verticillaris
#AY842276
#7°26'S/34°56'W
#-07.4333333 -034.9333333

#Ramalina anceps (70)
#AY842269
#25°19'52"S/48°25'10"W
#-25.3311111 -048.4194444

#Ramalina anceps (78)
#AY842268
#25°19'31"S/48°25'49"W
#-25.3252778 -048.4302778

#Ramalina complanata
#AY842262
#27°35'48"S/48°32'57"W
#-27.5966667 -048.5491667

#Ramalina dendroides (79)
#AY842267
#25°19'04"S/48°26'41"W
#-25.3177778 -048.4447222

#Ramalina farinacea (FB22)
#AY842264

#Ramalina fraxineae (FB30)
#AY842265

#Ramalina gracilis (63)
#AY842273
#25°32'05"S/48°20'30"W
#-25.5347222 -048.3416667

#Ramalina gracilis
#AY842263
#27°35'48"S/48°32'57"W
#-27.5966667 -048.5491667

#Ramalina peruviana (58)
#AY842270
#25°26'15"S/49°03'45"W
#-25.4375000 -049.0625000

#Ramalina peruviana (67)
#AY842266
#25°32'05"S/48°20'30"W
#-25.5347222 -048.3416667

#Ramalina peruviana (81)
#AY842275
#25°14'36"S/48°29'38"W
#-25.2433333 -048.4938889

#Ramalina sorediosa (59)
#AY842272
#25°19'52"S/48°25'10"W
#-25.3311111 -048.4194444

#Ramalina sorediosa (60)
#AY842271
#25°19'51"S/48°27'40"W
#-25.3308333 -048.4611111

#Ramalina sprengelii (68)
#AY842274
#25°32'05"S/48°20'30"W
#-25.5347222 -048.3416667

accs<-c("AY842279","AY842278","AY842278","AY842276","AY842269","AY842268","AY842262","AY842267","AY842273","AY842263","AY842270","AY842266","AY842275","AY842272","AY842271","AY842274")
lat<-c("-25.5347222","-20.0833333","-20.0833333","-07.4333333","-25.3311111","-25.3252778","-27.5966667","-25.3177778","-25.5347222","-27.5966667","-25.4375000","-25.5347222","-25.2433333","-25.3311111","-25.3308333","-25.5347222")
long<-c("-048.3416667","-043.4833333","-043.4833333","-034.9333333","-048.4194444","-048.4302778","-048.5491667","-048.4447222","-048.5491667","-048.5491667","-049.0625000","-048.3416667","-048.4938889","-048.4194444","-048.4611111","-048.3416667")

for(x in 1:length(accs)){
	missing$Lat[missing$Acc_No %in% accs[x]]<-lat[x]
	missing$Long[missing$Acc_No %in% accs[x]]<-long[x]	
	missing$LL_Publication[missing$Acc_No %in% accs[x]]<-"Cordeiro et al. (2005)"
	missing$LL_Man_Cur[missing$Acc_No %in% accs[x]]<-'Yes'
}




#Dahlkild et al. (2001)
#UPS specimens have coords
#Moberg 12018 Phaeophyscia orbicularis
#AF389928
#59.85 17.63333

#Nordin 3847 Physcia caesia
#AF389920
#no lat long given

#Mattson 3341 Physcia caesia
#AF389919
#cannot find

#Moberg 12019 Physcia caesia
#AF389918
#59.85 17.63333

#Moberg 12036 Physconia distorta
#AF389923
#57.43333 18.85

accs<-c("AF389916","AF389914","AF389913","AF389915","AF389917","AF389929","AF389931","AF389932","AF389928","AF389930","AF389922","AF389921","AF389920","AF389919","AF389918","AF389935","AF389937","AF389934","AF389933","AF389936","AF389924","AF389923","AF389926","AF389927","AF389925")
sub<-c("bark","bark","bark","bark","bark","bark","bark","bark","bark","bark","rock","rock","rock","rock","rock","bark","bark","bark","bark","bark","bark","bark","bark","bark","bark")
for(x in 1:length(accs)){
	missing$Substrate[missing$Acc_No %in% accs[x]]<-sub[x]
	missing$Substrate_Man_Cur[missing$Acc_No %in% accs[x]]<-'Yes'
	missing$Substrate_Publication[missing$Acc_No %in% accs[x]]<-"Dahlkild et al. (2001)"
}

accs<-c("AF389928","AF389918","AF389923")
lat<-c("59.85","59.85","57.43333")
long<-c("17.63333","17.63333","18.85")
for(x in 1:length(accs)){
	missing$Lat[missing$Acc_No %in% accs[x]]<-lat[x]
	missing$Long[missing$Acc_No %in% accs[x]]<-long[x]	
	missing$LL_Publication[missing$Acc_No %in% accs[x]]<-"Dahlkild et al. (2001)"
	missing$LL_Man_Cur[missing$Acc_No %in% accs[x]]<-'Yes'
}




#dal Grande et al. (2014)
accs<-c("KJ027684","KJ027683","KJ027682","KJ027685","KJ027687","KJ027686","KJ027698","KJ027699","KJ027688","KJ027700","KJ027693","KJ027694","KJ027690","KJ027697","KJ027695","KJ027691","KJ027689","KJ027696","KJ027701","KJ027692")
lat<-c("45.396","45.396","45.391","50.153","50.153","50.153","50.153","50.155","50.155","50.158","59.411","59.411","59.410","59.410","59.410","41.141","41.141","41.143","41.143","41.141")
long<-c("11.402","11.402","11.396","8.765","8.765","8.765","8.769","8.773","8.773","8.773","11.368","11.368","11.370","11.368","11.369","-3.306","-3.306","-3.304","-3.304","-3.302")
for(x in 1:length(accs)){
	missing$Lat[missing$Acc_No %in% accs[x]]<-lat[x]
	missing$Long[missing$Acc_No %in% accs[x]]<-long[x]	
	missing$LL_Publication[missing$Acc_No %in% accs[x]]<-"dal Grande et al. (2014)"
	missing$LL_Man_Cur[missing$Acc_No %in% accs[x]]<-'Yes'
}


#de los Rios et al (2002)
accs<-c("AF534385","AF534386","AF534387")
sub<-c("rock","rock","rock")
for(x in 1:length(accs)){
	missing$Substrate[missing$Acc_No %in% accs[x]]<-sub[x]
	missing$Substrate_Man_Cur[missing$Acc_No %in% accs[x]]<-'Yes'
	missing$Substrate_Publication[missing$Acc_No %in% accs[x]]<-"de los Rios et al. (2002)"
}


#de los Rios et al (2005)
#already included in NCBI
#accs<-"AY667580"
#sub<-"granite"
#for(x in 1:length(accs)){
#	missing$Substrate[missing$Acc_No %in% accs[x]]<-sub[x]
#}



#could not find de Nova paper




#de Oliveira et al. (2012)
accs<-c("JQ085336","JQ085335","JQ085327","JQ085338","JQ085324","JQ085325","JQ085326","JQ085337","JQ085329","JQ085330","JQ085332","JQ085331","JQ085333","JQ085334","JQ085339","JQ085340","JQ085341","JQ085342","JQ085343","JQ085328","JQ085345","JQ085344")
sub<-c("bark","bark","bark","bark","bark","bark","bark","bark","bark","bark","bark","bark","bark","bark","bark","bark","bark","bark","bark","bark","bark","bark")
lat<-c("49.2891667","49.2891667","49.2724167","49.2724167","49.5460833","49.6608611","49.6608611","49.6608611","49.6608611","49.6608611","49.6608611","49.6608611","50.1455000","50.1455000","50.1455000","50.1455000","50.1455000","50.1455000","50.1455000","50.1455000","50.1455000","50.1455000")
long<-c("-095.7561111","-095.7561111","-095.2115556","-095.2115556","-095.5751389","-099.2684722","-099.2684722","-099.2684722","-099.2684722","-099.2684722","-099.2684722","-099.2684722","-095.8067500","-095.8067500","-095.8067500","-095.8067500","-095.8067500","-095.8067500","-095.8067500","-095.8067500","-095.8067500","-095.8067500")
for(x in 1:length(accs)){
	missing$Substrate[missing$Acc_No %in% accs[x]]<-sub[x]
	missing$Lat[missing$Acc_No %in% accs[x]]<-lat[x]
	missing$Long[missing$Acc_No %in% accs[x]]<-long[x]	
	missing$LL_Publication[missing$Acc_No %in% accs[x]]<-"de Oliveira et al. (2012)"
	missing$LL_Man_Cur[missing$Acc_No %in% accs[x]]<-'Yes'
	missing$Substrate_Man_Cur[missing$Acc_No %in% accs[x]]<-'Yes'
	missing$Substrate_Publication[missing$Acc_No %in% accs[x]]<-"de Oliveira et al. (2012)"
}


accs<-c("JQ085339","JQ085340","JQ085341","JQ085338","JQ085344","JQ085345","JQ085342","JQ085343")
for(x in 1:length(accs)){
	missing$Country_Detailed[missing$Acc_No %in% accs[x]]<-"Canada"
	missing$Country[missing$Acc_No %in% accs[x]]<-"Canada"
	missing$Geo_Man_Cur[missing$Acc_No %in% accs[x]]<-"Yes"
	missing$DB_Notes[missing$Acc_No %in% accs[x]]<-""
	missing$ISO3Code[missing$Acc_No %in% accs[x]]<-"CAN"
	missing$GEO3SubRegion[missing$Acc_No %in% accs[x]]<-"Canada"
	missing$GEO3MajorRegion[missing$Acc_No %in% accs[x]]<-"North America"
}

#no ecological info really provided in del Campo et al. (2010)



#del Campo et al. (2012)
#coords and ecology from Casano et al. (2011)
#Casano et al. (2011)
#Ve is AS-228 site in Asturias with 3 samples 43.0758333 -006.0316667
#Me is AS-311 site in Asturias with 5 samples 43.2171472 -006.1332389
#Ca is Castellon 39.9589833 -000.7765306
#Cr is Ciudad Real 38.6780250 -002.6545778
#Le is Leon 42.7587694 -005.1485250
#Re is Realejos Tenerife 28.3475000 -016.5908333
#Gu is Pinar de la Guancha Tenerife 28.3466667 -016.6619444
#MH is Mount Hamilton 37.3230556 -121.6691667
#SF is SFBG 37.7666667 -122.4688889

accs<-c("JF414630","JF414631","JF414632","JF414633","JF414634","JF414651","JF414652","JF414653","JF414654","JF414655","JF414641","JF414642","JF414643","JF414644","JF414645","JF414637","JF414638","JF414639","JF414640","JF414646","JF414647","JF414648","JF414649","JF414650","JF414656","JF414657","JF414658","JF414659","JF414660","JF414635","JF414636")
lat<-c("43.2171472","43.2171472","43.2171472","43.2171472","43.2171472","42.7587694","42.7587694","42.7587694","42.7587694","42.7587694","39.9589833","39.9589833","39.9589833","39.9589833","39.9589833","38.6780250","38.6780250","38.6780250","38.6780250","28.3466667","28.3466667","28.3466667","28.3466667","28.3466667","28.3475000","28.3475000","28.3475000","28.3475000","28.3475000","37.3230556","37.7666667")
long<-c("-006.1332389","-006.1332389","-006.1332389","-006.1332389","-006.1332389","-005.1485250","-005.1485250","-005.1485250","-005.1485250","-005.1485250","-000.7765306","-000.7765306","-000.7765306","-000.7765306","-000.7765306","-002.6545778","-002.6545778","-002.6545778","-002.6545778","-016.6619444","-016.6619444","-016.6619444","-016.6619444","-016.6619444","-016.5908333","-016.5908333","-016.5908333","-016.5908333","-016.5908333","-121.6691667","-122.4688889")
for(x in 1:length(accs)){
	missing$Substrate[missing$Acc_No %in% accs[x]]<-"bark"
	missing$Substrate_Publication[missing$Acc_No %in% accs[x]]<-"del Campo et al. (2012), Casano et al. (2011)"
	missing$Substrate_Man_Cur[missing$Acc_No %in% accs[x]]<-"Yes"
	missing$Lat[missing$Acc_No %in% accs[x]]<-lat[x]
	missing$Long[missing$Acc_No %in% accs[x]]<-long[x]	
	missing$LL_Publication[missing$Acc_No %in% accs[x]]<-"del Campo et al. (2012), Casano et al. (2011)"
	missing$LL_Man_Cur[missing$Acc_No %in% accs[x]]<-'Yes'
}



#other del Campo et al. seqs perhaps used in del Campo et al. (2012), but no ecological info provided there



#Doering & Piercey-Normore (2009)
#all on Pinus banksiana bark
#tough to get coordinate info out of them, and accession numbers conflict with those in manuscript.
#EU683442 (T. impressa A. Parmelia sulcata 06019)
#EU683441
#EU683443
#EU683444 (from F. caperata, which was only found in transect 2)
#EU683440
accs<-c("EU683440","EU683441","EU683442","EU683443","EU683444")
for(x in 1:length(accs)){
	missing$Substrate[missing$Acc_No %in% accs[x]]<-"bark"
	missing$Substrate_Man_Cur[missing$Acc_No %in% accs[x]]<-'Yes'
	missing$Substrate_Publication[missing$Acc_No %in% accs[x]]<-"Doering & Piercey-Normore (2009)"
}


#Domaschke et al. (2012)
#presumably these are all on soil/rock
accs<-read.csv(file="xxxx/domaschke_et_al_2012_SI_table.csv",stringsAsFactors=FALSE)
sites<-read.csv(file="xxxx/domaschke_et_al_2012_sites.csv",stringsAsFactors=FALSE)
#changed Kazakhstan longitude to E and positive
sites$Long[sites$Site %in% "Kaz1"]<-(-1)*as.numeric(sites$Long[sites$Site %in% "Kaz1"])
#changed Turkey longitude to E and positive
sites$Long[sites$Site %in% "Tur1"]<-(-1)*as.numeric(sites$Long[sites$Site %in% "Tur1"])

accs$Lat<-NA
accs$Long<-NA

#add lat/long to Domaschke et al spreadsheet
for(x in 1:nrow(accs)){
	accs$Lat[x]<-sites$Lat[sites$Site %in% accs$Site[x]]
	accs$Long[x]<-sites$Long[sites$Site %in% accs$Site[x]]
}

#reduce accs to only those with algal ITS
nrow(accs)
accs<-accs[!accs$AITS %in% "",]
nrow(accs)

#transfer lat/long to missing spreadsheet
for(x in 1:nrow(accs)){
	missing$Lat[missing$Acc_No %in% accs$AITS[x]]<-accs$Lat[x]
	missing$Long[missing$Acc_No %in% accs$AITS[x]]<-accs$Long[x]
	missing$LL_Publication[missing$Acc_No %in% accs$AITS[x]]<-"Domaschke et al. (2012)"
	missing$LL_Man_Cur[missing$Acc_No %in% accs$AITS[x]]<-'Yes'
}

#some submitted to GenBank as part of this study, but not included
#Antarctica: GQ375316,GQ375315...listed in GenBank as being from Ant1
#Germany: GQ375336,GQ375334,GQ375332,GQ375330,GQ375328,GQ375326,GQ375324,GQ375347,GQ375335,GQ375333,GQ375331,GQ375329,GQ375327,GQ375325...listed in GenBank as being from Dtl1 and Dtl2
#Svalbard: GQ375366,GQ375387,GQ375367...listed in GenBank as being from Sva4
#Spain: GQ375337...listed in GenBank as being from Sp1
#Falklands: GQ375359,GQ375357...listed in GenBank as being from Fa2

#treat Antarctica as Ant1, Svalbard as Sva4, Spain (Sp1) as Spa1, and Falklands (Fa2) as Fal2
accs<-c("GQ375316","GQ375315")
for(x in 1:length(accs)){
	missing$Lat[missing$Acc_No %in% accs[x]]<-"-62.2463889"
	missing$Long[missing$Acc_No %in% accs[x]]<-"-58.6775"
	missing$LL_Publication[missing$Acc_No %in% accs[x]]<-"Domaschke et al. (2012)"
	missing$LL_Man_Cur[missing$Acc_No %in% accs[x]]<-'Yes'
}
accs<-c("GQ375366","GQ375387","GQ375367")
for(x in 1:length(accs)){
	missing$Lat[missing$Acc_No %in% accs[x]]<-"78.1791667"
	missing$Long[missing$Acc_No %in% accs[x]]<-"16.3066667"
	missing$LL_Publication[missing$Acc_No %in% accs[x]]<-"Domaschke et al. (2012)"
	missing$LL_Man_Cur[missing$Acc_No %in% accs[x]]<-'Yes'
}
accs<-("GQ375337")
for(x in 1:length(accs)){
	missing$Lat[missing$Acc_No %in% accs[x]]<-"42.2438889"
	missing$Long[missing$Acc_No %in% accs[x]]<-"-6.0091667"
	missing$LL_Publication[missing$Acc_No %in% accs[x]]<-"Domaschke et al. (2012)"
	missing$LL_Man_Cur[missing$Acc_No %in% accs[x]]<-'Yes'
}
accs<-c("GQ375359","GQ375357")
for(x in 1:length(accs)){
	missing$Lat[missing$Acc_No %in% accs[x]]<-"-51.6980556"
	missing$Long[missing$Acc_No %in% accs[x]]<-"-57.8202778"
	missing$LL_Publication[missing$Acc_No %in% accs[x]]<-"Domaschke et al. (2012)"
	missing$LL_Man_Cur[missing$Acc_No %in% accs[x]]<-'Yes'
}



#Domaschke et al. (2013)
#presumably all on ground, but not explicitly stated
#JN243330 is found in both Antarctica and Svalbard...treated just as Antarctica here as that was the first in listed in publication
accs<-c("JN243330","JN243328","JN243329","JN243325","JN243326","JN243327")
lat<-c("-62.5027778","78.1688056","78.1688056","41.6570917","41.6570917","49.9717500")
long<-c("-059.2877778","015.9419167","015.9419167","-003.5350556","-003.5350556","009.0801944")
for(x in 1:length(accs)){
	missing$Lat[missing$Acc_No %in% accs[x]]<-lat[x]
	missing$Long[missing$Acc_No %in% accs[x]]<-long[x]	
	missing$LL_Publication[missing$Acc_No %in% accs[x]]<-"Domaschke et al. (2013)"
	missing$LL_Man_Cur[missing$Acc_No %in% accs[x]]<-'Yes'
}

#some submitted to GenBank as part of this study, but not included
#Spain: JN243323,JN243324
#treat as part of the same Spain population in this paper
accs<-c("JN243323","JN243324")
for(x in 1:length(accs)){
	missing$Lat[missing$Acc_No %in% accs[x]]<-"41.6570917"
	missing$Long[missing$Acc_No %in% accs[x]]<-"-003.5350556"
	missing$LL_Publication[missing$Acc_No %in% accs[x]]<-"Domaschke et al. (2013)"
	missing$LL_Man_Cur[missing$Acc_No %in% accs[x]]<-'Yes'
}



#Engelen et al. (2010)
#all on ground
accs<-c("FJ406572","FJ406573","FJ406574","FJ406575","FJ406576","FJ406577")
for(x in 1:length(accs)){
	missing$Lat[missing$Acc_No %in% accs[x]]<-"-72.0500000"
	missing$Long[missing$Acc_No %in% accs[x]]<-"-068.5166667"
	missing$Substrate[missing$Acc_No %in% accs[x]]<-"ground"
	missing$LL_Publication[missing$Acc_No %in% accs[x]]<-"Engelen et al. (2010)"
	missing$LL_Man_Cur[missing$Acc_No %in% accs[x]]<-'Yes'
	missing$Substrate_Man_Cur[missing$Acc_No %in% accs[x]]<-'Yes'
	missing$Substrate_Publication[missing$Acc_No %in% accs[x]]<-"Engelen et al. (2010)"
}




#Engelen et al. (2016)
#Lat mistakenly given as 70S instead of 72S - corrected
#presumably all ground
#add in country info
accs<-paste("FJ4262",84:99,sep="")
for(x in 1:length(accs)){
	missing$Lat[missing$Acc_No %in% accs[x]]<-"-72.0500000"
	missing$Long[missing$Acc_No %in% accs[x]]<-"-068.5166667"
	missing$LL_Publication[missing$Acc_No %in% accs[x]]<-"Engelen et al. (2016)"
	missing$LL_Man_Cur[missing$Acc_No %in% accs[x]]<-'Yes'
	missing$Country_Detailed[missing$Acc_No %in% accs[x]]<-"Antarctica"
	missing$Country[missing$Acc_No %in% accs[x]]<-"Antarctica"
	missing$Geo_Man_Cur[missing$Acc_No %in% accs[x]]<-"Yes"
	missing$DB_Notes[missing$Acc_No %in% accs[x]]<-""
	missing$ISO3Code[missing$Acc_No %in% accs[x]]<-"ATA"
	missing$GEO3SubRegion[missing$Acc_No %in% accs[x]]<-"Antarctic"
	missing$GEO3MajorRegion[missing$Acc_No %in% accs[x]]<-"Polar"
}



#Ertz et al. (2018)
#added substrate and lat long to their table
accs<-read.csv(file="xxxx/ertz_et_al_2018_table.csv",stringsAsFactors=FALSE)

#reduce to only those with algal ITS's
nrow(accs)
accs<-accs[!accs$AITS1 %in% "",]
nrow(accs)

for(x in 1:nrow(accs)){
	if(!is.na(accs$Lat[x])){
		missing$Lat[missing$Acc_No %in% accs$AITS1[x]]<-accs$Lat[x]
		missing$Long[missing$Acc_No %in% accs$AITS1[x]]<-accs$Long[x]
		missing$Lat[missing$Acc_No %in% accs$AITS2[x]]<-accs$Lat[x]
		missing$Long[missing$Acc_No %in% accs$AITS2[x]]<-accs$Long[x]
		missing$LL_Publication[missing$Acc_No %in% accs$AITS1[x]]<-"Ertz et al. (2018)"
		missing$LL_Publication[missing$Acc_No %in% accs$AITS2[x]]<-"Ertz et al. (2018)"
		missing$LL_Man_Cur[missing$Acc_No %in% accs$AITS1[x]]<-'Yes'
		missing$LL_Man_Cur[missing$Acc_No %in% accs$AITS2[x]]<-'Yes'
	}
	if(!accs$Substrate[x] %in% ""){
		missing$Substrate[missing$Acc_No %in% accs$AITS1[x]]<-accs$Substrate[x]
		missing$Substrate[missing$Acc_No %in% accs$AITS2[x]]<-accs$Substrate[x]
		missing$Substrate_Man_Cur[missing$Acc_No %in% accs$AITS1[x]]<-'Yes'
		missing$Substrate_Publication[missing$Acc_No %in% accs$AITS1[x]]<-"Ertz et al. (2018)"
		missing$Substrate_Man_Cur[missing$Acc_No %in% accs$AITS2[x]]<-'Yes'
		missing$Substrate_Publication[missing$Acc_No %in% accs$AITS12[x]]<-"Ertz et al. (2018)"
	}
}






#not much in way of substrate or lat/long from Friedl et al. (2000) or AlgaTerra paper





#Gasulla et al. (2010)
#added substrate and lat long to their table
accs<-read.csv(file="xxxx/gasulla_et_al_2010_table.csv",stringsAsFactors=FALSE)

for(x in 1:nrow(accs)){
	missing$Lat[missing$Acc_No %in% accs$AITS[x]]<-accs$Lat[x]
	missing$Long[missing$Acc_No %in% accs$AITS[x]]<-accs$Long[x]
	missing$LL_Publication[missing$Acc_No %in% accs$AITS[x]]<-"Gasulla et al. (2010)"
	missing$LL_Man_Cur[missing$Acc_No %in% accs$AITS[x]]<-'Yes'
}

#also add in that FJ418565 and FJ418566 are from Spain
accs<-c("FJ418565","FJ418566")
for(x in 1:length(accs)){
	missing$Country_Detailed[missing$Acc_No %in% accs[x]]<-"Spain"
	missing$Country[missing$Acc_No %in% accs[x]]<-"Spain"
	missing$Geo_Man_Cur[missing$Acc_No %in% accs[x]]<-"Yes"
	missing$DB_Notes[missing$Acc_No %in% accs[x]]<-""
	missing$ISO3Code[missing$Acc_No %in% accs[x]]<-"ESP"
	missing$GEO3SubRegion[missing$Acc_No %in% accs[x]]<-"Western Europe"
	missing$GEO3MajorRegion[missing$Acc_No %in% accs[x]]<-"Europe"
}


#Guzow-Krzeminska (2006) substrate listed, but not lat long
accs<-read.csv(file="xxxx/guzow_krzeminska_2006_protoparmeliopsis_photobiont.csv",stringsAsFactors=FALSE)
for(x in 1:nrow(accs)){
	missing$Substrate[missing$Acc_No %in% accs$Accession[x]]<-accs$Substrate[x]
	missing$Substrate_Man_Cur[missing$Acc_No %in% accs$Accession[x]]<-'Yes'
	missing$Substrate_Publication[missing$Acc_No %in% accs$Accession[x]]<-"Guzow-Krzeminska (2006)"
}

#Malbork Castle...coords not given in paper, but found through Wikipedia to be: 54.039722, 19.027778
#should also be able to get coords for Graz botanical garden

#Guzow-Krzeminska & Stocker-Worgotter (2013) does not have substrate or lat/long, but specimens are presumably ground/stone



#Helms et al. (2001)
accs<-read.csv(file="xxxx/helms_et_al_2001_substrate_ll.csv",stringsAsFactors=FALSE)
for(x in 1:nrow(accs)){
	missing$Substrate[missing$Acc_No %in% accs$Accession[x]]<-accs$Substrate[x]
	missing$Substrate_Man_Cur[missing$Acc_No %in% accs$Accession[x]]<-'Yes'
	missing$Substrate_Publication[missing$Acc_No %in% accs$Accession[x]]<-"Helms et al. (2001)"
}

#now keep only those w ll
nrow(accs)
accs<-accs[!is.na(accs$Lat),]
nrow(accs)

for(x in 1:nrow(accs)){
	missing$Lat[missing$Acc_No %in% accs$Accession[x]]<-accs$Lat[x]
	missing$Long[missing$Acc_No %in% accs$Accession[x]]<-accs$Long[x]
	missing$LL_Publication[missing$Acc_No %in% accs$Accession[x]]<-"Helms et al. (2001), Helms (2003)"
	missing$LL_Man_Cur[missing$Acc_No %in% accs$Accession[x]]<-'Yes'
}


#Italy
#changed "AJ2937821" to "AJ293781" - think an extra digit added in ("2")
accs<-c("AJ293780","AJ293781")
for(x in 1:length(accs)){
	missing$Country_Detailed[missing$Acc_No %in% accs[x]]<-"Italy"
	missing$Country[missing$Acc_No %in% accs[x]]<-"Italy"
	missing$Geo_Man_Cur[missing$Acc_No %in% accs[x]]<-"Yes"
	missing$DB_Notes[missing$Acc_No %in% accs[x]]<-"Listed as from Germany in NCBI, but publication suggests differently"
	missing$ISO3Code[missing$Acc_No %in% accs[x]]<-"ITA"
	missing$GEO3SubRegion[missing$Acc_No %in% accs[x]]<-"Western Europe"
	missing$GEO3MajorRegion[missing$Acc_No %in% accs[x]]<-"Europe"
}

#Austria
accs<-c("AJ293782","AJ293785","AJ293791","AJ293793")
for(x in 1:length(accs)){
	missing$Country_Detailed[missing$Acc_No %in% accs[x]]<-"Austria"
	missing$Country[missing$Acc_No %in% accs[x]]<-"Austria"
	missing$Geo_Man_Cur[missing$Acc_No %in% accs[x]]<-"Yes"
	missing$DB_Notes[missing$Acc_No %in% accs[x]]<-"Listed as from Germany in NCBI, but publication suggests differently"
	missing$ISO3Code[missing$Acc_No %in% accs[x]]<-"AUT"
	missing$GEO3SubRegion[missing$Acc_No %in% accs[x]]<-"Western Europe"
	missing$GEO3MajorRegion[missing$Acc_No %in% accs[x]]<-"Europe"
}

#Australia
accs<-"AJ293783"
for(x in 1:length(accs)){
	missing$Country_Detailed[missing$Acc_No %in% accs[x]]<-"Australia"
	missing$Country[missing$Acc_No %in% accs[x]]<-"Australia"
	missing$Geo_Man_Cur[missing$Acc_No %in% accs[x]]<-"Yes"
	missing$DB_Notes[missing$Acc_No %in% accs[x]]<-"Listed as from Germany in NCBI, but publication suggests differently"
	missing$ISO3Code[missing$Acc_No %in% accs[x]]<-"AUS"
	missing$GEO3SubRegion[missing$Acc_No %in% accs[x]]<-"Australia and New Zealand"
	missing$GEO3MajorRegion[missing$Acc_No %in% accs[x]]<-"Asia and the Pacific"
}

#Spain, Canary Islands
accs<-"AJ293784"
for(x in 1:length(accs)){
	missing$Country_Detailed[missing$Acc_No %in% accs[x]]<-"Spain"
	missing$Country[missing$Acc_No %in% accs[x]]<-"Spain"
	missing$Geo_Man_Cur[missing$Acc_No %in% accs[x]]<-"Yes"
	missing$DB_Notes[missing$Acc_No %in% accs[x]]<-"Listed as from Germany in NCBI, but publication suggests differently"
	missing$ISO3Code[missing$Acc_No %in% accs[x]]<-"ESP"
	missing$GEO3SubRegion[missing$Acc_No %in% accs[x]]<-"Western Europe"
	missing$GEO3MajorRegion[missing$Acc_No %in% accs[x]]<-"Europe"
}

#Spain, mainland
accs<-"AJ293787"
for(x in 1:length(accs)){
	missing$Country_Detailed[missing$Acc_No %in% accs[x]]<-"Spain"
	missing$Country[missing$Acc_No %in% accs[x]]<-"Spain"
	missing$Geo_Man_Cur[missing$Acc_No %in% accs[x]]<-"Yes"
	missing$DB_Notes[missing$Acc_No %in% accs[x]]<-"Listed as from Germany in NCBI, but publication suggests differently"
	missing$ISO3Code[missing$Acc_No %in% accs[x]]<-"ESP"
	missing$GEO3SubRegion[missing$Acc_No %in% accs[x]]<-"Western Europe"
	missing$GEO3MajorRegion[missing$Acc_No %in% accs[x]]<-"Europe"
}

#USA
accs<-"AJ293776"
for(x in 1:length(accs)){
	missing$Country_Detailed[missing$Acc_No %in% accs[x]]<-"USA"
	missing$Country[missing$Acc_No %in% accs[x]]<-"USA"
	missing$Geo_Man_Cur[missing$Acc_No %in% accs[x]]<-"Yes"
	missing$DB_Notes[missing$Acc_No %in% accs[x]]<-"Listed as from Germany in NCBI, but publication suggests differently"
	missing$ISO3Code[missing$Acc_No %in% accs[x]]<-"USA"
	missing$GEO3SubRegion[missing$Acc_No %in% accs[x]]<-"US"
	missing$GEO3MajorRegion[missing$Acc_No %in% accs[x]]<-"North America"
}

#Finland
accs<-"AJ293794"
for(x in 1:length(accs)){
	missing$Country_Detailed[missing$Acc_No %in% accs[x]]<-"Finland"
	missing$Country[missing$Acc_No %in% accs[x]]<-"Finland"
	missing$Geo_Man_Cur[missing$Acc_No %in% accs[x]]<-"Yes"
	missing$DB_Notes[missing$Acc_No %in% accs[x]]<-"Listed as from Germany in NCBI, but publication suggests differently"
	missing$ISO3Code[missing$Acc_No %in% accs[x]]<-"FIN"
	missing$GEO3SubRegion[missing$Acc_No %in% accs[x]]<-"Western Europe"
	missing$GEO3MajorRegion[missing$Acc_No %in% accs[x]]<-"Europe"
}

#Greece
accs<-c("AJ293789","AJ293790")
for(x in 1:length(accs)){
	missing$Country_Detailed[missing$Acc_No %in% accs[x]]<-"Greece"
	missing$Country[missing$Acc_No %in% accs[x]]<-"Greece"
	missing$Geo_Man_Cur[missing$Acc_No %in% accs[x]]<-"Yes"
	missing$DB_Notes[missing$Acc_No %in% accs[x]]<-"Listed as from Germany in NCBI, but publication suggests differently"
	missing$ISO3Code[missing$Acc_No %in% accs[x]]<-"GRC"
	missing$GEO3SubRegion[missing$Acc_No %in% accs[x]]<-"Western Europe"
	missing$GEO3MajorRegion[missing$Acc_No %in% accs[x]]<-"Europe"
}

#Costa Rica
accs<-"AJ293792"
for(x in 1:length(accs)){
	missing$Country_Detailed[missing$Acc_No %in% accs[x]]<-"Costa Rica"
	missing$Country[missing$Acc_No %in% accs[x]]<-"Costa Rica"
	missing$Geo_Man_Cur[missing$Acc_No %in% accs[x]]<-"Yes"
	missing$DB_Notes[missing$Acc_No %in% accs[x]]<-"Listed as from Germany in NCBI, but publication suggests differently"
	missing$ISO3Code[missing$Acc_No %in% accs[x]]<-"CRI"
	missing$GEO3SubRegion[missing$Acc_No %in% accs[x]]<-"Meso-America"
	missing$GEO3MajorRegion[missing$Acc_No %in% accs[x]]<-"Latin America and the Caribbean"
}

#Hauck et al. (2007)
#go through site by site
#Germany - Hessia and Bavaria w/o ll/substrate...nothing to add w these
#accs<-c("AJ511363","AJ511366","AJ511365","AJ511364")
#substrate already present in NCBI

#Montana, USA - LL given in Hauck & Spribille (2005) 48 38' 15"N, 115 3' 10" W, which is 48.6375000 -115.0527778
accs<-c("AJ511357","AJ511356")
for(x in 1:length(accs)){
	missing$Lat[missing$Acc_No %in% accs[x]]<-"48.6375000"
	missing$Long[missing$Acc_No %in% accs[x]]<-"-115.0527778"
	#missing$Substrate[missing$Acc_No %in% accs[x]]<-"bark"	
	missing$LL_Publication[missing$Acc_No %in% accs[x]]<-"Hauck et al. (2007), Hauck & Spribille (2005)"
	missing$LL_Man_Cur[missing$Acc_No %in% accs[x]]<-'Yes'
}

#Harz Mtns 51 45' N, 10 30' E, which is 51.7500000 010.5000000
accs<-c("AJ511355","AJ511358","AJ511359","AJ511360","AJ511361","AJ511362","AJ511351","AJ511352","AJ511353","AJ511350","AJ511354")
for(x in 1:length(accs)){
	missing$Lat[missing$Acc_No %in% accs[x]]<-"51.7500000"
	missing$Long[missing$Acc_No %in% accs[x]]<-"010.5000000"
	#missing$Substrate[missing$Acc_No %in% accs[x]]<-"bark"	
	missing$LL_Publication[missing$Acc_No %in% accs[x]]<-"Hauck et al. (2007)"
	missing$LL_Man_Cur[missing$Acc_No %in% accs[x]]<-'Yes'
}




#Hestmark et al. (2010)
#presumably on rock
accs<-read.csv(file="xxxx/hestmark_et_al_2010_photobiont_associations_in_cooccurring_umbilicate_lichens_with_contrasting_modes_of_reproduction_in_coastal_norway.csv",stringsAsFactors=FALSE)
for(x in 1:nrow(accs)){
	missing$Lat[missing$Acc_No %in% accs$AITS_Lasallia[x]]<-accs$Lat[x]
	missing$Long[missing$Acc_No %in% accs$AITS_Lasallia[x]]<-accs$Long[x]
	missing$LL_Publication[missing$Acc_No %in% accs$AITS_Lasallia[x]]<-"Hestmark et al. (2010)"
	missing$LL_Man_Cur[missing$Acc_No %in% accs$AITS_Lasallia[x]]<-'Yes'
	missing$Lat[missing$Acc_No %in% accs$AITS_Umbilicaria[x]]<-accs$Lat[x]
	missing$Long[missing$Acc_No %in% accs$AITS_Umbilicaria[x]]<-accs$Long[x]
	missing$LL_Publication[missing$Acc_No %in% accs$AITS_Umbilicaria[x]]<-"Hestmark et al. (2010)"
	missing$LL_Man_Cur[missing$Acc_No %in% accs$AITS_Umbilicaria[x]]<-'Yes'
}





#Do not know of a publication for Jang et al.




#Jones et al. (2013)
#coords in NCBI
#see if can straighten out host and substrate
#presumably all rock, but host is an issue



#Jroundi et al. (2015) - samples at Royal Chapel of Granada...Wikipedia lists this as 37.175986, -3.599036


#Kotelko & Piercey-Normore (2010) - could potentially be georeferenced
#they note pocillum and pyxidatum grow on soil...algae also sequenced from monomorpha, magyarica and chlorophaea...presumably also on ground

#Kroken & Taylor (2000)...could probably georeference many...2001 Mycologia papers notes most specimens were on conifer bark








#Li et al. (2013) - antarctic specimens from Antarctica, and arctic specimens from Ny-Alesund in Svalbard, and could infer coordinates.  Specimens are presumably from the ground.
#update country info etc.
#Antarctica...wikipedia has coords as -62.033333, -58.35
accs<-c("JX509856","JX509857","JX509858","JX509839","JX509855","JX509859","JX509853","JX509854","JX518612","JX509840","JX509841","JX509842","JX509860","JX509861","JX509846","JX509851","JX509852","JX509844","JX509843","JX509845","JX509847","JX509848","JX509849","JX509850")

for(x in 1:length(accs)){
	missing$Country_Detailed[missing$Acc_No %in% accs[x]]<-"Antarctica"
	missing$Country[missing$Acc_No %in% accs[x]]<-"Antarctica"
	missing$Geo_Man_Cur[missing$Acc_No %in% accs[x]]<-"Yes"
	missing$DB_Notes[missing$Acc_No %in% accs[x]]<-""
	missing$ISO3Code[missing$Acc_No %in% accs[x]]<-"ATA"
	missing$GEO3SubRegion[missing$Acc_No %in% accs[x]]<-"Antarctic"
	missing$GEO3MajorRegion[missing$Acc_No %in% accs[x]]<-"Polar"
}

#Svalbard...wikipedia has coords for Ny-Alesund as 78.925, 11.922222
accs<-c("JX518606","JX518607","JX518605","JX518608","JX518609","JX518610","JX518611","JX518612")
for(x in 1:length(accs)){
	missing$Country_Detailed[missing$Acc_No %in% accs[x]]<-"Svalbard"
	missing$Country[missing$Acc_No %in% accs[x]]<-"Norway"
	missing$Geo_Man_Cur[missing$Acc_No %in% accs[x]]<-"Yes"
	missing$DB_Notes[missing$Acc_No %in% accs[x]]<-""
	missing$ISO3Code[missing$Acc_No %in% accs[x]]<-"NOR"
	missing$GEO3SubRegion[missing$Acc_No %in% accs[x]]<-"Western Europe"
	missing$GEO3MajorRegion[missing$Acc_No %in% accs[x]]<-"Europe"
}

#Need to also add Host info
accs<-c("JX509856","JX509857","JX509858","JX509839","JX509855","JX509859")
missing$Host_Detailed[missing$Acc_No %in% accs]<-"Umbilicaria antarctica"
missing$Host[missing$Acc_No %in% accs]<-"Umbilicaria_antarctica"
missing$Host_Genus[missing$Acc_No %in% accs]<-"Umbilicaria"
missing$Host_Species[missing$Acc_No %in% accs]<-"antarctica"
missing$Host_Man_Cur[missing$Acc_No %in% accs]<-"Yes"
missing$Host_Family[missing$Acc_No %in% accs]<-"Umbilicariaceae"
missing$Host_Order[missing$Acc_No %in% accs]<-"Umbilicariales"
missing$Host_Class[missing$Acc_No %in% accs]<-"Lecanoromycetes"
missing$Host_Publication[missing$Acc_No %in% accs]<-missing$NCBI_First_Author[missing$Acc_No %in% accs]


accs<-c("JX518606","JX518607","JX518605")
missing$Host_Detailed[missing$Acc_No %in% accs]<-"Umbilicaria arctica"
missing$Host[missing$Acc_No %in% accs]<-"Umbilicaria_arctica"
missing$Host_Genus[missing$Acc_No %in% accs]<-"Umbilicaria"
missing$Host_Species[missing$Acc_No %in% accs]<-"arctica"
missing$Host_Man_Cur[missing$Acc_No %in% accs]<-"Yes"
missing$Host_Family[missing$Acc_No %in% accs]<-"Umbilicariaceae"
missing$Host_Order[missing$Acc_No %in% accs]<-"Umbilicariales"
missing$Host_Class[missing$Acc_No %in% accs]<-"Lecanoromycetes"
missing$Host_Publication[missing$Acc_No %in% accs]<-missing$NCBI_First_Author[missing$Acc_No %in% accs]

accs<-c("JX518608","JX518609","JX518610","JX518611")
missing$Host_Detailed[missing$Acc_No %in% accs]<-"Umbilicaria decussata"
missing$Host[missing$Acc_No %in% accs]<-"Umbilicaria_decussata"
missing$Host_Genus[missing$Acc_No %in% accs]<-"Umbilicaria"
missing$Host_Species[missing$Acc_No %in% accs]<-"decussata"
missing$Host_Man_Cur[missing$Acc_No %in% accs]<-"Yes"
missing$Host_Family[missing$Acc_No %in% accs]<-"Umbilicariaceae"
missing$Host_Order[missing$Acc_No %in% accs]<-"Umbilicariales"
missing$Host_Class[missing$Acc_No %in% accs]<-"Lecanoromycetes"
missing$Host_Publication[missing$Acc_No %in% accs]<-missing$NCBI_First_Author[missing$Acc_No %in% accs]

accs<-"JX518612"
missing$Host_Detailed[missing$Acc_No %in% accs]<-"Umbilicaria lyngei"
missing$Host[missing$Acc_No %in% accs]<-"Umbilicaria_lyngei"
missing$Host_Genus[missing$Acc_No %in% accs]<-"Umbilicaria"
missing$Host_Species[missing$Acc_No %in% accs]<-"lyngei"
missing$Host_Man_Cur[missing$Acc_No %in% accs]<-"Yes"
missing$Host_Family[missing$Acc_No %in% accs]<-"Umbilicariaceae"
missing$Host_Order[missing$Acc_No %in% accs]<-"Umbilicariales"
missing$Host_Class[missing$Acc_No %in% accs]<-"Lecanoromycetes"
missing$Host_Publication[missing$Acc_No %in% accs]<-missing$NCBI_First_Author[missing$Acc_No %in% accs]

accs<-c("JX509853","JX509854")
missing$Host_Detailed[missing$Acc_No %in% accs]<-"Usnea antarctica"
missing$Host[missing$Acc_No %in% accs]<-"Usnea_antarctica"
missing$Host_Genus[missing$Acc_No %in% accs]<-"Usnea"
missing$Host_Species[missing$Acc_No %in% accs]<-"antarctica"
missing$Host_Man_Cur[missing$Acc_No %in% accs]<-"Yes"
missing$Host_Family[missing$Acc_No %in% accs]<-"Parmeliaceae"
missing$Host_Order[missing$Acc_No %in% accs]<-"Lecanorales"
missing$Host_Class[missing$Acc_No %in% accs]<-"Lecanoromycetes"
missing$Host_Publication[missing$Acc_No %in% accs]<-missing$NCBI_First_Author[missing$Acc_No %in% accs]

accs<-c("JX518612","JX509840","JX509841","JX509842","JX509860","JX509861","JX509846","JX509851","JX509852","JX509844","JX509843","JX509845","JX509847","JX509848","JX509849","JX509850")
missing$Host_Detailed[missing$Acc_No %in% accs]<-"Usnea aurantiacoatra"
missing$Host[missing$Acc_No %in% accs]<-"Usnea_aurantiacoatra"
missing$Host_Genus[missing$Acc_No %in% accs]<-"Usnea"
missing$Host_Species[missing$Acc_No %in% accs]<-"aurantiacoatra"
missing$Host_Man_Cur[missing$Acc_No %in% accs]<-"Yes"
missing$Host_Family[missing$Acc_No %in% accs]<-"Parmeliaceae"
missing$Host_Order[missing$Acc_No %in% accs]<-"Lecanorales"
missing$Host_Class[missing$Acc_No %in% accs]<-"Lecanoromycetes"
missing$Host_Publication[missing$Acc_No %in% accs]<-missing$NCBI_First_Author[missing$Acc_No %in% accs]



#Lindblom & Sochting (2013) - substrate presumably ground

#Lindgren et al. (2014) - no lat long.

#Cannot find Liu publication on Boreoplaca ultrafrigida photobionts



#Lutsak
#update mycobiont ID's to Cetraria aculeata
accs<-read.csv(file="xxxx/lutsak_et_al_2015.csv",stringsAsFactors=FALSE)
for(x in 1:nrow(accs)){
	if(missing$Host_Detailed[missing$Acc_No %in% accs$AITS[x]]=="None"){
		missing$Host_Detailed[missing$Acc_No %in% accs$AITS[x]]<-"Cetraria aculeata"
		missing$Host[missing$Acc_No %in% accs$AITS[x]]<-"Cetraria_aculeata"
		missing$Host_Genus[missing$Acc_No %in% accs$AITS[x]]<-"Cetraria"
		missing$Host_Species[missing$Acc_No %in% accs$AITS[x]]<-"aculeata"
		missing$Host_Man_Cur[missing$Acc_No %in% accs$AITS[x]]<-"Yes"
		missing$Host_Family[missing$Acc_No %in% accs$AITS[x]]<-"Parmeliaceae"
		missing$Host_Order[missing$Acc_No %in% accs$AITS[x]]<-"Lecanorales"
		missing$Host_Class[missing$Acc_No %in% accs$AITS[x]]<-"Lecanoromycetes"
		missing$Host_Publication[missing$Acc_No %in% accs$AITS[x]]<-missing$NCBI_First_Author[missing$Acc_No %in% accs$AITS[x]]
	}
}

#drop duplicates because some accessions (haplotypes) found in multiple sites - retain first one...these accessions are also listed in NCBI as being from Scotland (KT827672), Scotland (KT827678), and Spain (KT827683) - consistent w this approach.
nrow(accs)
accs<-accs[!duplicated(accs$AITS),]
nrow(accs)

for(x in 1:nrow(accs)){
	missing$Lat[missing$Acc_No %in% accs$AITS[x]]<-accs$Lat[x]
	missing$Long[missing$Acc_No %in% accs$AITS[x]]<-accs$Long[x]
	missing$LL_Publication[missing$Acc_No %in% accs$AITS[x]]<-"Lutsak et al. (2015)"
	missing$LL_Man_Cur[missing$Acc_No %in% accs$AITS[x]]<-'Yes'
}


#Also need to change KT827662 country from Norway to Iceland...something wrong in NCBI submission
accs<-"KT827662"
for(x in 1:length(accs)){
	missing$Country_Detailed[missing$Acc_No %in% accs[x]]<-"Iceland"
	missing$Country[missing$Acc_No %in% accs[x]]<-"Iceland"
	missing$Geo_Man_Cur[missing$Acc_No %in% accs[x]]<-"Yes"
	missing$DB_Notes[missing$Acc_No %in% accs[x]]<-"listed as Norway:Svalbard in NCBI submission, but changed to Iceland to reflect publication"
	missing$ISO3Code[missing$Acc_No %in% accs[x]]<-"ISL"
	missing$GEO3SubRegion[missing$Acc_No %in% accs[x]]<-"Western Europe"
	missing$GEO3MajorRegion[missing$Acc_No %in% accs[x]]<-"Europe"
}



#Mansournia et al. (2012)
#all accessions on bark
require(openxlsx)
accs<-readWorkbook(xlsxFile="xxxx/mansournia_et_al_2012.xlsx",colNames=FALSE)
for(x in 1:nrow(accs)){
	missing$Substrate[missing$Acc_No %in% accs$X1[x]]<-"bark"
	missing$Substrate_Man_Cur[missing$Acc_No %in% accs$X1[x]]<-'Yes'
	missing$Substrate_Publication[missing$Acc_No %in% accs$X1[x]]<-"Mansournia et al. (2012)"
}


#can only get lat long for these
#35 20.3'N 140 23.6E  35.3383333 140.3933333
require(openxlsx)
accs<-readWorkbook(xlsxFile="xxxx/mansournia_et_al_2012_these_are_from_the_tables_and_in_the_quadrat_on420.xlsx",colNames=FALSE)
for(x in 1:nrow(accs)){
	missing$Lat[missing$Acc_No %in% accs$X1[x]]<-"35.3383333"
	missing$Long[missing$Acc_No %in% accs$X1[x]]<-"140.3933333"
	missing$LL_Publication[missing$Acc_No %in% accs$X1[x]]<-"Mansournia et al. (2012)"
	missing$LL_Man_Cur[missing$Acc_No %in% accs$X1[x]]<-'Yes'
}




#Molins et al. (2018)
"Lichens as micro-ecosystems:novel approaches to efficiently reveal the hidden diversity of phycobionts in a single thallus"
#Circinaria hispida specimen from 40°46'02"N, 2°11'40"W and presumably from ground 40.7672222 -002.1944444
#Flavoparmelia soredians specimen from 39°51'13.68"N, 0°19'09.60"W from bark 39.8538000 -000.3193333
missing$Lat[missing$NCBI_Title %in% "Lichens as micro-ecosystems:novel approaches to efficiently reveal the hidden diversity of phycobionts in a single thallus" & missing$Host %in% "Circinaria_hispida"]<-"40.7672222"
missing$Long[missing$NCBI_Title %in% "Lichens as micro-ecosystems:novel approaches to efficiently reveal the hidden diversity of phycobionts in a single thallus" & missing$Host %in% "Circinaria_hispida"]<-"-002.1944444"
missing$LL_Publication[missing$NCBI_Title %in% "Lichens as micro-ecosystems:novel approaches to efficiently reveal the hidden diversity of phycobionts in a single thallus" & missing$Host %in% "Circinaria_hispida"]<-"Molins et al. (2018)"
missing$LL_Man_Cur[missing$NCBI_Title %in% "Lichens as micro-ecosystems:novel approaches to efficiently reveal the hidden diversity of phycobionts in a single thallus" & missing$Host %in% "Circinaria_hispida"]<-"Yes"
missing$Substrate_Publication[missing$NCBI_Title %in% "Lichens as micro-ecosystems:novel approaches to efficiently reveal the hidden diversity of phycobionts in a single thallus" & missing$Host %in% "Circinaria_hispida"]<-"Molins et al. (2018)"
missing$Substrate_Man_Cur[missing$NCBI_Title %in% "Lichens as micro-ecosystems:novel approaches to efficiently reveal the hidden diversity of phycobionts in a single thallus" & missing$Host %in% "Circinaria_hispida"]<-"Yes"


missing$Lat[missing$NCBI_Title %in% "Lichens as micro-ecosystems:novel approaches to efficiently reveal the hidden diversity of phycobionts in a single thallus" & missing$Host %in% "Flavoparmelia_soredians"]<-"39.8538000"
missing$Long[missing$NCBI_Title %in% "Lichens as micro-ecosystems:novel approaches to efficiently reveal the hidden diversity of phycobionts in a single thallus" & missing$Host %in% "Flavoparmelia_soredians"]<-"-000.3193333"
missing$Substrate[missing$NCBI_Title %in% "Lichens as micro-ecosystems:novel approaches to efficiently reveal the hidden diversity of phycobionts in a single thallus" & missing$Host %in% "Flavoparmelia_soredians"]<-"bark"
missing$LL_Publication[missing$NCBI_Title %in% "Lichens as micro-ecosystems:novel approaches to efficiently reveal the hidden diversity of phycobionts in a single thallus" & missing$Host %in% "Flavoparmelia_soredians"]<-"Molins et al. (2018)"
missing$LL_Man_Cur[missing$NCBI_Title %in% "Lichens as micro-ecosystems:novel approaches to efficiently reveal the hidden diversity of phycobionts in a single thallus" & missing$Host %in% "Flavoparmelia_soredians"]<-"Yes"
missing$Substrate_Publication[missing$NCBI_Title %in% "Lichens as micro-ecosystems:novel approaches to efficiently reveal the hidden diversity of phycobionts in a single thallus" & missing$Host %in% "Flavoparmelia_soredians"]<-"Molins et al. (2018)"
missing$Substrate_Man_Cur[missing$NCBI_Title %in% "Lichens as micro-ecosystems:novel approaches to efficiently reveal the hidden diversity of phycobionts in a single thallus" & missing$Host %in% "Flavoparmelia_soredians"]<-"Yes"




#Muggia et al. (2008) Caloplaca - could probably georeference these accessions
#all from rock
accs<-c("EF095233","EF095232","EF095234","EF095230","EF095231","EF095231")
missing$Substrate[missing$Acc_No %in% accs]<-"rock"
missing$Substrate_Publication[missing$Acc_No %in% accs]<-"Muggia et al. (2008) Caloplaca"
missing$Substrate_Man_Cur[missing$Acc_No %in% accs]<-"Yes"



#Muggia et al. (2008) Tephromela - could probably georeference these
#Tephromela atra calcarea from rock, Tephromela atra torulosa from bark, Tephromela atra from rock and Tephromela grumosa from rock
missing$Substrate[missing$NCBI_Title %in% "Genetic diversity and photobiont associations in selected taxa of the Tephromela atra group (Lecanorales, lichenised Ascomycota)" & missing$Host %in% "Tephromela_atra"]<-"rock"
missing$Substrate[missing$NCBI_Title %in% "Genetic diversity and photobiont associations in selected taxa of the Tephromela atra group (Lecanorales, lichenised Ascomycota)" & missing$Host %in% "Tephromela_grumosa"]<-"rock"
missing$Substrate[missing$NCBI_Title %in% "Genetic diversity and photobiont associations in selected taxa of the Tephromela atra group (Lecanorales, lichenised Ascomycota)" & missing$Host %in% "Tephromela_atra_calcarea"]<-"rock"
missing$Substrate[missing$NCBI_Title %in% "Genetic diversity and photobiont associations in selected taxa of the Tephromela atra group (Lecanorales, lichenised Ascomycota)" & missing$Host %in% "Tephromela_atra_torulosa"]<-"bark"

missing$Substrate_Publication[missing$NCBI_Title %in% "Genetic diversity and photobiont associations in selected taxa of the Tephromela atra group (Lecanorales, lichenised Ascomycota)" & missing$Host %in% "Tephromela_atra"]<-"Muggia et al. (2008) Tephromela"
missing$Substrate_Man_Cur[missing$NCBI_Title %in% "Genetic diversity and photobiont associations in selected taxa of the Tephromela atra group (Lecanorales, lichenised Ascomycota)" & missing$Host %in% "Tephromela_atra"]<-"Yes"
missing$Substrate_Publication[missing$NCBI_Title %in% "Genetic diversity and photobiont associations in selected taxa of the Tephromela atra group (Lecanorales, lichenised Ascomycota)" & missing$Host %in% "Tephromela_grumosa"]<-"Muggia et al. (2008) Tephromela"
missing$Substrate_Man_Cur[missing$NCBI_Title %in% "Genetic diversity and photobiont associations in selected taxa of the Tephromela atra group (Lecanorales, lichenised Ascomycota)" & missing$Host %in% "Tephromela_grumosa"]<-"Yes"
missing$Substrate_Publication[missing$NCBI_Title %in% "Genetic diversity and photobiont associations in selected taxa of the Tephromela atra group (Lecanorales, lichenised Ascomycota)" & missing$Host %in% "Tephromela_atra_calcarea"]<-"Muggia et al. (2008) Tephromela"
missing$Substrate_Man_Cur[missing$NCBI_Title %in% "Genetic diversity and photobiont associations in selected taxa of the Tephromela atra group (Lecanorales, lichenised Ascomycota)" & missing$Host %in% "Tephromela_atra_calcarea"]<-"Yes"
missing$Substrate_Publication[missing$NCBI_Title %in% "Genetic diversity and photobiont associations in selected taxa of the Tephromela atra group (Lecanorales, lichenised Ascomycota)" & missing$Host %in% "Tephromela_atra_torulosa"]<-"Muggia et al. (2008) Tephromela"
missing$Substrate_Man_Cur[missing$NCBI_Title %in% "Genetic diversity and photobiont associations in selected taxa of the Tephromela atra group (Lecanorales, lichenised Ascomycota)" & missing$Host %in% "Tephromela_atra_torulosa"]<-"Yes"


#Muggia et al. (2013)
#Locality 1 on calcareous rocks 47°12'02"N,5°28'18"E 47.2005556 005.4716667
#This locality is in Italy, and paper states in Austria.  What is likely the issue is that longitude should be 15 instead, so changed it.
accs<-c("KC740600",paste("KC74060",1:9,sep=""),"KC740610",paste("KC74061",1:7,sep=""))
missing$Substrate[missing$Acc_No %in% accs]<-"rock"
missing$Lat[missing$Acc_No %in% accs]<-"47.2005556"
missing$Long[missing$Acc_No %in% accs]<-"015.4716667"
missing$LL_Publication[missing$Acc_No %in% accs]<-"Muggia et al. (2013)"
missing$LL_Man_Cur[missing$Acc_No %in% accs]<-"Yes"
missing$Substrate_Publication[missing$Acc_No %in% accs]<-"Muggia et al. (2013)"
missing$Substrate_Man_Cur[missing$Acc_No %in% accs]<-"Yes"

#Locality 5 on concrete wall 50°10'31"N, 14°6'35"E 50.1752778 014.1097222
accs<-paste("KC7405",58:99,sep="")
missing$Substrate[missing$Acc_No %in% accs]<-"concrete"
missing$Lat[missing$Acc_No %in% accs]<-"50.1752778"
missing$Long[missing$Acc_No %in% accs]<-"014.1097222"
missing$LL_Publication[missing$Acc_No %in% accs]<-"Muggia et al. (2013)"
missing$LL_Man_Cur[missing$Acc_No %in% accs]<-"Yes"
missing$Substrate_Publication[missing$Acc_No %in% accs]<-"Muggia et al. (2013)"
missing$Substrate_Man_Cur[missing$Acc_No %in% accs]<-"Yes"


#Muggia et al. (2014)
accs<-read.csv(file="xxxx/muggia_et_al_2014_S1.csv",stringsAsFactors=FALSE)
for(x in 1:nrow(accs)){
	if(!is.na(accs$Lat[x])){
		missing$Lat[missing$Acc_No %in% accs$Acc[x]]<-accs$Lat[x]
		missing$Long[missing$Acc_No %in% accs$Acc[x]]<-accs$Long[x]
		missing$LL_Publication[missing$Acc_No %in% accs$Acc[x]]<-"Muggia et al. (2014)"
		missing$LL_Man_Cur[missing$Acc_No %in% accs$Acc[x]]<-"Yes"	
	}
	if(!accs$Substrate[x] %in% ""){
		missing$Substrate[missing$Acc_No %in% accs$Acc[x]]<-accs$Substrate[x]
		missing$Substrate_Publication[missing$Acc_No %in% accs$Acc[x]]<-"Muggia et al. (2014)"
		missing$Substrate_Man_Cur[missing$Acc_No %in% accs$Acc[x]]<-"Yes"	
	}
}

#KJ754311 is in Mexico not USA
accs<-"KJ754311"
for(x in 1:length(accs)){
	missing$Country_Detailed[missing$Acc_No %in% accs[x]]<-"Mexico"
	missing$Country[missing$Acc_No %in% accs[x]]<-"Mexico"
	missing$Geo_Man_Cur[missing$Acc_No %in% accs[x]]<-"Yes"
	missing$DB_Notes[missing$Acc_No %in% accs[x]]<-""
	missing$ISO3Code[missing$Acc_No %in% accs[x]]<-"MEX"
	missing$GEO3SubRegion[missing$Acc_No %in% accs[x]]<-"Meso-America"
	missing$GEO3MajorRegion[missing$Acc_No %in% accs[x]]<-"Latin America and the Caribbean"
}



#Nyati et al.
#no solid lat long/substrate info



#Ohmura et al. (2008) can georeference sites
#all on stone
#"Genetic combinations of symbionts in a vegetatively reproducing lichen, Parmotrema tinctorum, based on ITS rDNA sequences"
missing$Substrate[missing$NCBI_Title %in% "Genetic combinations of symbionts in a vegetatively reproducing lichen, Parmotrema tinctorum, based on ITS rDNA sequences"]<-"rock"
missing$Substrate_Publication[missing$NCBI_Title %in% "Genetic combinations of symbionts in a vegetatively reproducing lichen, Parmotrema tinctorum, based on ITS rDNA sequences"]<-"Ohmura et al. (2008)"
missing$Substrate_Man_Cur[missing$NCBI_Title %in% "Genetic combinations of symbionts in a vegetatively reproducing lichen, Parmotrema tinctorum, based on ITS rDNA sequences"]<-"Yes"


#Onut-Braennstroem et al. (2017) NCBI lists these as being from USA: British Colombia, Radio Tower Mt But coords are 54.3586944 -128.579125 which are in Canada (British Columbia)
accs<-c("KY559225","KY559224","KY559207")
for(x in 1:length(accs)){
	missing$Country_Detailed[missing$Acc_No %in% accs[x]]<-"Canada"
	missing$Country[missing$Acc_No %in% accs[x]]<-"Canada"
	missing$Geo_Man_Cur[missing$Acc_No %in% accs[x]]<-"Yes"
	missing$DB_Notes[missing$Acc_No %in% accs[x]]<-"Changed country from what was listed in NCBI (USA: British Colombia, Radio Tower Mt) as coordinates are in Canada"
	missing$ISO3Code[missing$Acc_No %in% accs[x]]<-"CAN"
	missing$GEO3SubRegion[missing$Acc_No %in% accs[x]]<-"Canada"
	missing$GEO3MajorRegion[missing$Acc_No %in% accs[x]]<-"North America"
}

#Onut-Braennstroem et al. (2018) - can probably georeference, and all are likely on ground

#Opanowicz & Grube (2004) - grows on soil...could probably georeference
missing$Substrate[missing$NCBI_Title %in% "Photobiont genetic variation in Flavocetraria nivalis from Poland (Parmeliaceae, lichenized Ascomycota)"]<-"soil"
missing$Substrate_Publication[missing$NCBI_Title %in% "Photobiont genetic variation in Flavocetraria nivalis from Poland (Parmeliaceae, lichenized Ascomycota)"]<-"Opanowicz & Grube (2004)"
missing$Substrate_Man_Cur[missing$NCBI_Title %in% "Photobiont genetic variation in Flavocetraria nivalis from Poland (Parmeliaceae, lichenized Ascomycota)"]<-"Yes"




#Park et al. (2014)
#KM207206 KoLRI_000978A Heterodermia hypoleuca bark 35°48'54.6" N, 128°06'55.1" E 35.8151667 128.1153056
#KM207207 KoLRI_011648A Myelochroa irrugans rock 34°08'50.4" N, 126°32'90.6" E 34.1473333 126.5585000
#KM207205 KoLRI_007742A Heterodermia diademata rock 34°45'39.1" N, 128°02'55.7" E 34.7608611 128.0488056
#KM207209 KoLRI_007349A Umbilicaria esculenta rock 36°57'51.3" N, 128°30'32.6" E 36.9642500 128.5090556
#KM207208 KoLRI_007637A Punctelia rudecta rock 36°45'00.3" N, 128°15'53.0" E 36.7500833 128.2647222
accs<-c("KM207206","KM207207","KM207205","KM207209","KM207208")
sub<-c("bark","rock","rock","rock","rock")
lat<-c("35.8151667","34.1473333","34.7608611","36.9642500","36.7500833")
long<-c("128.1153056","126.5585000","128.0488056","128.5090556","128.2647222")
for(x in 1:length(accs)){
	missing$Substrate[missing$Acc_No %in% accs[x]]<-sub[x]
	missing$Lat[missing$Acc_No %in% accs[x]]<-lat[x]
	missing$Long[missing$Acc_No %in% accs[x]]<-long[x]	
	missing$LL_Publication[missing$Acc_No %in% accs[x]]<-"Park et al. (2014)"
	missing$LL_Man_Cur[missing$Acc_No %in% accs[x]]<-'Yes'
	missing$Substrate_Man_Cur[missing$Acc_No %in% accs[x]]<-'Yes'
	missing$Substrate_Publication[missing$Acc_No %in% accs[x]]<-"Park et al. (2014)"
}





#Peksa & Skaloud (2008)
#presumably from rock/soil
#could likely georeference




#Perez-Ortega et al. (2012) FEMS - presumably all rock/soil; range of coords given, but cannot link to accessions



#Perez-Ortega et al. (2012) AoB - presumably all rock/soil
accs<-read.csv(file="xxxx/perez-ortega_et_al_2012_cetraria_new_accs_lat.csv",stringsAsFactors=FALSE)
for(x in 1:nrow(accs)){
	if(!is.na(accs$Lat[x])){
		missing$Lat[missing$Acc_No %in% accs$Acc[x]]<-accs$Lat[x]
		missing$Long[missing$Acc_No %in% accs$Acc[x]]<-accs$Long[x]
		missing$LL_Publication[missing$Acc_No %in% accs$Acc[x]]<-"Perez-Ortega et al. (2012) AoB"
		#added this line in on 29 oct 2019 bc didn't note that it was manually scored
		missing$LL_Man_Cur[missing$Acc_No %in% accs$Acc[x]]<-'Yes'
	}
}
#c("GQ375346","GQ375349") changed Kazakhstan longitude to E and positive
#c("GQ375339","GQ375340") changed Turkey longitude to E and positive

#Piercey-Normore & DePriest (2001) - no lat/long or substrate info


#Piercey-Normore (2009) should be able to georeference accessions...and get substrate for some missing substrate info
accs<-read.csv(file="xxxx/piercey-normore_2009.csv",stringsAsFactors=FALSE)
#reduce to those w substrate
nrow(accs)
accs<-accs[!accs$Substrate=="",]
nrow(accs)

for(x in 1:nrow(accs)){
	if(!accs$Substrate[x] %in% ""){
		missing$Substrate[missing$Acc_No %in% accs$Acc[x]]<-accs$Substrate[x]
		missing$Substrate_Publication[missing$Acc_No %in% accs$Acc[x]]<-"Piercey-Normore (2009)"
		missing$Substrate_Man_Cur[missing$Acc_No %in% accs$Acc[x]]<-"Yes"
	}
}

#reduce to those w coords
nrow(accs)
accs<-accs[!is.na(accs$Lat),]
nrow(accs)
for(x in 1:nrow(accs)){
	missing$Lat[missing$Acc_No %in% accs$Acc[x]]<-accs$Lat[x]
	missing$Long[missing$Acc_No %in% accs$Acc[x]]<-accs$Long[x]
	missing$LL_Publication[missing$Acc_No %in% accs$Acc[x]]<-"Piercey-Normore (2009), Piercey-Normore (2006)"
	missing$LL_Man_Cur[missing$Acc_No %in% accs$Acc[x]]<-"Yes"
}


#Piercey-Normore (2006) - corticolous, but tough to link accessions to sites
accs<-c("DQ086105","DQ086104","DQ086108","DQ086103","DQ086106","DQ086107","DQ086109")
for(x in 1:length(accs)){
	missing$Substrate[missing$Acc_No %in% accs[x]]<-"bark"
	missing$Substrate_Publication[missing$Acc_No %in% accs[x]]<-"Piercey-Normore (2006)"
	missing$Substrate_Man_Cur[missing$Acc_No %in% accs[x]]<-"Yes"
}




#Pinar et al. (2015) from Da Vinci self-portrait drawing "The drawing is housed at the Royal Library in Turin (‘Biblioteca Reale’, Italy) and is strongly affected by foxing spots (Fig. 1)."
#Finally, in 2012 the portrait was transferred from Turin to the ICRCPAL: ‘Istituto Centrale per il Restauro e la Conservazione del Patrimonio Archivistico e librario’, Rome, to be subjected to a thorough and complete series of inves- tigations of its biological, chemical and physical properties, for the first time. 
#Could probably georeference lat long
accs<-"KP828166"
for(x in 1:length(accs)){
	missing$Substrate[missing$Acc_No %in% accs[x]]<-"Drawing"
	missing$Substrate_Publication[missing$Acc_No %in% accs[x]]<-"Pinar et al. (2015)"
	missing$Substrate_Man_Cur[missing$Acc_No %in% accs[x]]<-"Yes"
}


#Do not know of Pogoda publication

#Rafat et al. (2015) could get lat long for some but not all...was able to get host info, though
#some accessions have info for Buckley et al. paper

accs<-read.csv(file="xxxx/rafat_et_al_2015.csv",stringsAsFactors=FALSE)
#first add in host info
for(x in 1:nrow(accs)){
	missing$Host_Genus[missing$Acc_No %in% accs$Acc[x]]<-accs$Genus[x]
	missing$Host_Species[missing$Acc_No %in% accs$Acc[x]]<-accs$Species[x]
	missing$Host[missing$Acc_No %in% accs$Acc[x]]<-paste(accs$Genus[x],accs$Species[x],sep="_")
	missing$Host_Detailed[missing$Acc_No %in% accs$Acc[x]]<-paste(accs$Genus[x],accs$Species[x],sep=" ")
	missing$Host_Man_Cur[missing$Acc_No %in% accs$Acc[x]]<-"Yes"
	missing$Host_Family[missing$Acc_No %in% accs$Acc[x]]<-"Parmeliaceae"
	missing$Host_Order[missing$Acc_No %in% accs$Acc[x]]<-"Lecanorales"
	missing$Host_Class[missing$Acc_No %in% accs$Acc[x]]<-"Lecanoromycetes"
	missing$Host_Man_Cur[missing$Acc_No %in% accs$Acc[x]]<-"Yes"
	missing$Host_Publication[missing$Acc_No %in% accs$Acc[x]]<-missing$NCBI_First_Author[missing$Acc_No %in% accs$Acc[x]]
}

#now add lat long in for those with ll
nrow(accs)
accs<-accs[!is.na(accs$Lat),]
nrow(accs)
for(x in 1:nrow(accs)){
	missing$Lat[missing$Acc_No %in% accs$Acc[x]]<-accs$Lat[x]
	missing$Long[missing$Acc_No %in% accs$Acc[x]]<-accs$Long[x]
	missing$LL_Publication[missing$Acc_No %in% accs$Acc[x]]<-"Rafat et al. (2015)"
	missing$LL_Man_Cur[missing$Acc_No %in% accs$Acc[x]]<-"Yes"
}


#Romeike et al. (2002) - ll for some and others are challenging - could geolocate them
#keep those w clean ll
accs<-read.csv(file="xxxx/romeike_et_al_2002.csv",stringsAsFactors=FALSE)
nrow(accs)
accs<-accs[!is.na(accs$Lat),]
nrow(accs)
for(x in 1:nrow(accs)){
	if(!is.na(accs$Lat[x])){
		missing$Lat[missing$Acc_No %in% accs$Acc[x]]<-accs$Lat[x]
		missing$Long[missing$Acc_No %in% accs$Acc[x]]<-accs$Long[x]
		missing$LL_Publication[missing$Acc_No %in% accs$Acc[x]]<-"Romeike et al. (2002)"
		missing$LL_Man_Cur[missing$Acc_No %in% accs$Acc[x]]<-"Yes"
	}
}



#Ruprecht et al. (2012) Saxicolous, and latitude provided, but longitude excluded...change all to saxicolous
missing$Substrate[missing$NCBI_Title %in% "Genetic diversity of photobionts in Antarctic lecideoid lichens from an ecological view point"]<-"rock"
missing$Substrate_Publication[missing$NCBI_Title %in% "Genetic diversity of photobionts in Antarctic lecideoid lichens from an ecological view point"]<-"Ruprecht et al. (2012)"
missing$Substrate_Man_Cur[missing$NCBI_Title %in% "Genetic diversity of photobionts in Antarctic lecideoid lichens from an ecological view point"]<-"Yes"



#Ruprecht et al. (2016) saxicolous; lat long given for some...gave all on I Navarino the same
missing$Substrate[missing$NCBI_Title %in% "Porpidia navarina, a new endemic species from Isla Navarino - Southern Tierra del Fuego (Chile)"]<-"rock"
missing$Substrate_Publication[missing$NCBI_Title %in% "Porpidia navarina, a new endemic species from Isla Navarino - Southern Tierra del Fuego (Chile)"]<-"Ruprecht et al. (2016)"
missing$Substrate_Man_Cur[missing$NCBI_Title %in% "Porpidia navarina, a new endemic species from Isla Navarino - Southern Tierra del Fuego (Chile)"]<-"Yes"

accs<-read.csv(file="xxxx/ruprecht_et_al_2016.csv",stringsAsFactors=FALSE)
nrow(accs)
#drop those w/o lat long
accs<-accs[!is.na(accs$Lat),]
#drop those w/o algal seqs
accs<-accs[any(!accs$Tacc %in% "" & !accs$Aacc %in% ""),]
nrow(accs)

nrow(accs)
for(x in 1:nrow(accs)){
	if(!accs$Tacc[x] %in% ""){
		missing$Lat[missing$Acc_No %in% accs$Tacc[x]]<-accs$Lat[x]
		missing$Long[missing$Acc_No %in% accs$Tacc[x]]<-accs$Long[x]
		missing$LL_Publication[missing$Acc_No %in% accs$Tacc[x]]<-"Ruprecht et al. (2016)"
		missing$LL_Man_Cur[missing$Acc_No %in% accs$Tacc[x]]<-"Yes"
	}
	if(!accs$Aacc[x] %in% ""){
		missing$Lat[missing$Acc_No %in% accs$Aacc[x]]<-accs$Lat[x]
		missing$Long[missing$Acc_No %in% accs$Aacc[x]]<-accs$Long[x]
		missing$LL_Publication[missing$Acc_No %in% accs$Aacc[x]]<-"Ruprecht et al. (2016)"
		missing$LL_Man_Cur[missing$Acc_No %in% accs$Aacc[x]]<-"Yes"
	}
}



#Sadowska-Des et al. (2014)
#Presumably saxicolous
accs<-read.csv(file="xxxx/sadowska_des_et_al_2014_SI.csv",stringsAsFactors=FALSE)
for(x in 1:nrow(accs)){
	missing$Lat[missing$Acc_No %in% accs$Acc[x]]<-accs$Lat[x]
	missing$Long[missing$Acc_No %in% accs$Acc[x]]<-accs$Long[x]
	missing$LL_Publication[missing$Acc_No %in% accs$Acc[x]]<-"Sadowska-Des et al. (2014)"
	missing$LL_Man_Cur[missing$Acc_No %in% accs$Acc[x]]<-"Yes"
}


#Schmull et al. (2011) - no coords given...can surmise substrate from their list of substrates individual fungal species occur on (from literature, not the eactual specimen)

accs<-c("HQ667317","HQ667316","HQ667315","HQ667314","HQ667313","HQ667312","HQ667311","HQ667310","HQ667309","HQ667308","HQ667307","HQ667306")
sub<-c("corticolous","lignicolous","corticolous and lignicolous","corticolous","muscicolous","corticolous","corticolous","corticolous","corticolous (lignicolous)","corticolous","corticolous","corticolous")

for(x in 1:length(accs)){
	missing$Substrate[missing$Acc_No %in% accs[x]]<-sub[x]
	missing$Substrate_Publication[missing$Acc_No %in% accs[x]]<-"Schmull et al. (2011)"
	missing$Substrate_Man_Cur[missing$Acc_No %in% accs[x]]<-"Yes"
}

#Lecanora fuscescens
#corticolous
#HQ667317

#Lecidea hercynica
#lignicolous
#HQ667316

#Lecidea leprarioides
#corticolous and lignicolous
#HQ667315

#Lecidea nylanderi
#corticolous
#HQ667314

#Lecidea polytrichina
#muscicolous
#HQ667313

#Lecidea pullata
#corticolous
#HQ667312

#Lecidea roseotincta
#corticolous
#HQ667311

#Lecidea roseotincta
#corticolous
#HQ667310

#Lecidea turgidula
#corticolous (lignicolous)
#HQ667309

#Schaereria dolodes
#corticolous
#HQ667308

#Schaereria dolodes
#corticolous
#HQ667307

#Schaereria dolodes
#corticolous
#HQ667306




#Seifried
#presumably ground



#Siddique - Arocena et al. (2007) Coords not provided, but could georeference from map provided.
#rock
#not free-living, but matching up identically with hosts does not make sense either (Massalongia, Sticta...)
#already included in NCBI
#accs<-c("AY928201","AY928200","AY928199")
#for(x in 1:length(accs)){
#	missing$Substrate[missing$Acc_No %in% accs[x]]<-"rock"
#}




#Skaloud et al. (2018)
#all on rock
#Los Cancajos 28°38´49"N 17°45´41"W 28.6469444 -017.7613889
#Buenavista 28°21´37"N 16°50´41"W 28.3602778 -016.8447222
#Jameos del Agua 29°09´45"N 13°25´53"W 29.1625000 -013.4313889

accs<-c("MF192896","MF192892","MF192893","MF192891","MF192894","MF192895","MF287096")
sites<-c("Los Cancajos","Buenavista","Buenavista","Jameos del Agua","Buenavista","Los Cancajos","Los Cancajos")
lat<-c("28.6469444","28.3602778","28.3602778","29.1625000","28.3602778","28.6469444","28.6469444")
long<-c("-017.7613889","-016.8447222","-016.8447222","-013.4313889","-016.8447222","-017.7613889","-017.7613889")
for(x in 1:length(accs)){
	missing$Substrate[missing$Acc_No %in% accs[x]]<-"rock"
	missing$Substrate_Man_Cur[missing$Acc_No %in% accs[x]]<-"Yes"
	missing$Substrate_Publication[missing$Acc_No %in% accs[x]]<-"Skaloud et al. (2018)"	
	missing$Lat[missing$Acc_No %in% accs[x]]<-lat[x]
	missing$Long[missing$Acc_No %in% accs[x]]<-long[x]
	missing$LL_Publication[missing$Acc_No %in% accs[x]]<-"Skaloud et al. (2018)"
	missing$LL_Man_Cur[missing$Acc_No %in% accs[x]]<-"Yes"
}




#Tang - cannot find a reference




#Vargas-Castillo & Beck (2012) - can get substrate info from the mycobiont substrate discussed in results
accs<-read.csv(file="xxxx/vargas_castillo_&_beck_2012.csv",stringsAsFactors=FALSE)
for(x in 1:nrow(accs)){
	missing$Substrate[missing$Acc_No %in% accs$Aacc[x]]<-accs$Substrate[x]
	missing$Substrate_Man_Cur[missing$Acc_No %in% accs$Aacc[x]]<-"Yes"
	missing$Substrate_Publication[missing$Acc_No %in% accs$Aacc[x]]<-"Vargas-Castillo & Beck (2012)"	
}



#Voytsekhovich (2015) - has some new type cultures deposited in UTEX...make sure other publications have new type cultures added
accs<-read.csv(file="xxxx/voytsekhovich2015_table.csv",stringsAsFactors=FALSE)
for(x in 1:nrow(accs)){
	missing$Substrate[missing$Acc_No %in% accs$Acc[x]]<-accs$Substrate[x]
	missing$Substrate_Man_Cur[missing$Acc_No %in% accs$Acc[x]]<-"Yes"
	missing$Substrate_Publication[missing$Acc_No %in% accs$Acc[x]]<-"Voytsekhovich & Beck (2015)"	
}

#then restrict to type cultures which are the only w solid lat/long's
nrow(accs)
accs<-accs[!is.na(accs$Lat),]
nrow(accs)
for(x in 1:nrow(accs)){
	missing$Lat[missing$Acc_No %in% accs$Acc[x]]<-accs$Lat[x]
	missing$Long[missing$Acc_No %in% accs$Acc[x]]<-accs$Long[x]
	missing$LL_Publication[missing$Acc_No %in% accs$Acc[x]]<-"Voytsekhovich & Beck (2015)"
	missing$LL_Man_Cur[missing$Acc_No %in% accs$Acc[x]]<-"Yes"
	missing$Strain[missing$Acc_No %in% accs$Acc[x]]<-accs$Type_Culture[x]
}


#Wedin et al. (2015)
#Gynge Alvar, at 56°32'31.7"N, 16°28'42.2"E 56.5421389 016.4783889
#‘Ruine Homburg’ at Gössenheim, Bavaria, Germany, 50°01'34.8"N, 9°47'56.1"E 50.0263333 009.7989167
#Dröstorp, at 56°35'29.7"N, 16°35'01.1"E

#ground
#All sites are on limestone bedrock with a thin gravel layer on top of the bedrock, and the vegetation is composed of a colourful lichen-dominated BSC including Cladonia spp., Diploschistes muscorum, Thamnolia vermicularis, Squamarina spp., Fulgensia spp., Psora decipiens, Toninia spp. and cyanobacteria (Fig. S5). Here, the dominating Cladonia species in the BSC is Cladonia symphycarpa, and it is possible to follow the transition from C. symphycarpa to D. muscorum (Fig. 1A–D) at all sites.

accs<-c("KT215316","KT215318","KT215313","KT215317","KT215314","KT215315")
lat<-c("50.0263333","50.0263333","56.5421389","50.0263333","56.5421389","56.5421389")
long<-c("009.7989167","009.7989167","016.4783889","009.7989167","016.4783889","016.4783889")
for(x in 1:length(accs)){
	missing$Substrate[missing$Acc_No %in% accs[x]]<-"ground"
	missing$Substrate_Man_Cur[missing$Acc_No %in% accs[x]]<-"Yes"
	missing$Substrate_Publication[missing$Acc_No %in% accs[x]]<-"Wedin et al. (2015)"	
	missing$Lat[missing$Acc_No %in% accs[x]]<-lat[x]
	missing$Long[missing$Acc_No %in% accs[x]]<-long[x]
	missing$LL_Publication[missing$Acc_No %in% accs[x]]<-"Wedin et al. (2015)"
	missing$LL_Man_Cur[missing$Acc_No %in% accs[x]]<-"Yes"
}



#Wornik & Grube (2009)
#Substrate provided in NCBI
#bark, but lat long not given
#missing$Substrate[missing$NCBI_Title %in% "Joint dispersal does not imply maintenance of partnerships in lichen symbioses"]<-"bark"



#Xu - cannot find paper


#Zhang et al. (2015)
#SI gives refined lat long and substrate info, but cannot link those to Sanger reads in GenBank (NGS sample codes provided)
#presumably all ground


#cannot find Zhou paper






#Muggia et al. (2014) table S2...if no substrate info for taxa already, then add in from here.
#restrict to those w substrate info
accs<-read.csv(file="xxxx/muggia_et_al_2014_SI2.csv",stringsAsFactors=FALSE)
nrow(accs)
accs<-accs[!accs$Substrate %in% "",]
nrow(accs)
for(x in 1:nrow(accs)){
	if(is.na(missing$Substrate[missing$Acc_No %in% accs$Acc[x]])){
		missing$Substrate[missing$Acc_No %in% accs$Acc[x]]<-accs$Substrate[x]
		missing$Substrate_Man_Cur[missing$Acc_No %in% accs$Acc[x]]<-"Yes"
		missing$Substrate_Publication[missing$Acc_No %in% accs$Acc[x]]<-"Muggia et al. (2014)"	
	
	}
}




#take care of some geographic and host stuff

#SVALBARD SHOULD BE CODED AS NORWAY FROM THE START
missing$Country_Detailed[missing$Country_GB_Mod %in% "Svalbard"]<-"Norway"
missing$Country[missing$Country_GB_Mod %in% "Svalbard"]<-"Norway"
missing$Geo_Man_Cur[missing$Country_GB_Mod %in% "Svalbard"]<-"Yes"
missing$ISO3Code[missing$Country_GB_Mod %in% "Svalbard"]<-"NOR"
missing$GEO3SubRegion[missing$Country_GB_Mod %in% "Svalbard"]<-"Western Europe"
missing$GEO3MajorRegion[missing$Country_GB_Mod %in% "Svalbard"]<-"Europe"
missing$Country_GB_Mod[missing$Country_GB_Mod %in% "Svalbard"]<-"Norway"

#Issue with Kosovo - have one sample. ISOA3 code I think is UNK, but not in world map used here...so, change to Serbia
missing$ISO3Code[missing$Country_GB_Mod %in% "Kosovo"]<-"UNK"
missing$GEO3SubRegion[missing$Country_GB_Mod %in% "Kosovo"]<-"Central Europe"
missing$GEO3MajorRegion[missing$Country_GB_Mod %in% "Kosovo"]<-"Europe"

#change some GBR (UK) to FLK - should have done earlier, but overlooked.  Checked publications for others and all are with North Atlantic UK
accs<-c("GQ375360","GQ375358","GQ375356","GQ375354","GQ375361","GQ375359","GQ375357","GQ375355")
for(x in 1:length(accs)){
	missing$Country_Detailed[missing$Acc_No %in% accs[x]]<-"Falkland Islands"
	missing$Country[missing$Acc_No %in% accs[x]]<-"Falkland Islands"
	missing$Geo_Man_Cur[missing$Acc_No %in% accs[x]]<-"Yes"
	missing$DB_Notes[missing$Acc_No %in% accs[x]]<-"Listed as from UK: Falkland in NCBI, and changed to Falklands"
	missing$ISO3Code[missing$Acc_No %in% accs[x]]<-"FLK"
	missing$GEO3SubRegion[missing$Acc_No %in% accs[x]]<-"South America"
	missing$GEO3MajorRegion[missing$Acc_No %in% accs[x]]<-"Latin America and the Caribbean"
}

# a few from Werth & Sork (2014) have lat/long, but were not discussed in paper, so country is missing.  Add in country from lat/long
accs<-c("KF556649","KF556650")
for(x in 1:length(accs)){
	missing$Country_Detailed[missing$Acc_No %in% accs[x]]<-"USA"
	missing$Country[missing$Acc_No %in% accs[x]]<-"USA"
	missing$Geo_Man_Cur[missing$Acc_No %in% accs[x]]<-"Yes"
	missing$DB_Notes[missing$Acc_No %in% accs[x]]<-"Country not listed in Werth & Sork, but lat/long found and country ascertained"
	missing$ISO3Code[missing$Acc_No %in% accs[x]]<-"USA"
	missing$GEO3SubRegion[missing$Acc_No %in% accs[x]]<-"US"
	missing$GEO3MajorRegion[missing$Acc_No %in% accs[x]]<-"North America"
}

accs<-"KF556651"
for(x in 1:length(accs)){
	missing$Country_Detailed[missing$Acc_No %in% accs[x]]<-"Mexico"
	missing$Country[missing$Acc_No %in% accs[x]]<-"Mexico"
	missing$Geo_Man_Cur[missing$Acc_No %in% accs[x]]<-"Yes"
	missing$DB_Notes[missing$Acc_No %in% accs[x]]<-"Country not listed in Werth & Sork, but lat/long found and country ascertained"
	missing$ISO3Code[missing$Acc_No %in% accs[x]]<-"MEX"
	missing$GEO3SubRegion[missing$Acc_No %in% accs[x]]<-"Meso-America"
	missing$GEO3MajorRegion[missing$Acc_No %in% accs[x]]<-"Latin America and the Caribbean"
}


missing$Host_Class[missing$Host_Family %in% "Parmeliaceae\tLecanorales\tLecanoromycetes"]<-"Lecanoromycetes"
missing$Host_Order[missing$Host_Family %in% "Parmeliaceae\tLecanorales\tLecanoromycetes"]<-"Lecanorales"
missing$Host_Family[missing$Host_Family %in% "Parmeliaceae\tLecanorales\tLecanoromycetes"]<-"Parmeliaceae"

missing$Host_Order[missing$Host_Genus %in% "Diploschistes"]<-"Ostropales"

missing$Host_Class[missing$Host_Order %in% "Candelariales"]<-"Incertae_Sedis"

missing$Host_Family[missing$Host_Family %in% ""]<-"None"
missing$Host_Order[missing$Host_Order %in% ""]<-"None"
missing$Host_Class[missing$Host_Class %in% ""]<-"None"

missing$Host_GenusSp<-paste(missing$Host_Genus,missing$Host_Species,sep="_")
missing$Host_GenusSp[missing$Host_GenusSp %in% "None_None"]<-"None"
missing$Host_GenusSp[missing$Host_GenusSp %in% "Non-lichenized_sp"]<-"None"
missing$Host_GenusSp[missing$Host_GenusSp %in% "unidentified_lichen"]<-"None"
missing$Host_GenusSp[missing$Host_GenusSp %in% "NA_NA"]<-"None"
missing$Host_GenusSp[is.na(missing$Host_GenusSp)]<-"None"

missing$Host_GenusSp<-gsub("_aff_","_",missing$Host_GenusSp)
missing$Host_GenusSp<-gsub("_cf_","_",missing$Host_GenusSp)
missing$Host_GenusSp<-gsub("_cf ","_",missing$Host_GenusSp)

#Caloplaca squamulosa is Caloplaca squamosa, which is now Squamulea_squamosa.
missing$Host_Genus[missing$Host_GenusSp %in% "Caloplaca_squamulosa"]<-"Squamulea"
missing$Host_Species[missing$Host_GenusSp %in% "Caloplaca_squamulosa"]<-"squamosa"
missing$Host_GenusSp[missing$Host_GenusSp %in% "Caloplaca_squamulosa"]<-"Squamulea_squamosa"

missing$Host_Genus[missing$Host_GenusSp %in% "Xanthoria_borealis"]<-"Xanthomendoza"
missing$Host_GenusSp[missing$Host_GenusSp %in% "Xanthoria_borealis"]<-"Xanthomendoza_borealis"

missing$Host_Genus[missing$Host_GenusSp %in% "Xanthoria_fallax"]<-"Xanthomendoza"
missing$Host_GenusSp[missing$Host_GenusSp %in% "Xanthoria_fallax"]<-"Xanthomendoza_fallax"

missing$Host_Genus[missing$Host_GenusSp %in% "Xanthoria_ligulata"]<-"Dufourea"
missing$Host_GenusSp[missing$Host_GenusSp %in% "Xanthoria_ligulata"]<-"Dufourea_ligulata"

missing$Host_Genus[missing$Host_GenusSp %in% "Xanthoria_polycarpa"]<-"Polycauliona"
missing$Host_GenusSp[missing$Host_GenusSp %in% "Xanthoria_polycarpa"]<-"Polycauliona_polycarpa"

#correct these
missing$Host_Genus[missing$Host_GenusSp %in% "Xanthoria_candelaria"]<-"Polycauliona"
missing$Host_GenusSp[missing$Host_GenusSp %in% "Xanthoria_candelaria"]<-"Polycauliona_candelaria"
missing$Host_Genus[missing$Host_GenusSp %in% "Xanthoria_elegans"]<-"Rusavskia"
missing$Host_GenusSp[missing$Host_GenusSp %in% "Xanthoria_elegans"]<-"Rusavskia_elegans"
missing$Host_Genus[missing$Host_GenusSp %in% "Xanthoria_tenax"]<-"Polycauliona"
missing$Host_GenusSp[missing$Host_GenusSp %in% "Xanthoria_tenax"]<-"Polycauliona_tenax"
missing$Host_Genus[missing$Host_GenusSp %in% "Caloplaca_erodens"]<-"Pyrenodesmia"
missing$Host_GenusSp[missing$Host_GenusSp %in% "Caloplaca_erodens"]<-"Pyrenodesmia_erodens"
missing$Host_Genus[missing$Host_GenusSp %in% "Caloplaca_orthoclada"]<-"Follmannia"
missing$Host_GenusSp[missing$Host_GenusSp %in% "Caloplaca_orthoclada"]<-"Follmannia_orthoclada"
missing$Host_Genus[missing$Host_GenusSp %in% "Lecanora_muralis"]<-"Protoparmeliopsis"
missing$Host_GenusSp[missing$Host_GenusSp %in% "Lecanora_muralis"]<-"Protoparmeliopsis_muralis"
missing$Host_Genus[missing$Host_GenusSp %in% "Buellia_punctata"]<-"Amandinea"
missing$Host_GenusSp[missing$Host_GenusSp %in% "Buellia_punctata"]<-"Amandinea_punctata"
missing$Host_Genus[missing$Host_GenusSp %in% "Caloplaca_ammiospila"]<-"Blastenia"
missing$Host_GenusSp[missing$Host_GenusSp %in% "Caloplaca_ammiospila"]<-"Blastenia_ammiospila"
missing$Host_Genus[missing$Host_GenusSp %in% "Caloplaca_aurantia"]<-"Variospora"
missing$Host_GenusSp[missing$Host_GenusSp %in% "Caloplaca_aurantia"]<-"Variospora_aurantia"
missing$Host_Genus[missing$Host_GenusSp %in% "Caloplaca_crenulatella"]<-"Xanthocarpia"
missing$Host_GenusSp[missing$Host_GenusSp %in% "Caloplaca_crenulatella"]<-"Xanthocarpia_crenulatella"
#Might become Pyrenodesmia or something else (see Arup et al. 2013), but seems still Caloplaca for now (corrected spelling)
missing$Host_Genus[missing$Host_GenusSp %in% "Caloplaca_erythrocarpia"]<-"Caloplaca"
missing$Host_GenusSp[missing$Host_GenusSp %in% "Caloplaca_erythrocarpia"]<-"Caloplaca_erythrocarpa"
#see Kondratyuk et al. (2017)
missing$Host_Genus[missing$Host_GenusSp %in% "Caloplaca_fernandeziana"]<-"Teuvoahtiana"
missing$Host_GenusSp[missing$Host_GenusSp %in% "Caloplaca_fernandeziana"]<-"Teuvoahtiana_fernandeziana"
missing$Host_Genus[missing$Host_GenusSp %in% "Melanelia_glabra"]<-"Melanelixia"
missing$Host_GenusSp[missing$Host_GenusSp %in% "Melanelia_glabra"]<-"Melanelixia_glabra"
#see Blanco et al.
missing$Host_Genus[missing$Host_GenusSp %in% "Melanelia_huei"]<-"Melanelixia"
missing$Host_GenusSp[missing$Host_GenusSp %in% "Melanelia_huei"]<-"Melanelixia_huei"
#see Blanco et al.
missing$Host_Genus[missing$Host_GenusSp %in% "Neofuscelia_pulla"]<-"Xanthoparmelia"
missing$Host_GenusSp[missing$Host_GenusSp %in% "Neofuscelia_pulla"]<-"Xanthoparmelia_pulla"
missing$Host_Genus[missing$Host_GenusSp %in% "Physcia_pulverulenta"]<-"Physconia"
missing$Host_GenusSp[missing$Host_GenusSp %in% "Physcia_pulverulenta"]<-"Physconia_pulverulenta"
missing$Host_Species[missing$Host_GenusSp %in% "Pseudevernia_cladoniae"]<-"cladonia"
missing$Host_GenusSp[missing$Host_GenusSp %in% "Pseudevernia_cladoniae"]<-"Pseudevernia_cladonia"
missing$Host_Species[missing$Host_GenusSp %in% "Ramalina_farinaceae"]<-"farinacea"
missing$Host_GenusSp[missing$Host_GenusSp %in% "Ramalina_farinaceae"]<-"Ramalina_farinacea"
missing$Host_Genus[missing$Host_GenusSp %in% "Teloschistes_chrysophthalmus"]<-"Niorma"
missing$Host_Species[missing$Host_GenusSp %in% "Teloschistes_chrysophthalmus"]<-"chrysophthalma"
missing$Host_GenusSp[missing$Host_GenusSp %in% "Teloschistes_chrysophthalmus"]<-"Niorma_chrysophthalma"
missing$Host_Genus[missing$Host_GenusSp %in% "Xanthoria_capensis"]<-"Dufourea"
missing$Host_GenusSp[missing$Host_GenusSp %in% "Xanthoria_capensis"]<-"Dufourea_capensis"
missing$Host_Genus[missing$Host_GenusSp %in% "Xanthoria_hasseana"]<-"Xanthomendoza"
missing$Host_GenusSp[missing$Host_GenusSp %in% "Xanthoria_hasseana"]<-"Xanthomendoza_hasseana"
missing$Host_Genus[missing$Host_GenusSp %in% "Xanthoria_karrooensis"]<-"Dufourea"
missing$Host_GenusSp[missing$Host_GenusSp %in% "Xanthoria_karrooensis"]<-"Dufourea_karrooensis"
missing$Host_Genus[missing$Host_GenusSp %in% "Xanthoria_sorediata"]<-"Rusavskia"
missing$Host_GenusSp[missing$Host_GenusSp %in% "Xanthoria_sorediata"]<-"Rusavskia_sorediata"
missing$Host_Genus[missing$Host_GenusSp %in% "Xanthoria_turbinata"]<-"Dufourea"
missing$Host_GenusSp[missing$Host_GenusSp %in% "Xanthoria_turbinata"]<-"Dufourea_turbinata"


missing$Full_Host_GenusSp<-NA
missing$Full_Host_GenusSp[!grepl("_sp",missing$Host_GenusSp) & !grepl("None",missing$Host_GenusSp)]<-missing$Host_GenusSp[!grepl("_sp",missing$Host_GenusSp) & !grepl("None",missing$Host_GenusSp)]
table(missing$Full_Host_GenusSp)
sum(table(missing$Full_Host_GenusSp))
length(table(missing$Full_Host_GenusSp))
sum(table(missing$Full_Host_GenusSp)>0)
table(missing$Full_Host_GenusSp)[table(missing$Full_Host_GenusSp)>0]



#make single columns for Lat and Long and note where coords from
missing$LatC<-NA
missing$LongC<-NA

#first add info from GenBank to columns
missing$LatC<-as.numeric(missing$NCBI_Lat)
missing$LongC<-as.numeric(missing$NCBI_Lon)

#then add manually curated info
missing$LatC[is.na(missing$LatC) & !is.na(missing$Lat) & !missing$Lat %in% "NA"]<-as.numeric(missing$Lat[is.na(missing$LatC) & !is.na(missing$Lat) & !missing$Lat %in% "NA"])
missing$LongC[is.na(missing$LongC) & !is.na(missing$Long) & !missing$Long %in% "NA"]<-as.numeric(missing$Long[is.na(missing$LongC) & !is.na(missing$Long) & !missing$Long %in% "NA"])

write.csv(missing,file="/xxxx/MASTER_accession_list_INFOADDED.csv",row.names=FALSE)



#use rangBuilder to check they are in right place more or less
require(rangeBuilder)

missing<-read.csv(file="/xxxx/MASTER_accession_list_INFOADDED.csv",stringsAsFactors=FALSE)
missing$LongC<-as.numeric(missing$LongC)
missing$LatC<-as.numeric(missing$LatC)

#Make empty column for closest country
missing$closest<-NA

#find closest country and take first one if multiple matched
for(i in 1:nrow(missing)){
	if(!is.na(missing$LongC[i])){
		missing$closest[i]<-closestCountry(missing[i,c("LongC","LatC")])[1]
	}
}

temp<-missing[,c("Country","closest")]
#remove those w/o closest
nrow(temp)
temp<-temp[!is.na(temp$closest),]
nrow(temp)

#change Country to uppercase
temp$Country<-toupper(temp$Country)

unique(temp[temp$Country!=temp$closest,])

#missing$Acc_No[missing$Country %in% "Germany" & missing$closest %in% "ITALY"]
#missing$Acc_No[missing$Country %in% "Germany" & missing$closest %in% "FINLAND"]
#missing$Acc_No[missing$Country %in% "Austria" & missing$closest %in% "FRANCE"]
#missing$Acc_No[missing$Country %in% "Ireland" & missing$closest %in% "DENMARK"]

#add some plate and biome data
library(rgdal)
library(dismo)
library(rgeos)

#add plate info
tectonics<-readOGR(dsn="/xxxx/tectonicplates-master",layer="PB2002_plates_MODIFIED_Third")
unique(tectonics$PlateName)
# subset to take out NAs
missingll<-missing[!is.na(missing$LatC),]
# create numeric cells
missingll$LongC<-as.numeric(missingll$LongC)
missingll$LatC<-as.numeric(missingll$LatC)

temp<-extract(x=tectonics,y=cbind(missingll$LongC[1:nrow(missingll)],missingll$LatC[1:nrow(missingll)]),method="simple")

missingll[1:nrow(missingll),"Code"]<-temp[,"Code"]
missingll$Code<-as.character(missingll$Code)
missingll[1:nrow(missingll),"PlateName"]<-temp[,"PlateName"]
missingll$PlateName<-as.character(missingll$PlateName)

table(missingll$PlateName)



#add biome info
biomes<-readOGR(dsn='/xxxx/official',layer='wwf_terr_ecos')
#98 and 99 represent water or other things so replace with NA
biomes$BIOME[biomes$BIOME==98]<-NA
biomes$BIOME[biomes$BIOME==99]<-NA
unique(biomes$BIOME)

temp.b<-extract(x=biomes,y=cbind(missingll$LongC[1:nrow(missingll)],missingll$LatC[1:nrow(missingll)]),method="simple")
missingll[1:nrow(missingll),"Biome"]<-temp.b[,"BIOME"]
missingll[1:nrow(missingll),"Realm"]<-temp.b[,"REALM"]

table(missingll$Biome)
table(missingll$Realm)


#read in merraclim files
mc<-1:19
require(raster)

for(i in 1:length(mc)) {
  rastFile<-paste('/xxxx/merraclim/2_5_mean_mean80sto00s_bio',mc[i],'.tif',sep='')
  rast<-raster(rastFile,values=TRUE)
  temp<-extract(x=rast,y=cbind(missingll$LongC[1:nrow(missingll)],missingll$LatC[1:nrow(missingll)]),method='simple')
  missingll[1:nrow(missingll),paste('bio',mc[i],sep='')]<-temp
  rastFile<-NULL
  rast<-NULL
  temp<-NULL
}


#add "Code","PlateName","Biome","Realm",paste('bio',mc,sep='') to missing and add values from missingll into missing
newcols<-c("Code","PlateName","Biome","Realm",paste('bio',mc,sep=''))
missing[newcols,]<-NA
for(x in 1:nrow(missingll)){
	for(z in 1:length(newcols)){
		missing[missing$Acc_No %in% missingll$Acc_No[x],newcols[z]]<-missingll[x,newcols[z]]
	}
}




#essentially taken from here: http://stackoverflow.com/questions/27338512/color-countries-on-world-map-based-on-iso3-codes-in-r-using-ggplot
library(maptools)
library(mapproj)
library(rgeos)
library(rgdal)
library(ggplot2)
library(jsonlite)
library(RCurl)

#French Guiana, Greenland and the Falkland Islands, which all have separate ISO3A3 codes, were treated as separate countries

#downloaded ne_10m_admin_0_map_units
#from http://www.naturalearthdata.com/downloads/10m-cultural-vectors/10m-admin-0-details/

#wworld<-readOGR(dsn="/xxxx/ne_10m_admin_0_map_units",layer="ne_10m_admin_0_map_units")
world<-readOGR(dsn="/xxxx/ne_110m_admin_0_countries",layer="ne_110m_admin_0_countries")
world<-spTransform(world,CRS("+proj=wintri"))
names(world)
unique(world$continent)

#should be Russia (RUS)
world$iso_a3[136]
#change to part of Asia instead of Europe
world$continent[136]<-"Asia"

#should be Trinidad & Tobago (TTO)
world$iso_a3[161]
#change to part of South America instead of North America
world$continent[161]<-"South America"
#make a table by ISO3 code for samples


#Issue with Kosovo - have one sample. ISOA3 code I think is UNK, but not in world map used here...so, change to Serbia
world$admin[89]
world$country[89]
world$continent[89]
world$iso_a3[89]
countries<-as.character(world$iso_a3)
countries
countries[89]
countries[89]<-"UNK"
countries
new.levels<-c(levels(world$iso_a3)[1:89],"UNK",levels(world$iso_a3)[90:175])
levels(world$iso_a3)
world$iso_a3
world$iso_a3<-factor(countries)
world$iso_a3

missing$Continent<-NA
for(j in 1:nrow(missing)){
	if(!is.na(missing$ISO3Code[j])){
		missing$Continent[j]<-as.character(world$continent[world$iso_a3 %in% missing$ISO3Code[j]])
		if(!is.na(missing$ISO3Code[j])){
			#French Guiana coded as Europe (actually, as France)
			if(missing$ISO3Code[j] %in% "GUF"){
				missing$Continent[j]<-"South America"
			}
			##Trinidad and Tobago coded as North America
			#if(me$ISO3Code[j] %in% "TTO"){
			#	me$Continent[j]<-"South America"
			#}	
			##Russia coded as Europe
			#if(me$ISO3Code[j] %in% "RUS"){
			#	me$Continent[j]<-"Asia"
			#}		
		}
	}
}


write.csv(missing,file="/xxxx/MASTER_accession_list_INFOADDED.csv",row.names=FALSE)







#4 pairs of taxa were basically identical but differed the presence or absence of a large indel that pushed their divergence level beyond 2.5%.
#OTU A_97.5_9 uncultured_Trebouxia_photobiont_HQ026187 lacks indel, 77 seqs
#OTU A_97.5_56 uncultured_Trebouxia_photobiont_HQ026172 contains indel 1 seq

#OTU A_97.5_5 uncultured_Trebouxia_photobiont_AJ969576 contains indel 105 seqs
#OTU A_97.5_60 Trebouxia_arboricola_AJ007385 lacks indel 1 seq

#OTU A_97.5_7 Trebouxia_sp_OTU_A07_KR913093 contains indel 85 seqs
#OTU A_97.5_24 Trebouxia_sp_OTU_A07_KR913092 lacks indel 8 seqs

#OTU A_97.5_27 Trebouxia_sp_OTU_A01_KR912550 lacks indel 7 seqs
#OTU A_97.5_2 Trebouxia_sp_OTU_A01_KR912564 contains indel 283 seqs


master<-read.csv(file="/xxxx/MASTER_accession_list_INFOADDED.csv",stringsAsFactors=FALSE)
nrow(master[master$OTU_97.5_Raw_28_Oct_2019 %in% "A_97.5_9",])

#Make a new 97.5 column named modified, and have all subsequents use that...keep the lower numbered OTU, and change just the OTU number (and leave repseq column alone)?

missing$OTU_97.5_Raw_28_Oct_2019_Modified<-missing$OTU_97.5_Raw_28_Oct_2019
missing$OTU_97.5_Raw_28_Oct_2019_Modified[missing$OTU_97.5_Raw_28_Oct_2019_Modified %in% "A_97.5_56"]<-"A_97.5_9"
missing$OTU_97.5_Raw_28_Oct_2019_Modified[missing$OTU_97.5_Raw_28_Oct_2019_Modified %in% "A_97.5_60"]<-"A_97.5_5"
missing$OTU_97.5_Raw_28_Oct_2019_Modified[missing$OTU_97.5_Raw_28_Oct_2019_Modified %in% "A_97.5_24"]<-"A_97.5_7"
missing$OTU_97.5_Raw_28_Oct_2019_Modified[missing$OTU_97.5_Raw_28_Oct_2019_Modified %in% "A_97.5_27"]<-"A_97.5_2"

write.csv(missing,file="/xxxx/MASTER_accession_list_INFOADDED_MODIFIED.csv",row.names=FALSE)






#Keep non-singleton OTU's and summarize info
missing<-read.csv(file="/xxxx/MASTER_accession_list_INFOADDED_MODIFIED.csv",stringsAsFactors=FALSE)
nrow(missing)

#retain seqs that passed criteria
keeps<-missing[!is.na(missing$OTU_97.5_Raw_28_Oct_2019_Modified),]
nrow(keeps)

table(keeps$Clade)

#how many otu's
otus<-unique(keeps$OTU_97.5_Raw_28_Oct_2019_Modified)
length(otus)

length(unique(keeps$OTU_97.5_Raw_28_Oct_2019_Modified[keeps$Clade %in% "A"]))
length(unique(keeps$OTU_97.5_Raw_28_Oct_2019_Modified[keeps$Clade %in% "I"]))
length(unique(keeps$OTU_97.5_Raw_28_Oct_2019_Modified[keeps$Clade %in% "G"]))
length(unique(keeps$OTU_97.5_Raw_28_Oct_2019_Modified[keeps$Clade %in% "S"]))


#keep non-singleton otu's
otusns<-names(table(keeps$OTU_97.5_Raw_28_Oct_2019_Modified))[table(keeps$OTU_97.5_Raw_28_Oct_2019_Modified)>1]
length(otusns)

#retains sequences from otu's that are not singletons
keeps.otus<-keeps[keeps$OTU_97.5_Raw_28_Oct_2019_Modified %in% otusns,]

length(unique(keeps.otus$OTU_97.5_Raw_28_Oct_2019_Modified[keeps.otus$Clade %in% "A"]))
length(unique(keeps.otus$OTU_97.5_Raw_28_Oct_2019_Modified[keeps.otus$Clade %in% "I"]))
length(unique(keeps.otus$OTU_97.5_Raw_28_Oct_2019_Modified[keeps.otus$Clade %in% "G"]))
length(unique(keeps.otus$OTU_97.5_Raw_28_Oct_2019_Modified[keeps.otus$Clade %in% "S"]))


#how many w country data?
wco<-keeps[!is.na(keeps$Country),]
nrow(wco)


#prop of keeps with country data
nrow(wco)/nrow(keeps)

#number of wco with manually modified country data
nrow(wco[wco$Geo_Man_Cur %in% "Yes",])

table(wco$Country)

table(wco$Continent)



#make summary table for OTU's which includes singletons.
africa<-sort(unique(wco$Country[wco$Continent %in% "Africa"]))
antarctica<-sort(unique(wco$Country[wco$Continent %in% "Antarctica"]))
asia<-sort(unique(wco$Country[wco$Continent %in% "Asia"]))
europe<-sort(unique(wco$Country[wco$Continent %in% "Europe"]))
namerica<-sort(unique(wco$Country[wco$Continent %in% "North America"]))
oceania<-sort(unique(wco$Country[wco$Continent %in% "Oceania"]))
samerica<-sort(unique(wco$Country[wco$Continent %in% "South America"]))

#Africa
reg.tab<-data.frame(matrix(nrow=length(africa)+1,ncol=12))
colnames(reg.tab)<-c("Continent","Country","No_Seqs","No_OTUs","No_A_Seqs","No_A_OTUs","No_I_Seqs","No_I_OTUs","No_G_Seqs","No_G_OTUs","No_S_Seqs","No_S_OTUs")
reg.tab$Continent<-"Africa"
reg.tab$Country[2:nrow(reg.tab)]<-africa
reg.wco<-wco[wco$Continent %in% "Africa",]
reg.tab$No_Seqs[1]<-nrow(reg.wco)

#add in number of OTU's and clade specific seqs/OTU's per continent
reg.tab$No_OTUs[1]<-length(unique(reg.wco$OTU_97.5_Raw_28_Oct_2019_Modified))
reg.tab$No_A_Seqs[1]<-nrow(reg.wco[reg.wco$Clade %in% "A",])
reg.tab$No_A_OTUs[1]<-length(unique(reg.wco$OTU_97.5_Raw_28_Oct_2019_Modified[reg.wco$Clade %in% "A"]))
reg.tab$No_I_Seqs[1]<-nrow(reg.wco[reg.wco$Clade %in% "I",])
reg.tab$No_I_OTUs[1]<-length(unique(reg.wco$OTU_97.5_Raw_28_Oct_2019_Modified[reg.wco$Clade %in% "I"]))
reg.tab$No_G_Seqs[1]<-nrow(reg.wco[reg.wco$Clade %in% "G",])
reg.tab$No_G_OTUs[1]<-length(unique(reg.wco$OTU_97.5_Raw_28_Oct_2019_Modified[reg.wco$Clade %in% "G"]))
reg.tab$No_S_Seqs[1]<-nrow(reg.wco[reg.wco$Clade %in% "S",])
reg.tab$No_S_OTUs[1]<-length(unique(reg.wco$OTU_97.5_Raw_28_Oct_2019_Modified[reg.wco$Clade %in% "S"]))

#add in number of sequences per country
reg.co.tab<-table(reg.wco$Country)
for(x in 1:length(reg.co.tab)){
	reg.tab$No_Seqs[reg.tab$Country %in% names(reg.co.tab)[x]]<-reg.co.tab[[x]][1]
}

#add in number of OTUs and clade-specific seqs and OTU's per country
for(x in 2:nrow(reg.tab)){
	subwco<-NULL
	subwco<-reg.wco[reg.wco$Country %in% reg.tab$Country[x],]
	reg.tab$No_OTUs[x]<-length(unique(subwco$OTU_97.5_Raw_28_Oct_2019_Modified))
	reg.tab$No_A_Seqs[x]<-nrow(subwco[subwco$Clade %in% "A",])
	reg.tab$No_A_OTUs[x]<-length(unique(subwco$OTU_97.5_Raw_28_Oct_2019_Modified[subwco$Clade %in% "A"]))
	reg.tab$No_I_Seqs[x]<-nrow(subwco[subwco$Clade %in% "I",])
	reg.tab$No_I_OTUs[x]<-length(unique(subwco$OTU_97.5_Raw_28_Oct_2019_Modified[subwco$Clade %in% "I"]))
	reg.tab$No_G_Seqs[x]<-nrow(subwco[subwco$Clade %in% "G",])
	reg.tab$No_G_OTUs[x]<-length(unique(subwco$OTU_97.5_Raw_28_Oct_2019_Modified[subwco$Clade %in% "G"]))
	reg.tab$No_S_Seqs[x]<-nrow(subwco[subwco$Clade %in% "S",])
	reg.tab$No_S_OTUs[x]<-length(unique(subwco$OTU_97.5_Raw_28_Oct_2019_Modified[subwco$Clade %in% "S"]))
}

af.tab<-reg.tab

#Antarctia
reg.tab<-data.frame(matrix(nrow=length(antarctica)+1,ncol=12))
colnames(reg.tab)<-c("Continent","Country","No_Seqs","No_OTUs","No_A_Seqs","No_A_OTUs","No_I_Seqs","No_I_OTUs","No_G_Seqs","No_G_OTUs","No_S_Seqs","No_S_OTUs")
reg.tab$Continent<-"Antarctica"
reg.tab$Country[2:nrow(reg.tab)]<-antarctica
reg.wco<-wco[wco$Continent %in% "Antarctica",]
reg.tab$No_Seqs[1]<-nrow(reg.wco)

#add in number of OTU's and clade specific seqs/OTU's per continent
reg.tab$No_OTUs[1]<-length(unique(reg.wco$OTU_97.5_Raw_28_Oct_2019_Modified))
reg.tab$No_A_Seqs[1]<-nrow(reg.wco[reg.wco$Clade %in% "A",])
reg.tab$No_A_OTUs[1]<-length(unique(reg.wco$OTU_97.5_Raw_28_Oct_2019_Modified[reg.wco$Clade %in% "A"]))
reg.tab$No_I_Seqs[1]<-nrow(reg.wco[reg.wco$Clade %in% "I",])
reg.tab$No_I_OTUs[1]<-length(unique(reg.wco$OTU_97.5_Raw_28_Oct_2019_Modified[reg.wco$Clade %in% "I"]))
reg.tab$No_G_Seqs[1]<-nrow(reg.wco[reg.wco$Clade %in% "G",])
reg.tab$No_G_OTUs[1]<-length(unique(reg.wco$OTU_97.5_Raw_28_Oct_2019_Modified[reg.wco$Clade %in% "G"]))
reg.tab$No_S_Seqs[1]<-nrow(reg.wco[reg.wco$Clade %in% "S",])
reg.tab$No_S_OTUs[1]<-length(unique(reg.wco$OTU_97.5_Raw_28_Oct_2019_Modified[reg.wco$Clade %in% "S"]))

#add in number of sequences per country
reg.co.tab<-table(reg.wco$Country)
for(x in 1:length(reg.co.tab)){
	reg.tab$No_Seqs[reg.tab$Country %in% names(reg.co.tab)[x]]<-reg.co.tab[[x]][1]
}

#add in number of OTUs and clade-specific seqs and OTU's per country
for(x in 2:nrow(reg.tab)){
	subwco<-NULL
	subwco<-reg.wco[reg.wco$Country %in% reg.tab$Country[x],]
	reg.tab$No_OTUs[x]<-length(unique(subwco$OTU_97.5_Raw_28_Oct_2019_Modified))
	reg.tab$No_A_Seqs[x]<-nrow(subwco[subwco$Clade %in% "A",])
	reg.tab$No_A_OTUs[x]<-length(unique(subwco$OTU_97.5_Raw_28_Oct_2019_Modified[subwco$Clade %in% "A"]))
	reg.tab$No_I_Seqs[x]<-nrow(subwco[subwco$Clade %in% "I",])
	reg.tab$No_I_OTUs[x]<-length(unique(subwco$OTU_97.5_Raw_28_Oct_2019_Modified[subwco$Clade %in% "I"]))
	reg.tab$No_G_Seqs[x]<-nrow(subwco[subwco$Clade %in% "G",])
	reg.tab$No_G_OTUs[x]<-length(unique(subwco$OTU_97.5_Raw_28_Oct_2019_Modified[subwco$Clade %in% "G"]))
	reg.tab$No_S_Seqs[x]<-nrow(subwco[subwco$Clade %in% "S",])
	reg.tab$No_S_OTUs[x]<-length(unique(subwco$OTU_97.5_Raw_28_Oct_2019_Modified[subwco$Clade %in% "S"]))
}

an.tab<-reg.tab


#Asia
reg.tab<-data.frame(matrix(nrow=length(asia)+1,ncol=12))
colnames(reg.tab)<-c("Continent","Country","No_Seqs","No_OTUs","No_A_Seqs","No_A_OTUs","No_I_Seqs","No_I_OTUs","No_G_Seqs","No_G_OTUs","No_S_Seqs","No_S_OTUs")
reg.tab$Continent<-"Asia"
reg.tab$Country[2:nrow(reg.tab)]<-asia
reg.wco<-wco[wco$Continent %in% "Asia",]
reg.tab$No_Seqs[1]<-nrow(reg.wco)

#add in number of OTU's and clade specific seqs/OTU's per continent
reg.tab$No_OTUs[1]<-length(unique(reg.wco$OTU_97.5_Raw_28_Oct_2019_Modified))
reg.tab$No_A_Seqs[1]<-nrow(reg.wco[reg.wco$Clade %in% "A",])
reg.tab$No_A_OTUs[1]<-length(unique(reg.wco$OTU_97.5_Raw_28_Oct_2019_Modified[reg.wco$Clade %in% "A"]))
reg.tab$No_I_Seqs[1]<-nrow(reg.wco[reg.wco$Clade %in% "I",])
reg.tab$No_I_OTUs[1]<-length(unique(reg.wco$OTU_97.5_Raw_28_Oct_2019_Modified[reg.wco$Clade %in% "I"]))
reg.tab$No_G_Seqs[1]<-nrow(reg.wco[reg.wco$Clade %in% "G",])
reg.tab$No_G_OTUs[1]<-length(unique(reg.wco$OTU_97.5_Raw_28_Oct_2019_Modified[reg.wco$Clade %in% "G"]))
reg.tab$No_S_Seqs[1]<-nrow(reg.wco[reg.wco$Clade %in% "S",])
reg.tab$No_S_OTUs[1]<-length(unique(reg.wco$OTU_97.5_Raw_28_Oct_2019_Modified[reg.wco$Clade %in% "S"]))

#add in number of sequences per country
reg.co.tab<-table(reg.wco$Country)
for(x in 1:length(reg.co.tab)){
	reg.tab$No_Seqs[reg.tab$Country %in% names(reg.co.tab)[x]]<-reg.co.tab[[x]][1]
}

#add in number of OTUs and clade-specific seqs and OTU's per country
for(x in 2:nrow(reg.tab)){
	subwco<-NULL
	subwco<-reg.wco[reg.wco$Country %in% reg.tab$Country[x],]
	reg.tab$No_OTUs[x]<-length(unique(subwco$OTU_97.5_Raw_28_Oct_2019_Modified))
	reg.tab$No_A_Seqs[x]<-nrow(subwco[subwco$Clade %in% "A",])
	reg.tab$No_A_OTUs[x]<-length(unique(subwco$OTU_97.5_Raw_28_Oct_2019_Modified[subwco$Clade %in% "A"]))
	reg.tab$No_I_Seqs[x]<-nrow(subwco[subwco$Clade %in% "I",])
	reg.tab$No_I_OTUs[x]<-length(unique(subwco$OTU_97.5_Raw_28_Oct_2019_Modified[subwco$Clade %in% "I"]))
	reg.tab$No_G_Seqs[x]<-nrow(subwco[subwco$Clade %in% "G",])
	reg.tab$No_G_OTUs[x]<-length(unique(subwco$OTU_97.5_Raw_28_Oct_2019_Modified[subwco$Clade %in% "G"]))
	reg.tab$No_S_Seqs[x]<-nrow(subwco[subwco$Clade %in% "S",])
	reg.tab$No_S_OTUs[x]<-length(unique(subwco$OTU_97.5_Raw_28_Oct_2019_Modified[subwco$Clade %in% "S"]))
}

asia.tab<-reg.tab


#Europe
reg.tab<-data.frame(matrix(nrow=length(europe)+1,ncol=12))
colnames(reg.tab)<-c("Continent","Country","No_Seqs","No_OTUs","No_A_Seqs","No_A_OTUs","No_I_Seqs","No_I_OTUs","No_G_Seqs","No_G_OTUs","No_S_Seqs","No_S_OTUs")
reg.tab$Continent<-"Europe"
reg.tab$Country[2:nrow(reg.tab)]<-europe
reg.wco<-wco[wco$Continent %in% "Europe",]
reg.tab$No_Seqs[1]<-nrow(reg.wco)

#add in number of OTU's and clade specific seqs/OTU's per continent
reg.tab$No_OTUs[1]<-length(unique(reg.wco$OTU_97.5_Raw_28_Oct_2019_Modified))
reg.tab$No_A_Seqs[1]<-nrow(reg.wco[reg.wco$Clade %in% "A",])
reg.tab$No_A_OTUs[1]<-length(unique(reg.wco$OTU_97.5_Raw_28_Oct_2019_Modified[reg.wco$Clade %in% "A"]))
reg.tab$No_I_Seqs[1]<-nrow(reg.wco[reg.wco$Clade %in% "I",])
reg.tab$No_I_OTUs[1]<-length(unique(reg.wco$OTU_97.5_Raw_28_Oct_2019_Modified[reg.wco$Clade %in% "I"]))
reg.tab$No_G_Seqs[1]<-nrow(reg.wco[reg.wco$Clade %in% "G",])
reg.tab$No_G_OTUs[1]<-length(unique(reg.wco$OTU_97.5_Raw_28_Oct_2019_Modified[reg.wco$Clade %in% "G"]))
reg.tab$No_S_Seqs[1]<-nrow(reg.wco[reg.wco$Clade %in% "S",])
reg.tab$No_S_OTUs[1]<-length(unique(reg.wco$OTU_97.5_Raw_28_Oct_2019_Modified[reg.wco$Clade %in% "S"]))

#add in number of sequences per country
reg.co.tab<-table(reg.wco$Country)
for(x in 1:length(reg.co.tab)){
	reg.tab$No_Seqs[reg.tab$Country %in% names(reg.co.tab)[x]]<-reg.co.tab[[x]][1]
}

#add in number of OTUs and clade-specific seqs and OTU's per country
for(x in 2:nrow(reg.tab)){
	subwco<-NULL
	subwco<-reg.wco[reg.wco$Country %in% reg.tab$Country[x],]
	reg.tab$No_OTUs[x]<-length(unique(subwco$OTU_97.5_Raw_28_Oct_2019_Modified))
	reg.tab$No_A_Seqs[x]<-nrow(subwco[subwco$Clade %in% "A",])
	reg.tab$No_A_OTUs[x]<-length(unique(subwco$OTU_97.5_Raw_28_Oct_2019_Modified[subwco$Clade %in% "A"]))
	reg.tab$No_I_Seqs[x]<-nrow(subwco[subwco$Clade %in% "I",])
	reg.tab$No_I_OTUs[x]<-length(unique(subwco$OTU_97.5_Raw_28_Oct_2019_Modified[subwco$Clade %in% "I"]))
	reg.tab$No_G_Seqs[x]<-nrow(subwco[subwco$Clade %in% "G",])
	reg.tab$No_G_OTUs[x]<-length(unique(subwco$OTU_97.5_Raw_28_Oct_2019_Modified[subwco$Clade %in% "G"]))
	reg.tab$No_S_Seqs[x]<-nrow(subwco[subwco$Clade %in% "S",])
	reg.tab$No_S_OTUs[x]<-length(unique(subwco$OTU_97.5_Raw_28_Oct_2019_Modified[subwco$Clade %in% "S"]))
}

eu.tab<-reg.tab



#North America
reg.tab<-data.frame(matrix(nrow=length(namerica)+1,ncol=12))
colnames(reg.tab)<-c("Continent","Country","No_Seqs","No_OTUs","No_A_Seqs","No_A_OTUs","No_I_Seqs","No_I_OTUs","No_G_Seqs","No_G_OTUs","No_S_Seqs","No_S_OTUs")
reg.tab$Continent<-"North America"
reg.tab$Country[2:nrow(reg.tab)]<-namerica
reg.wco<-wco[wco$Continent %in% "North America",]
reg.tab$No_Seqs[1]<-nrow(reg.wco)

#add in number of OTU's and clade specific seqs/OTU's per continent
reg.tab$No_OTUs[1]<-length(unique(reg.wco$OTU_97.5_Raw_28_Oct_2019_Modified))
reg.tab$No_A_Seqs[1]<-nrow(reg.wco[reg.wco$Clade %in% "A",])
reg.tab$No_A_OTUs[1]<-length(unique(reg.wco$OTU_97.5_Raw_28_Oct_2019_Modified[reg.wco$Clade %in% "A"]))
reg.tab$No_I_Seqs[1]<-nrow(reg.wco[reg.wco$Clade %in% "I",])
reg.tab$No_I_OTUs[1]<-length(unique(reg.wco$OTU_97.5_Raw_28_Oct_2019_Modified[reg.wco$Clade %in% "I"]))
reg.tab$No_G_Seqs[1]<-nrow(reg.wco[reg.wco$Clade %in% "G",])
reg.tab$No_G_OTUs[1]<-length(unique(reg.wco$OTU_97.5_Raw_28_Oct_2019_Modified[reg.wco$Clade %in% "G"]))
reg.tab$No_S_Seqs[1]<-nrow(reg.wco[reg.wco$Clade %in% "S",])
reg.tab$No_S_OTUs[1]<-length(unique(reg.wco$OTU_97.5_Raw_28_Oct_2019_Modified[reg.wco$Clade %in% "S"]))

#add in number of sequences per country
reg.co.tab<-table(reg.wco$Country)
for(x in 1:length(reg.co.tab)){
	reg.tab$No_Seqs[reg.tab$Country %in% names(reg.co.tab)[x]]<-reg.co.tab[[x]][1]
}

#add in number of OTUs and clade-specific seqs and OTU's per country
for(x in 2:nrow(reg.tab)){
	subwco<-NULL
	subwco<-reg.wco[reg.wco$Country %in% reg.tab$Country[x],]
	reg.tab$No_OTUs[x]<-length(unique(subwco$OTU_97.5_Raw_28_Oct_2019_Modified))
	reg.tab$No_A_Seqs[x]<-nrow(subwco[subwco$Clade %in% "A",])
	reg.tab$No_A_OTUs[x]<-length(unique(subwco$OTU_97.5_Raw_28_Oct_2019_Modified[subwco$Clade %in% "A"]))
	reg.tab$No_I_Seqs[x]<-nrow(subwco[subwco$Clade %in% "I",])
	reg.tab$No_I_OTUs[x]<-length(unique(subwco$OTU_97.5_Raw_28_Oct_2019_Modified[subwco$Clade %in% "I"]))
	reg.tab$No_G_Seqs[x]<-nrow(subwco[subwco$Clade %in% "G",])
	reg.tab$No_G_OTUs[x]<-length(unique(subwco$OTU_97.5_Raw_28_Oct_2019_Modified[subwco$Clade %in% "G"]))
	reg.tab$No_S_Seqs[x]<-nrow(subwco[subwco$Clade %in% "S",])
	reg.tab$No_S_OTUs[x]<-length(unique(subwco$OTU_97.5_Raw_28_Oct_2019_Modified[subwco$Clade %in% "S"]))
}

nam.tab<-reg.tab




#Oceania
reg.tab<-data.frame(matrix(nrow=length(oceania)+1,ncol=12))
colnames(reg.tab)<-c("Continent","Country","No_Seqs","No_OTUs","No_A_Seqs","No_A_OTUs","No_I_Seqs","No_I_OTUs","No_G_Seqs","No_G_OTUs","No_S_Seqs","No_S_OTUs")
reg.tab$Continent<-"Oceania"
reg.tab$Country[2:nrow(reg.tab)]<-oceania
reg.wco<-wco[wco$Continent %in% "Oceania",]
reg.tab$No_Seqs[1]<-nrow(reg.wco)

#add in number of OTU's and clade specific seqs/OTU's per continent
reg.tab$No_OTUs[1]<-length(unique(reg.wco$OTU_97.5_Raw_28_Oct_2019_Modified))
reg.tab$No_A_Seqs[1]<-nrow(reg.wco[reg.wco$Clade %in% "A",])
reg.tab$No_A_OTUs[1]<-length(unique(reg.wco$OTU_97.5_Raw_28_Oct_2019_Modified[reg.wco$Clade %in% "A"]))
reg.tab$No_I_Seqs[1]<-nrow(reg.wco[reg.wco$Clade %in% "I",])
reg.tab$No_I_OTUs[1]<-length(unique(reg.wco$OTU_97.5_Raw_28_Oct_2019_Modified[reg.wco$Clade %in% "I"]))
reg.tab$No_G_Seqs[1]<-nrow(reg.wco[reg.wco$Clade %in% "G",])
reg.tab$No_G_OTUs[1]<-length(unique(reg.wco$OTU_97.5_Raw_28_Oct_2019_Modified[reg.wco$Clade %in% "G"]))
reg.tab$No_S_Seqs[1]<-nrow(reg.wco[reg.wco$Clade %in% "S",])
reg.tab$No_S_OTUs[1]<-length(unique(reg.wco$OTU_97.5_Raw_28_Oct_2019_Modified[reg.wco$Clade %in% "S"]))

#add in number of sequences per country
reg.co.tab<-table(reg.wco$Country)
for(x in 1:length(reg.co.tab)){
	reg.tab$No_Seqs[reg.tab$Country %in% names(reg.co.tab)[x]]<-reg.co.tab[[x]][1]
}

#add in number of OTUs and clade-specific seqs and OTU's per country
for(x in 2:nrow(reg.tab)){
	subwco<-NULL
	subwco<-reg.wco[reg.wco$Country %in% reg.tab$Country[x],]
	reg.tab$No_OTUs[x]<-length(unique(subwco$OTU_97.5_Raw_28_Oct_2019_Modified))
	reg.tab$No_A_Seqs[x]<-nrow(subwco[subwco$Clade %in% "A",])
	reg.tab$No_A_OTUs[x]<-length(unique(subwco$OTU_97.5_Raw_28_Oct_2019_Modified[subwco$Clade %in% "A"]))
	reg.tab$No_I_Seqs[x]<-nrow(subwco[subwco$Clade %in% "I",])
	reg.tab$No_I_OTUs[x]<-length(unique(subwco$OTU_97.5_Raw_28_Oct_2019_Modified[subwco$Clade %in% "I"]))
	reg.tab$No_G_Seqs[x]<-nrow(subwco[subwco$Clade %in% "G",])
	reg.tab$No_G_OTUs[x]<-length(unique(subwco$OTU_97.5_Raw_28_Oct_2019_Modified[subwco$Clade %in% "G"]))
	reg.tab$No_S_Seqs[x]<-nrow(subwco[subwco$Clade %in% "S",])
	reg.tab$No_S_OTUs[x]<-length(unique(subwco$OTU_97.5_Raw_28_Oct_2019_Modified[subwco$Clade %in% "S"]))
}

oc.tab<-reg.tab


#South America
reg.tab<-data.frame(matrix(nrow=length(samerica)+1,ncol=12))
colnames(reg.tab)<-c("Continent","Country","No_Seqs","No_OTUs","No_A_Seqs","No_A_OTUs","No_I_Seqs","No_I_OTUs","No_G_Seqs","No_G_OTUs","No_S_Seqs","No_S_OTUs")
reg.tab$Continent<-"South America"
reg.tab$Country[2:nrow(reg.tab)]<-samerica
reg.wco<-wco[wco$Continent %in% "South America",]
reg.tab$No_Seqs[1]<-nrow(reg.wco)

#add in number of OTU's and clade specific seqs/OTU's per continent
reg.tab$No_OTUs[1]<-length(unique(reg.wco$OTU_97.5_Raw_28_Oct_2019_Modified))
reg.tab$No_A_Seqs[1]<-nrow(reg.wco[reg.wco$Clade %in% "A",])
reg.tab$No_A_OTUs[1]<-length(unique(reg.wco$OTU_97.5_Raw_28_Oct_2019_Modified[reg.wco$Clade %in% "A"]))
reg.tab$No_I_Seqs[1]<-nrow(reg.wco[reg.wco$Clade %in% "I",])
reg.tab$No_I_OTUs[1]<-length(unique(reg.wco$OTU_97.5_Raw_28_Oct_2019_Modified[reg.wco$Clade %in% "I"]))
reg.tab$No_G_Seqs[1]<-nrow(reg.wco[reg.wco$Clade %in% "G",])
reg.tab$No_G_OTUs[1]<-length(unique(reg.wco$OTU_97.5_Raw_28_Oct_2019_Modified[reg.wco$Clade %in% "G"]))
reg.tab$No_S_Seqs[1]<-nrow(reg.wco[reg.wco$Clade %in% "S",])
reg.tab$No_S_OTUs[1]<-length(unique(reg.wco$OTU_97.5_Raw_28_Oct_2019_Modified[reg.wco$Clade %in% "S"]))

#add in number of sequences per country
reg.co.tab<-table(reg.wco$Country)
for(x in 1:length(reg.co.tab)){
	reg.tab$No_Seqs[reg.tab$Country %in% names(reg.co.tab)[x]]<-reg.co.tab[[x]][1]
}

#add in number of OTUs and clade-specific seqs and OTU's per country
for(x in 2:nrow(reg.tab)){
	subwco<-NULL
	subwco<-reg.wco[reg.wco$Country %in% reg.tab$Country[x],]
	reg.tab$No_OTUs[x]<-length(unique(subwco$OTU_97.5_Raw_28_Oct_2019_Modified))
	reg.tab$No_A_Seqs[x]<-nrow(subwco[subwco$Clade %in% "A",])
	reg.tab$No_A_OTUs[x]<-length(unique(subwco$OTU_97.5_Raw_28_Oct_2019_Modified[subwco$Clade %in% "A"]))
	reg.tab$No_I_Seqs[x]<-nrow(subwco[subwco$Clade %in% "I",])
	reg.tab$No_I_OTUs[x]<-length(unique(subwco$OTU_97.5_Raw_28_Oct_2019_Modified[subwco$Clade %in% "I"]))
	reg.tab$No_G_Seqs[x]<-nrow(subwco[subwco$Clade %in% "G",])
	reg.tab$No_G_OTUs[x]<-length(unique(subwco$OTU_97.5_Raw_28_Oct_2019_Modified[subwco$Clade %in% "G"]))
	reg.tab$No_S_Seqs[x]<-nrow(subwco[subwco$Clade %in% "S",])
	reg.tab$No_S_OTUs[x]<-length(unique(subwco$OTU_97.5_Raw_28_Oct_2019_Modified[subwco$Clade %in% "S"]))
}

sam.tab<-reg.tab


combo.tab<-rbind(af.tab,an.tab,asia.tab,eu.tab,nam.tab,oc.tab,sam.tab)

write.csv(combo.tab,file="/xxxx/summary_country_data_passed_filters_but_singletons_included_MODIFIED.csv",row.names=FALSE)














#plot by country
#essentially taken from here: http://stackoverflow.com/questions/27338512/color-countries-on-world-map-based-on-iso3-codes-in-r-using-ggplot
library(maptools)
library(mapproj)
library(rgeos)
library(rgdal)
library(ggplot2)
library(jsonlite)
library(RCurl)

#French Guiana, Greenland and the Falkland Islands, which all have separate ISO3A3 codes, were treated as separate countries

#downloaded ne_10m_admin_0_map_units
#from http://www.naturalearthdata.com/downloads/10m-cultural-vectors/10m-admin-0-details/

#wworld<-readOGR(dsn="/xxxx/ne_10m_admin_0_map_units",layer="ne_10m_admin_0_map_units")
world<-readOGR(dsn="/xxxx/ne_110m_admin_0_countries",layer="ne_110m_admin_0_countries")
world<-spTransform(world,CRS("+proj=wintri"))
names(world)
unique(world$continent)

#should be Russia (RUS)
world$iso_a3[136]
#change to part of Asia instead of Europe
world$continent[136]<-"Asia"

#should be Trinidad & Tobago (TTO)
world$iso_a3[161]
#change to part of South America instead of North America
world$continent[161]<-"South America"
#make a table by ISO3 code for samples


#Issue with Kosovo - have one sample. ISOA3 code I think is UNK, but not in world map used here...so, change to Serbia
world$admin[89]
world$country[89]
world$continent[89]
world$iso_a3[89]
countries<-as.character(world$iso_a3)
countries
countries[89]
countries[89]<-"UNK"
countries
new.levels<-c(levels(world$iso_a3)[1:89],"UNK",levels(world$iso_a3)[90:175])
levels(world$iso_a3)
world$iso_a3
world$iso_a3<-factor(countries)
world$iso_a3

me<-read.csv(file="/xxxx/MASTER_accession_list_INFOADDED_MODIFIED.csv",stringsAsFactors=FALSE)
me<-me[!is.na(me$OTU_97.5_Raw_28_Oct_2019_Modified) & !is.na(me$ISO3Code),]
samps<-table(me$ISO3Code)
samps.df<-as.data.frame(samps)
samps.df[]<-lapply(samps.df,as.character)
colnames(samps.df)<-c("Country.Code","Abundance")
samps.df$Abundance<-as.numeric(samps.df$Abundance)
dd<-data.frame(matrix(ncol=2,nrow=length(world$iso_a3)))
colnames(dd)<-c("Country.Code","Abundance")
dd$Abundance<-NA
dd$Country.Code<-as.character(world$iso_a3)
for(d in 1:nrow(dd)){
	if(any(samps.df$Country.Code %in% dd$Country.Code[d])){
		dd$Abundance[d]<-samps.df[samps.df$Country.Code %in% dd$Country.Code[d],"Abundance"]
	}
}

map<-fortify(world,region="iso_a3")
gg <- ggplot()
gg <- gg + geom_map(data=map, map=map,aes(x=long, y=lat, map_id=id, group=group),fill="black", color=NA)
gg <- gg + geom_map(data=dd, map=map, color="white", size=0.05,aes(fill=Abundance,group=Country.Code, map_id=Country.Code))
gg <- gg + theme_bw()
gg <- gg + scale_fill_gradient(low="#f7fcb9", high="#31a354",na.value="grey92",name="Number of Sequences")
mytitle<-substitute(paste(italic("Trebouxia")," Sequences by Country (N=",x,")",sep=""),list(x=nrow(me)))
gg <- gg + labs(title=mytitle)
gg <- gg + coord_equal(ratio=1)
gg <- gg + theme(legend.position="bottom")
gg <- gg + theme(legend.key = element_blank())
gg <- gg + theme(plot.title=element_text(size=22))
gg <- gg + theme(plot.title=element_text(hjust=0.5),axis.title.x=element_blank(),axis.title.y=element_blank(),axis.ticks.x=element_blank(),axis.ticks.y=element_blank(),axis.text.x=element_blank(),axis.text.y=element_blank(),panel.border=element_blank(),panel.grid.major=element_blank(),panel.grid.minor=element_blank())
#png("~/Desktop/trebouxia_seqs_per_country.png",width=11,height=11,units="in",res=600)
#gg
#dev.off()
#png("~/Desktop/trebouxia_seqs_per_country_highres.png",width=6,height=6,units="in",res=2400)
#gg
#dev.off()
pdf("/xxxx/trebouxia_seqs_per_country_29oct2019_highres_MODIFIED.pdf",width=11,height=11)
gg
dev.off()
#which countries have most
nrow(me)
sort(table(me$Country))
sum(table(me$Country))


#############Plot those with coordinates by clade: 
missing<-read.csv(file="/xxxx/MASTER_accession_list_INFOADDED_MODIFIED.csv",stringsAsFactors=FALSE)

#retain seqs that passed criteria
keeps<-missing[!is.na(missing$OTU_97.5_Raw_28_Oct_2019_Modified),]

#number passing quality filters
nrow(keeps)

#keep only those w LatC
keeps<-keeps[!is.na(keeps$LatC),]
nrow(keeps)

#change clade G to C
keeps$OTU_97.5_Raw_28_Oct_2019_Modified<-gsub("G_","C_",keeps$OTU_97.5_Raw_28_Oct_2019_Modified)
keeps$Clade<-gsub("G","C",keeps$Clade)

#plot them
library(maptools)
require(ggplot2)
data(wrld_simpl)
map<-fortify(wrld_simpl)
gg <- ggplot()
gg <- gg + geom_map(data=map, map=map,aes(x=long, y=lat, map_id=id, group=group),fill="grey", color=NA)
#gg <- gg + geom_map(data=dd, map=map, color="white", size=0.05,aes(fill=Abundance,group=Country.Code, map_id=Country.Code))
gg <- gg + theme_bw()
gg <- gg + scale_fill_gradient(low="#f7fcb9", high="#31a354",na.value="grey92",name="Number of Sequences")
mytitle<-substitute(paste(italic("Trebouxia")," Sequences with Coordinates (N=",x,")",sep=""),list(x=nrow(keeps)))
gg <- gg + labs(title=mytitle)
gg <- gg + coord_equal(ratio=1)
#gg <- gg + theme(legend.position="bottom")
gg <- gg + theme(legend.key = element_blank())
gg <- gg + theme(plot.title=element_text(size=22))
gg <- gg + theme(plot.title=element_text(hjust=0.5),axis.title.x=element_blank(),axis.title.y=element_blank(),axis.ticks.x=element_blank(),axis.ticks.y=element_blank(),axis.text.x=element_blank(),axis.text.y=element_blank(),panel.border=element_blank(),panel.grid.major=element_blank(),panel.grid.minor=element_blank())
#gg<-gg+geom_point(data=keeps, aes(x=LongC,y=LatC),color=CladeCol,alpha=0.75,na.rm=TRUE)
gg<-gg+geom_point(data=keeps, aes(x=LongC,y=LatC,color=Clade),alpha=0.5,na.rm=TRUE, size=0.4)+scale_color_manual(values=c(my.cb<-c("#543005", "#8E0152","#F0E442","#0072B2")))+ guides(colour = guide_legend(override.aes = list(size=3)))
pdf("/xxxx/trebouxia_seqs_coords_MODIFIED.pdf",width=11,height=11)
gg
dev.off()








######################
#OTU table with total number of seqs, number of seqs for that OTU and number of OTUs per continent
######################
#read in data file
missing<-read.csv(file="/xxxx/MASTER_accession_list_INFOADDED_MODIFIED.csv",stringsAsFactors=FALSE)

#retain seqs that passed criteria
keeps<-missing[!is.na(missing$OTU_97.5_Raw_28_Oct_2019_Modified),]

#number passing quality filters
nrow(keeps)

#make data frame
require(gtools) #for mixedsort function
clades<-c("A","I","G","S")
cols<-c("OTU","No_Sequences","No_OTUs","No_Sequences_Country","No_OTUs_Country","No_Seqs_Africa","No_Seqs_Antarctica","No_Seqs_Asia","No_Seqs_Europe","No_Seqs_North_America","No_Seqs_Oceania","No_Seqs_South_America","No_OTUs_Africa","No_OTUs_Antarctica","No_OTUs_Asia","No_OTUs_Europe","No_OTUs_North_America","No_OTUs_Oceania","No_OTUs_South_America")
combo<-data.frame(matrix(nrow=0,ncol=length(cols)))
colnames(combo)<-cols
for(x in 1:length(clades)){
	#subset to keep those w country info and from clade of interest
	temp<-NULL
	temp<-data.frame(matrix(nrow=1+length(unique(keeps$OTU_97.5_Raw_28_Oct_2019_Modified[keeps$Clade %in% clades[x]])),ncol=length(cols),0))
	colnames(temp)<-cols
	temp$OTU[1]<-clades[x]
	temp$No_Sequences[1]<-nrow(keeps[keeps$Clade %in% clades[x],])
	temp$No_OTUs[1]<-length(unique(keeps$OTU_97.5_Raw_28_Oct_2019_Modified[keeps$Clade %in% clades[x]]))
	wco<-NULL
	wco<-keeps[!is.na(keeps$Country) & keeps$Clade %in% clades[x],]
	temp$No_Sequences_Country[1]<-nrow(wco)
	temp$No_OTUs_Country[1]<-length(unique(wco$OTU_97.5_Raw_28_Oct_2019_Modified))
	temp$No_Seqs_Africa[1]<-nrow(wco[wco$Continent %in% "Africa",])
	temp$No_Seqs_Antarctica[1]<-nrow(wco[wco$Continent %in% "Antarctica",])
	temp$No_Seqs_Asia[1]<-nrow(wco[wco$Continent %in% "Asia",])
	temp$No_Seqs_Europe[1]<-nrow(wco[wco$Continent %in% "Europe",])
	temp$No_Seqs_North_America[1]<-nrow(wco[wco$Continent %in% "North America",])
	temp$No_Seqs_Oceania[1]<-nrow(wco[wco$Continent %in% "Oceania",])
	temp$No_Seqs_South_America[1]<-nrow(wco[wco$Continent %in% "South America",])
	temp$No_OTUs_Africa[1]<-length(unique(wco$OTU_97.5_Raw_28_Oct_2019_Modified[wco$Continent %in% "Africa"]))
	temp$No_OTUs_Antarctica[1]<-length(unique(wco$OTU_97.5_Raw_28_Oct_2019_Modified[wco$Continent %in% "Antarctica"]))
	temp$No_OTUs_Asia[1]<-length(unique(wco$OTU_97.5_Raw_28_Oct_2019_Modified[wco$Continent %in% "Asia"]))
	temp$No_OTUs_Europe[1]<-length(unique(wco$OTU_97.5_Raw_28_Oct_2019_Modified[wco$Continent %in% "Europe"]))
	temp$No_OTUs_North_America[1]<-length(unique(wco$OTU_97.5_Raw_28_Oct_2019_Modified[wco$Continent %in% "North America"]))
	temp$No_OTUs_Oceania[1]<-length(unique(wco$OTU_97.5_Raw_28_Oct_2019_Modified[wco$Continent %in% "Oceania"]))
	temp$No_OTUs_South_America[1]<-length(unique(wco$OTU_97.5_Raw_28_Oct_2019_Modified[wco$Continent %in% "South America"]))
	temp$OTU[2:nrow(temp)]<-mixedsort(unique(keeps[keeps$Clade %in% clades[x],"OTU_97.5_Raw_28_Oct_2019_Modified"]))
	temp.tab<-table(keeps[keeps$Clade %in% clades[x],"OTU_97.5_Raw_28_Oct_2019_Modified"])
	for(z in 1:length(temp.tab)){
		temp$No_Sequences[temp$OTU %in% names(temp.tab)[z]]<-temp.tab[[z]][1]
		temp$No_OTUs[temp$OTU %in% names(temp.tab)[z]]<-1
	}
	
	temp.tab<-NULL
	temp.tab<-table(wco[wco$Clade %in% clades[x],"OTU_97.5_Raw_28_Oct_2019_Modified"])
	for(z in 1:length(temp.tab)){
		temp$No_Sequences_Country[temp$OTU %in% names(temp.tab)[z]]<-temp.tab[[z]][1]
		temp$No_OTUs_Country[temp$OTU %in% names(temp.tab)[z]]<-1
	}
	
	for(z in 2:nrow(temp)){	
		temp$No_Seqs_Africa[z]<-nrow(wco[wco$Continent %in% "Africa" & wco$OTU_97.5_Raw_28_Oct_2019_Modified %in% temp$OTU[z],])
		if(temp$No_Seqs_Africa[z]>0){
			temp$No_OTUs_Africa[z]<-1
		}
		temp$No_Seqs_Antarctica[z]<-nrow(wco[wco$Continent %in% "Antarctica" & wco$OTU_97.5_Raw_28_Oct_2019_Modified %in% temp$OTU[z],])
		if(temp$No_Seqs_Antarctica[z]>0){
			temp$No_OTUs_Antarctica[z]<-1
		}
		temp$No_Seqs_Asia[z]<-nrow(wco[wco$Continent %in% "Asia" & wco$OTU_97.5_Raw_28_Oct_2019_Modified %in% temp$OTU[z],])
		if(temp$No_Seqs_Asia[z]>0){
			temp$No_OTUs_Asia[z]<-1
		}
		temp$No_Seqs_Europe[z]<-nrow(wco[wco$Continent %in% "Europe" & wco$OTU_97.5_Raw_28_Oct_2019_Modified %in% temp$OTU[z],])
		if(temp$No_Seqs_Europe[z]>0){
			temp$No_OTUs_Europe[z]<-1
		}
		temp$No_Seqs_North_America[z]<-nrow(wco[wco$Continent %in% "North America" & wco$OTU_97.5_Raw_28_Oct_2019_Modified %in% temp$OTU[z],])
		if(temp$No_Seqs_North_America[z]>0){
			temp$No_OTUs_North_America[z]<-1
		}
		temp$No_Seqs_Oceania[z]<-nrow(wco[wco$Continent %in% "Oceania" & wco$OTU_97.5_Raw_28_Oct_2019_Modified %in% temp$OTU[z],])
		if(temp$No_Seqs_Oceania[z]>0){
			temp$No_OTUs_Oceania[z]<-1
		}
		temp$No_Seqs_South_America[z]<-nrow(wco[wco$Continent %in% "South America" & wco$OTU_97.5_Raw_28_Oct_2019_Modified %in% temp$OTU[z],])
		if(temp$No_Seqs_South_America[z]>0){
			temp$No_OTUs_South_America[z]<-1
		}
	}
	combo<-rbind(combo,temp)
}

write.csv(combo,file="/xxxx/OTU_97.5_GEO_SUMMARY_MODIFIED.csv",row.names=FALSE)





######################
#OTU table only for those w lat long AND bioclim...n seqs, bioclim mean and sd number of unique coordinates
######################
#read in data file
missing<-read.csv(file="/xxxx/MASTER_accession_list_INFOADDED_MODIFIED.csv",stringsAsFactors=FALSE)

#retain seqs that passed criteria
keeps<-missing[!is.na(missing$OTU_97.5_Raw_28_Oct_2019_Modified) &!is.na(missing$LatC),]

#number passing quality filters
nrow(keeps)

#looks like all w coords have climatic data
keeps[!is.na(keeps$LatC) & is.na(keeps$bio1),]

#might not be same for biomes (some could be close to water (looks like 72 accessions lack biome info)
nrow(keeps[!is.na(keeps$LatC) & is.na(keeps$Biome),])

#make temp variables in degrees C
cols<-paste("bio",1:11,sep="")
for(x in 1:nrow(keeps)){
	for(b in 1:length(cols)){
		keeps[x,cols[b]]<-keeps[x,cols[b]]/10
	}
}

#make data frame
require(gtools) #for mixedsort function
clades<-c("A","I","G","S")
cols<-c("OTU","No_Sequences","No_Unique_Sites","No_Unique_Sites_Seqs",paste("Mean_Bio",1:19,sep=""),paste("SD_Bio",1:19,sep=""))
combo<-data.frame(matrix(nrow=0,ncol=length(cols)))
colnames(combo)<-cols

for(x in 1:length(clades)){
	wco<-NULL
	wco<-keeps[keeps$Clade %in% clades[x],]
	temp<-NULL
	temp<-data.frame(matrix(nrow=length(unique(wco$OTU_97.5_Raw_28_Oct_2019_Modified)),ncol=length(cols),0))
	colnames(temp)<-cols
	temp$OTU<-mixedsort(unique(wco[wco$Clade %in% clades[x],"OTU_97.5_Raw_28_Oct_2019_Modified"]))
	temp.tab<-table(wco[wco$Clade %in% clades[x],"OTU_97.5_Raw_28_Oct_2019_Modified"])
	for(z in 1:length(temp.tab)){
		temp$No_Sequences[temp$OTU %in% names(temp.tab)[z]]<-temp.tab[[z]][1]
		temp$No_Unique_Sites[temp$OTU %in% names(temp.tab)[z]]<-nrow(unique(wco[wco$OTU_97.5_Raw_28_Oct_2019_Modified %in% names(temp.tab)[z],c("LatC","LongC")]))
	}
	for(y in 1:nrow(temp)){	
		for(q in 1:19){
			sub.otu<-NULL
			sub.otu<-keeps[keeps$OTU_97.5_Raw_28_Oct_2019_Modified %in% temp$OTU[y],]
			sub.otu<-sub.otu[!duplicated(sub.otu[,c("LatC","LongC")]),]
			temp[y,"No_Unique_Sites_Seqs"]<-nrow(sub.otu)
			temp[y,paste("Mean_Bio",q,sep="")]<-mean(sub.otu[,paste("bio",q,sep="")])
			#temp[y,paste("Mean_Bio",q,sep="")]<-mean(keeps[keeps$OTU_97.5_Raw_28_Oct_2019_Modified %in% temp$OTU[y],paste("bio",q,sep="")])
			temp[y,paste("SD_Bio",q,sep="")]<-sd(sub.otu[,paste("bio",q,sep="")])
			#temp[y,paste("SD_Bio",q,sep="")]<-sd(keeps[keeps$OTU_97.5_Raw_28_Oct_2019_Modified %in% temp$OTU[y],paste("bio",q,sep="")])
		}	
	}	
	combo<-rbind(combo,temp)
}		
write.csv(combo,file="/xxxx/OTU_97.5_BIOCLIM_SUMMARY_MODIFIED_UNIQUES.csv",row.names=FALSE)

#make boxplot?
#read in data file
missing<-read.csv(file="/xxxx/MASTER_accession_list_INFOADDED_MODIFIED.csv",stringsAsFactors=FALSE)

#retain seqs that passed criteria
keeps<-missing[!is.na(missing$OTU_97.5_Raw_28_Oct_2019_Modified) &!is.na(missing$LatC),]

#number passing quality filters
nrow(keeps)

#looks like all w coords have climatic data
keeps[!is.na(keeps$LatC) & is.na(keeps$bio1),]

#might not be same for biomes (some could be close to water (looks like 72 accessions lack biome info)
nrow(keeps[!is.na(keeps$LatC) & is.na(keeps$Biome),])

#make temp variables in degrees C
cols<-paste("bio",1:11,sep="")
for(x in 1:nrow(keeps)){
	for(b in 1:length(cols)){
		keeps[x,cols[b]]<-keeps[x,cols[b]]/10
	}
}






#############MEAN
#get repseq names
repseqs<-read.csv(file="/xxxx/MASTER_accession_list_INFOADDED.csv",stringsAsFactors=FALSE)
repseqs<-repseqs[repseqs$RepSeq_97.5_Raw_28_Oct_2019 %in% "Yes",]
repseqs<-repseqs[,c("Organism_Acc_No","OTU_97.5_Raw_28_Oct_2019")]

#read back in mean summary data
taxa<-read.csv(file="/xxxx/OTU_97.5_BIOCLIM_SUMMARY_UNIQUES.csv",stringsAsFactors=FALSE,row.names=1)
taxa$RepSeq<-NA
for(x in 1:nrow(taxa)){
	taxa$RepSeq[x]<-repseqs$Organism_Acc_No[repseqs$OTU_97.5_Raw_28_Oct_2019 %in% rownames(taxa)[x]]
}

#reduce to those that only have bioclim data
tbc<-taxa[!is.na(taxa$Mean_Bio1),]

#find those that lack bioclim data
abs<-repseqs$Organism_Acc_No[!repseqs$Organism_Acc_No %in% tbc$RepSeq]
tr<-read.nexus(file="/xxxx/burnin25pcn_resampevery20k_mcc_medhts.tre")

#identify outgroup taxa to drop
outs<-c("Asterochloris_excentrica_AM905993","Asterochloris_gaertneri_AM905997","Asterochloris_phycobiontica_AM900490","Vulcanochloris_canariensis_KR952329","Vulcanochloris_guanchorum_KR952330","Vulcanochloris_symbiotica_KR952322")

#add those lacking bioclim data to the outgroup batch
outs<-c(outs,abs)

#drop tips lacking bioclim data and outgroups
tr<-drop.tip(tr,outs)

#change names to that of OTU's
for(x in 1:length(tr$tip.label)){
	tr$tip.label[tr$tip.label %in% tr$tip.label[x]]<-repseqs$OTU_97.5_Raw_28_Oct_2019[repseqs$Organism_Acc_No %in% tr$tip.label[x]]
}
#write trees
write.nexus(tr,file="/xxxx/burnin25pcn_resampevery20k_mcc_medhts_OTUs_renamed_bioclim_retained.tre")

trees<-read.nexus(file="/xxxx/burnin25pcn_resampevery20k.tre")

#drop those without data
cltr<-lapply(trees,drop.tip,outs)
class(cltr)<-"multiPhylo"

#rename tips
for(z in 1:length(cltr)){
	for(x in 1:length(cltr[[z]]$tip.label)){
		cltr[[z]]$tip.label[cltr[[z]]$tip.label %in% cltr[[z]]$tip.label[x]]<-repseqs$OTU_97.5_Raw_28_Oct_2019[repseqs$Organism_Acc_No %in% cltr[[z]]$tip.label[x]]
	}
}

#write trees
write.nexus(cltr,file="/xxxx/burnin25pcn_resampevery20k_OTUs_renamed_bioclim_retained.tre")


#read sample of trees
trees<-read.nexus(file="/xxxx/burnin25pcn_resampevery20k_OTUs_renamed_bioclim_retained.tre")

#randomly select 1000 values
samps<-sample(1:length(trees),1000,replace=F)
#> samps
#    [1] 4184 6363 2756 2320 2230 3985 3551 6616 2567 6375 4214 6590  680 6106
# [15] 1681 4322 6423  104  832 1154 3776 2889 4665 2969 2259 3158 4846 4099
# [29] 2888 4430 6504 6424 3583 5645  322 2149 3488  206 3297 1550 5331 2489
# [43] 6567 1597 6120 4512 6200 4807  800 6291 6549  864 4387 1174 5832 1403
# [57] 5559 2339 3695  359 2241 5474 5467 4433  231 5783 1680 6234 5810 5179
# [71] 1522 5314 5191 2519 6133 2813 6405 1489 3639 5636 1554 4546 3802  745
# [85]  319 4631 1526 5243 4809 1185 3584 4239 1370 2399 3044 3231 2435 5346
# [99] 5695 4424  488 2581 4680  970  567 4811 5064 1496 5595 3867 3865 3336
#[113] 6071  204 2806 4068 5897 1523 2914 1273 3007 5011 6586 1951 1590 1002
#[127] 5661 1107 2890 5260 1014 4837 1254 3588 1737    6 6431 4358 3406 1517
#[141] 4533 2860 2915 5809 2044 1930 5890 4548 3644 2447 2781 2840 5628 3405
#[155] 2068 4449 6679 2759 3051  487 4190 2464 6028 3825 2555 5733 4501 6535
#[169] 6615 2520 2051 3203 2664 4831 3909 3148 5294 2353  288 3090 4867 1562
#[183] 3859 1830 1974  121 1396 1817 4026 1869 4605 5329 6584 1290  900  552
#[197] 4388 2762 2835 5824 3629 2212 4870 1502 2770 2457 5240 5333 4191 3176
#[211]   26 5170 3462 2234 5864 2065 4452 2118 5552 6100 1925 3296 1567 3125
#[225] 1343 3580  112 4669  304 4357 2673  199  989  530 4499 1393 4021  529
#[239] 5319  111 6305  611 1309 5609 6636 2378 6217 4227 2790 2900 3254   16
#[253] 5347 4936 1033 5449 4676 5989   50 6292 2853 5273 5100 3693 4999 5556
#[267] 2412 4691 4461 3392 2628 4202 1614  467 1472 6653 3754  355 1023 6029
#[281] 1100 1709 2409 5081 3806 6737 5215 2548 1659  944 2503 3997 2215 2005
#[295] 5780 1271 4000 2596 2423 2446 1024 2849 5539 5197 6359 3717 1602 5150
#[309] 1978 1284 1862 5262 6556 6500 4329 1442 5304 5970 6408 1961 4004 3056
#[323] 1847 6575 2600 2326 3335 6122 3220 2527 6545 5324 4920  197 2731 5978
#[337] 1777 4259 4047 5835 5043 5171 2177  803 4041 3513 3347 2560 6694 1746
#[351] 4822 2911  651 4513 1949 5503 5153 2316 4292 5040 4172  825 5705  665
#[365]  448  146 4108  418 4842 6266 5511 5555 5362 5138 2868  858 3829 3303
#[379] 5075 4997 5882 4393 3507 6378 1473 1422  307 1131 4210 4283 6583 1402
#[393] 2933 1613 4946  804 5922  619  571 2690 4576 3475 4859 1654 3845 5652
#[407] 5419 5967 4909 2356  888 1178 2119 6728 4351 1204 5025  802   56  796
#[421] 1557 4663 6347   11 4305 3266 4656 2742  855 4237 1269 5027 4598 4196
#[435] 3655 3521 4751 4025 1125 3725 5045 1097  593 6674 5514 2482  828 4955
#[449] 3991 3029 2245 4030  401 3037 3541 3078 1648  150 1115 4525 4197 4933
#[463] 4678 1201  168 2738 6110 6665  627 2258 3775 4904 5062 5208 6522 1082
#[477] 5854  174   24 2970 4162 3418 5002 3363 4205 2963 4352 4897 1467 1190
#[491] 5879 6617  498 4093 2098  312 3626 2144 1012 2085 6268 3700 1695 6660
#[505]  883 5693 4314 6695  485 6301 6058 2121 4991 5159  750 5755 6207 1585
#[519] 2566 5793 2114 6406 3674  160 6208 4464 2452 6092 5135 4182  747 4555
#[533] 4468 5061 6135 5345 2674 3497 4544 3031 3963 2202  616 1917 6330 2847
#[547] 4343 6121   79 4273 3587 2884 2042 4254 4475 6310 5696 5432  455 5444
#[561] 5632 6095 5350 1101 5309 4970 1087 1624 4947 1596 5423 6072  451 5966
#[575] 1070 1726 1732 3958 6331 4300 2469 4764  836 4503 4559 4016 2668 5713
#[589] 1074 4164 2337 4200   86  902 5082  280 5445 4716 3543 6317 2232 5253
#[603] 2007 3494 3770  183 2122 6143 1668 3000  521 6018 6502 4612 3875 3635
#[617] 1696 3493 6001 2338 6098 5841 3628 3834 3811 4427 4718 6428 3675 1861
#[631]  556  801  486 4736 2620 5293 5506 6279 4133 5079 4455 2006 3224 3484
#[645] 5908 2549 4752 2815 1045  373 1705 5576 2445 6227 4372 1179 3167 3623
#[659] 1462 3872 4130 5065 3287 1660  925 2282 5246 2724 2546 2592   60 5984
#[673] 3529 2208 1446 6470 4391 3127 3525 6289  176 3237 2205 1658  838  314
#[687] 1764 1459 4115 4412 1570  860  899 5883 5389 6478  273 2180 1310 6293
#[701] 4436  421 1432 4623 5972 5131  889 2550 3245 5156 3429   46 6682 5433
#[715] 3046 4827 2247 2948 4111 6518 1745  672 6213 4806 5521 4245 2287 4331
#[729] 3823 2166    8 1841 4019 3954 5202  776 1300 4493 3972 4540 1638 5291
#[743] 5664 3317 5440 4924 5192 4810  632 5093 3467  768 6704 2328 4692  782
#[757] 1649 4952 3650 3975 1943 3080 1198 5098 5405 4232  207 5014 6418  628
#[771]  920 5694 6365  644 2879 5311 3063 1863 6011 3341 2978 1663 2523 6516
#[785] 5282 6739 5070 2455 3964 2211 2083 1763  859 5593 4820 1976 3153 6081
#[799]   20 3079 4679 6662 5393 5162 6249 4960 5934 4007   74 2389 3818  669
#[813] 5148 1928 4440 5041 6150 5740 3576 3217 5781 5760 2461 4732 2488 2855
#[827] 2938 2803 4601 2015 6283 4672 4749 1109 5365 3249 2660 4800 2988  364
#[841] 1406  390 1363 1059 6438 5646 1975 1312  878 1052 3074  334  588 6467
#[855] 1593 5176 4120  405 3139 4879 3256  610 2828 4063 5578 1352 4057 2996
#[869] 6698 2456  725 4173 3798  459 6101 6538 1083 1750 4136 3606 5248  887
#[883] 2809 2982  177 1440 1536 5245 2782 3752 5443 2261 5798 1824  330  605
#[897] 3464 3420  916 6634 1871 3469 6532 5568 6344 1607 2168 2525 1463 5885
#[911]  578 6710 3061 3052 5408 2755 2109  123 2363  685 3381  586 1088 2579
#[925] 4957 2329 2236  201 2413 3531 2957 1367 1047 2243 1127 6601 3334 1699
#[939] 6511 2569 1202  549 3951 1912 2607  524 6247 3200 4294 4773 4812 1118
#[953]  743 1456 5583 4629 4914  575 2757 1337 1592 2324 6664 3574 2193 1346
#[967] 1873 6453 4092 6065  192 4739 4783 5332 6336 5920 5232 4028 5842 5963
#[981] 4981 1606 1762 4637 1019 1162 6259 5034   81 6721 5095 2518  494 6300
#[995] 1294 3701 6745 2974 1690  119
 
#subsample 1000 trees
thoustr<-trees[samps]

#write trees
write.nexus(thoustr,file="/xxxx/burnin25pcn_resampevery20k_OTUs_renamed_bioclim_retained_1krandomlyselected.trees")

#library(phytools)
#library(devtools)
# isolate one variable from data frame
#bio1<-tbc$bio1
#names(bio1)<-rownames(tbc)
#plot phylogeny
#obj<-contMap(tr,bio1,plot=FALSE)
#n<-length(obj$cols)
#obj$cols[1:n]<-colorRampPalette(c("blue","red"), space="Lab")(n)
#plotTree.wBars(obj$tree,bio1,method='plotSimmap',tip.labels=TRUE,fsize=0.5,colors=obj$cols,type='fan',scale=0.2)
#add.color.bar(125.0,obj$cols,fsize=0.7,title='trait value',lims=obj$lims,prompt=FALSE,x=0.9*par()$usr[1], y=0.9*par()$usr[3])



#NOW NEED TO DROP OTUS that were merged
tr<-read.nexus(file="/xxxx/burnin25pcn_resampevery20k_mcc_medhts_OTUs_renamed_bioclim_retained.tre")

#identify outgroup taxa to drop
outs<-c("A_97.5_56","A_97.5_60","A_97.5_24","A_97.5_27")

#drop tips lacking bioclim data and outgroups
tr<-drop.tip(tr,outs)

#write trees
write.nexus(tr,file="/xxxx/burnin25pcn_resampevery20k_mcc_medhts_OTUs_renamed_bioclim_retained_MODIFIED.tre")

trees<-read.nexus(file="/xxxx/burnin25pcn_resampevery20k_OTUs_renamed_bioclim_retained_1krandomlyselected.trees")

#drop those without data
cltr<-lapply(trees,drop.tip,outs)
class(cltr)<-"multiPhylo"

#write trees
write.nexus(cltr,file="/xxxx/burnin25pcn_resampevery20k_OTUs_renamed_bioclim_retained_1krandomlyselected_MODIFIED.trees")




######################
######################
######################
######################

#Phylogenetic PCA
require(phytools)
#read back in summary data
taxa<-read.csv(file="/xxxx/OTU_97.5_BIOCLIM_SUMMARY_MODIFIED_UNIQUES.csv",stringsAsFactors=FALSE,row.names=1)

#reduce to those that only have bioclim data
tbc<-taxa[!is.na(taxa$Mean_Bio1),]

#read and clean mcc tree
tr<-read.nexus(file="/xxxx/burnin25pcn_resampevery20k_mcc_medhts_OTUs_renamed_bioclim_retained_MODIFIED.tre")

#make data into a matrix (instead of data frame)
bio.mat<-as.matrix(tbc[,paste("Mean_Bio",1:19,sep="")])

#change column names
colnames(bio.mat)<-gsub("Mean_B","b",colnames(bio.mat))

#make column for clade names
tbc$Clade<-NA
for(x in 1:nrow(tbc)){
	tbc$Clade[x]<-strsplit(rownames(tbc)[x],"_")[[1]][1]
}

#change clade G to C
tbc$Clade[tbc$Clade=="G"]<-"C"

#do ppca
ppca<-phyl.pca(tr, bio.mat, method="BM", mode="corr")

summary(ppca)
ppca
write.table(ppca$Eval,file="/xxxx/ppca.eval.MODIFIED_UNIQUES.csv",sep=",",col.names=NA)
write.table(ppca$Evec,file="/xxxx/ppca.evec.MODIFIED_UNIQUES.csv",sep=",",col.names=NA)
write.table(round(ppca$Evec,2),file="/xxxx/ppca.evec.rounded.MODIFIED_UNIQUES.csv",sep=",",col.names=NA)
write.table(ppca[3],file="/xxxx/ppca.s.MODIFIED_UNIQUES.csv",sep=",",col.names=NA)
write.table(ppca[4],file="/xxxx/ppca.l.MODIFIED_UNIQUES.csv",sep=",",col.names=NA)
write.table(bio.mat,file="/xxxx/bioclim.MODIFIED_UNIQUES.csv",sep=",",col.names=NA)
class(ppca)<-"prcomp"
names(ppca)[4]<-"rotation"
names(ppca)[3]<-"x"
ppca$sdev<-unname(sqrt(diag(ppca$Eval)))
junk<-dist(ppca$x,method="euclidean")
junk.mat<-as.matrix(junk)

#biplot(ppca)

#library(devtools)
#install_github("vqv/ggbiplot")
library(ggbiplot)
#all.clades12<-ggbiplot(ppca,choices=1:2,scale=0,groups=tbc$Clade,ellipse=TRUE,alpha=0.75,linetype="blank", ellipse.prob = 0.95)+theme_bw()+xlim(-90,90)+ylim(-50,50)+theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+guides(color = guide_legend(override.aes = list(linetype = 0,size=3)))+labs(title="A.)  PC1 vs. PC2")+theme(plot.title=element_text(size=10,face="bold"))
#all.clades12
#all.clades13<-ggbiplot(ppca,choices=c(1,3),scale=0,groups=tbc$Clade,ellipse=TRUE,alpha=0.75,linetype="blank", ellipse.prob = 0.95)+theme_bw()+xlim(-90,90)+ylim(-50,50)+theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+guides(color = guide_legend(override.aes = list(linetype = 0,size=3)))+labs(title="B.)  PC1 vs. PC3")+theme(plot.title=element_text(size=10,face="bold"))
#all.clades13
#all.clades23<-ggbiplot(ppca,choices=2:3,scale=0,groups=tbc$Clade,ellipse=TRUE,alpha=0.75,linetype="blank", ellipse.prob = 0.95)+xlim(-90,90)+ylim(-50,50)+theme_bw()+theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+guides(color = guide_legend(override.aes = list(linetype = 0,size=3)))+labs(title="C.)  PC2 vs. PC3")+theme(plot.title=element_text(size=10,face="bold"))
#all.clades23

my.cb<-c("#543005", "#8E0152","#F0E442","#0072B2")
all.clades12<-ggbiplot.mod(ppca,choices=1:2,scale=0,groups=tbc$Clade,alpha=0.75,linetype="blank",var.col="darkgrey")+theme_bw()+xlim(-90,90)+ylim(-50,50)+theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+guides(color = guide_legend(title="Clade",override.aes = list(linetype = 0,size=3)))+labs(title="PC1 vs. PC2")+theme(plot.title=element_text(size=10,face="bold",hjust=0.5))+scale_colour_manual(values=my.cb)
all.clades12
all.clades13<-ggbiplot.mod(ppca,choices=c(1,3),scale=0,groups=tbc$Clade,alpha=0.75,linetype="blank",var.col="darkgrey")+theme_bw()+xlim(-90,90)+ylim(-50,50)+theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+guides(color = guide_legend(title="Clade",override.aes = list(linetype = 0,size=3)))+labs(title="PC1 vs. PC3")+theme(plot.title=element_text(size=10,face="bold",hjust=0.5))+scale_colour_manual(values=my.cb)
all.clades13
all.clades23<-ggbiplot.mod(ppca,choices=2:3,scale=0,groups=tbc$Clade,alpha=0.75,linetype="blank",var.col="darkgrey")+theme_bw()+xlim(-90,90)+ylim(-50,50)+theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+guides(color = guide_legend(title="Clade",override.aes = list(linetype = 0,size=3)))+labs(title="PC2 vs. PC3")+theme(plot.title=element_text(size=10,face="bold",hjust=0.5))+scale_colour_manual(values=my.cb)
all.clades23

require(gridExtra)
png("/xxxx/ppca_MODIFIED_UNIQUES.png",width=11,height=2.1,units="in",res=600)
grid.arrange(all.clades12,all.clades13,all.clades23,nrow=1)
dev.off()



#IF previously saved ppca
loadings<-read.csv(file="/xxxx/ppca.l.MODIFIED_UNIQUES.csv",header=TRUE,row.names=1,stringsAsFactors=FALSE)
colnames(loadings)<-gsub("L.","",colnames(loadings))
ppcaPC.all<-read.csv(file="/xxxx/ppca.s.MODIFIED_UNIQUES.csv",header=TRUE,row.names=1,stringsAsFactors=FALSE)
colnames(ppcaPC.all)<-gsub("S.","",colnames(ppcaPC.all))
eval<-read.csv(file="/xxxx/ppca.eval.MODIFIED_UNIQUES.csv",header=TRUE,row.names=1,stringsAsFactors=FALSE)
evec<-read.csv(file="/xxxx/ppca.evec.MODIFIED_UNIQUES.csv",header=TRUE,row.names=1,stringsAsFactors=FALSE)
ppca<-list()
ppca$rotation<-loadings
ppca$x<-as.data.frame(ppcaPC.all)
ppca$sdev<-unname(sqrt(diag(as.matrix(eval))))
class(ppca)<-"prcomp"
rownames(ppca$rotation)<-1:19
colnames(ppca$x)<-1:19


require(phytools)
#read back in summary data
taxa<-read.csv(file="/xxxx/OTU_97.5_BIOCLIM_SUMMARY_MODIFIED_UNIQUES.csv",stringsAsFactors=FALSE,row.names=1)

#reduce to those that only have bioclim data
tbc<-taxa[!is.na(taxa$Mean_Bio1),]

#read and clean mcc tree
tr<-read.nexus(file="/xxxx/burnin25pcn_resampevery20k_mcc_medhts_OTUs_renamed_bioclim_retained_MODIFIED.tre")

#make data into a matrix (instead of data frame)
bio.mat<-as.matrix(tbc[,paste("Mean_Bio",1:19,sep="")])

#change column names
colnames(bio.mat)<-gsub("Mean_B","b",colnames(bio.mat))

#make column for clade names
tbc$Clade<-NA
for(x in 1:nrow(tbc)){
	tbc$Clade[x]<-strsplit(rownames(tbc)[x],"_")[[1]][1]
}

#change clade G to C
tbc$Clade[tbc$Clade=="G"]<-"C"


library(ggbiplot)
my.cb<-c("#543005", "#8E0152","#F0E442","#0072B2")
all.clades12<-ggbiplot.mod(ppca,choices=1:2,scale=0,groups=tbc$Clade,alpha=0.75,linetype="blank",var.col="darkgrey")+theme_bw()+xlim(-70,90)+ylim(-45,35)+theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+guides(color = guide_legend(title="Clade",override.aes = list(linetype = 0,size=3)))+labs(title="PC1 vs. PC2")+theme(plot.title=element_text(size=10,face="bold",hjust=0.5))+scale_colour_manual(values=my.cb)
png("/xxxx/ppca_MODIFIED_UNIQUES_PC1vsPC2.png",width=7,height=4,units="in",res=600)
all.clades12
dev.off()
all.clades13<-ggbiplot.mod(ppca,choices=c(1,3),scale=0,groups=tbc$Clade,alpha=0.75,linetype="blank",var.col="darkgrey")+theme_bw()+xlim(-70,90)+ylim(-25,25)+theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+guides(color = guide_legend(title="Clade",override.aes = list(linetype = 0,size=3)))+labs(title="PC1 vs. PC3")+theme(plot.title=element_text(size=10,face="bold",hjust=0.5))+scale_colour_manual(values=my.cb)
png("/xxxx/ppca_MODIFIED_UNIQUES_PC1vsPC3.png",width=7,height=4,units="in",res=600)
all.clades13
dev.off()
all.clades23<-ggbiplot.mod(ppca,choices=2:3,scale=0,groups=tbc$Clade,alpha=0.75,linetype="blank",var.col="darkgrey")+theme_bw()+xlim(-45,35)+ylim(-25,25)+theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+guides(color = guide_legend(title="Clade",override.aes = list(linetype = 0,size=3)))+labs(title="PC2 vs. PC3")+theme(plot.title=element_text(size=10,face="bold",hjust=0.5))+scale_colour_manual(values=my.cb)
png("/xxxx/ppca_MODIFIED_UNIQUES_PC2vsPC3.png",width=7,height=4,units="in",res=600)
all.clades23
dev.off()

#########################################
#########################################
#########################################
#########################################
#########################################
#########################################
#CONVERT TO OPEN/CLOSED HABITATS AND CONDUCT ASR and leave state missing for taxa lacking info

#read in data file
missing<-read.csv(file="/xxxx/MASTER_accession_list_INFOADDED_MODIFIED.csv",stringsAsFactors=FALSE)

#retain seqs that passed criteria
keeps<-missing[!is.na(missing$OTU_97.5_Raw_28_Oct_2019_Modified) &!is.na(missing$LatC),]

#number passing quality filters
nrow(keeps)

#looks like all w coords have climatic data
keeps[!is.na(keeps$LatC) & is.na(keeps$bio1),]

#might not be same for biomes (some could be close to water (looks like 72 accessions lack biome info)
nrow(keeps[!is.na(keeps$LatC) & is.na(keeps$Biome),])

keeps<-keeps[!is.na(keeps$LatC) & !is.na(keeps$Biome),]


#make data frame
require(gtools) #for mixedsort function
clades<-c("A","I","G","S")
new.bios<-paste(LETTERS[1:14],"Pcnt",sep="")
cols<-c("OTU","No_Sequences","No_Unique_Sites","No_Unique_Sites_Seqs",new.bios)
combo<-data.frame(matrix(nrow=0,ncol=length(cols)))
colnames(combo)<-cols

for(x in 1:length(clades)){
	wco<-NULL
	wco<-keeps[keeps$Clade %in% clades[x],]
	temp<-NULL
	temp<-data.frame(matrix(nrow=length(unique(wco$OTU_97.5_Raw_28_Oct_2019_Modified)),ncol=length(cols),0))
	colnames(temp)<-cols
	temp$OTU<-mixedsort(unique(wco[wco$Clade %in% clades[x],"OTU_97.5_Raw_28_Oct_2019_Modified"]))
	temp.tab<-table(wco[wco$Clade %in% clades[x],"OTU_97.5_Raw_28_Oct_2019_Modified"])
	for(z in 1:length(temp.tab)){
		temp$No_Sequences[temp$OTU %in% names(temp.tab)[z]]<-temp.tab[[z]][1]
		temp$No_Unique_Sites[temp$OTU %in% names(temp.tab)[z]]<-nrow(unique(wco[wco$OTU_97.5_Raw_28_Oct_2019_Modified %in% names(temp.tab)[z],c("LatC","LongC")]))
	}
	for(y in 1:nrow(temp)){	
		for(q in 1:14){
			sub.otu<-NULL
			sub.otu<-keeps[keeps$OTU_97.5_Raw_28_Oct_2019_Modified %in% temp$OTU[y],]
			sub.otu<-sub.otu[!duplicated(sub.otu[,c("LatC","LongC")]),]
			temp[y,"No_Unique_Sites_Seqs"]<-nrow(sub.otu)
			temp[y,new.bios[q]]<-100*(sum(sub.otu$Biome==q)/nrow(sub.otu))
		}
	}	
	combo<-rbind(combo,temp)
}		

open<-c("GPcnt","HPcnt","IPcnt","JPcnt","KPcnt","MPcnt")
closed<-c("APcnt","BPcnt","CPcnt","DPcnt","EPcnt","NPcnt","FPcnt","LPcnt")

combo$Open<-0
combo$Closed<-0

for(x in 1:nrow(combo)){
	if(sum(combo[x,open])>33.3){
		combo$Open[x]<-1
	}
	if(sum(combo[x,closed])>33.3){
		combo$Closed[x]<-1
	}
}

combo$habcod<-NA

for(x in 1:nrow(combo)){
	if(combo$Open[x]==0 & combo$Closed[x]==1){
		combo$habcod[x]<-0
	}
	if(combo$Open[x]==1 & combo$Closed[x]==0){
		combo$habcod[x]<-1
	}
	if(combo$Open[x]==1 & combo$Closed[x]==1){
		combo$habcod[x]<-"0&1"
	}
}


combo$habcodthree<-NA
for(x in 1:nrow(combo)){
	if(combo$Open[x]==0 & combo$Closed[x]==1){
		combo$habcodthree[x]<-0
	}
	if(combo$Open[x]==1 & combo$Closed[x]==0){
		combo$habcodthree[x]<-2
	}
	if(combo$Open[x]==1 & combo$Closed[x]==1){
		combo$habcodthree[x]<-1
	}
}


#means they rely exclusively on forests vs others
combo$habcodclosedvsmixedopen<-NA
for(x in 1:nrow(combo)){
	if(combo$Open[x]==0 & combo$Closed[x]==1){
		combo$habcodclosedvsmixedopen[x]<-0
	}
	if(combo$Open[x]==1 & combo$Closed[x]==0){
		combo$habcodclosedvsmixedopen[x]<-1
	}
	if(combo$Open[x]==1 & combo$Closed[x]==1){
		combo$habcodclosedvsmixedopen[x]<-1
	}
}

#means they rely exclusively on forests vs others
combo$habcodopenvsmixedclosed<-NA
for(x in 1:nrow(combo)){
	if(combo$Open[x]==0 & combo$Closed[x]==1){
		combo$habcodopenvsmixedclosed[x]<-0
	}
	if(combo$Open[x]==1 & combo$Closed[x]==0){
		combo$habcodopenvsmixedclosed[x]<-1
	}
	if(combo$Open[x]==1 & combo$Closed[x]==1){
		combo$habcodopenvsmixedclosed[x]<-0
	}
}

tr<-read.tree(file="/xxxx/burnin25pcn_resampevery20k_mcc_medhts_OTUs_renamed_bioclim_retained_MODIFIED_ladderized.tre")
missing<-tr$tip.label[!tr$tip.label %in% combo$OTU]

empty<-combo[1:length(missing),]
empty[1:nrow(empty),1:ncol(empty)]<-NA
empty$OTU<-missing
empty[,c("Open","Closed","habcod","habcodthree","habcodclosedvsmixedopen","habcodopenvsmixedclosed")]<-"?"

combo<-rbind(combo,empty)

#save combo file
write.csv(combo,file="/xxxx/OTU_97.5_BIOCLIM_SUMMARY_MODIFIED_UNIQUES_vegetative_regimes.csv",row.names=FALSE)


#########
#habcodopenvsmixedclosed


#ASR
require(corHMM)
#read mcc
tr<-read.tree(file="/xxxx/asr_2state_vegetative_regime_openvsmixedclosed/burnin25pcn_resampevery20k_mcc_medhts_OTUs_renamed_bioclim_retained_MODIFIED_ladderized.tre")

#read sample 
trees<-read.nexus(file="/xxxx/asr_2state_vegetative_regime_openvsmixedclosed/burnin25pcn_resampevery20k_OTUs_renamed_bioclim_retained_1krandomlyselected_MODIFIED.trees")

#add mcc tree to end of trees (make tree 1001)
trees<-c(trees,tr)

#now...
combo<-read.csv(file="/xxxx/asr_2state_vegetative_regime_openvsmixedclosed/OTU_97.5_BIOCLIM_SUMMARY_MODIFIED_UNIQUES_vegetative_regimes.csv",stringsAsFactors=FALSE)


dat<-combo[!is.na(combo$habcodopenvsmixedclosed),c("OTU","habcodopenvsmixedclosed")]
rownames(dat)<-dat$OTU

path<-"/xxxx/asr_2state_vegetative_regime_openvsmixedclosed/"

fit.mods<-function(trees,dat){
	#make empty data frame
	cols<-c("Tree","Root_State","Oldest_0","Oldest_1")
	df<-as.data.frame(matrix(nrow=1001,ncol=length(cols)))
	colnames(df)<-cols
	df$Tree[1:1000]<-1:1000
	df$Tree[1001]<-"MCC"
	for(x in 1:nrow(df)){
		pp1<-rayDISC(trees[[x]],dat,ntraits=1,charnum=1,model="ARD",node.states="marginal",root.p="maddfitz",lewis.asc.bias=FALSE)
		save(pp1,file=paste(path,"pp2s_vegetative_regime_openvsmixedclosed_",df$Tree[x],".Rsave",sep=""))
		pp1.temp<-as.data.frame(pp1$states)
		pp1.temp$Age<-NA
		#get ages of each node
		pp1.temp$Age<-branching.times(pp1$phy)

		#get states
		pp1.temp$State<-NA
		#for each row, find most likely state, and if ties, select the first value.  Subtract by 1 because states are 0-2, vs 1-3
		pp1.temp$State<-max.col(pp1.temp[1:2],ties.method=("first"))-1

		#root state
		df$Root_State[x]<-pp1.temp$State[1]

		#get oldest 0 value
		df$Oldest_0[x]<-max(pp1.temp[pp1.temp$State==0,"Age"])

		#get oldest 1 value
		df$Oldest_1[x]<-max(pp1.temp[pp1.temp$State==1,"Age"])

		#turn back to null...
		pp1.temp<-NULL
		pp1<-NULL
	}
return(df)
}

twos.summary<-fit.mods(trees=trees,dat=dat)
write.csv(twos.summary,paste(path,"pp2s_vegetative_regime_summary_openvsmixedclosed.csv",sep=""),row.names=FALSE)


only keep those that are monophyletic findMRCA(tree,tips=c(),type="node") of clade and then get descendent getDescendants(tree,node) node numbers and label


biome.summary<-read.csv(file=paste(path,"pp2s_vegetative_regime_summary_openvsmixedclosed.csv",sep=""),stringsAsFactors=FALSE)


#Root state
require("HDInterval")
table(biome.summary$Root_State[1:1000])
round(min(biome.summary$Oldest_0[1:1000]),2)
round(mean(biome.summary$Oldest_0[1:1000]),2)
round(max(biome.summary$Oldest_0[1:1000]),2)
round(sd(biome.summary$Oldest_0[1:1000]),2)
hdi(biome.summary$Oldest_0[1:1000],credMas=0.95)


round(min(biome.summary$Oldest_1[1:1000]),2)
round(mean(biome.summary$Oldest_1[1:1000]),2)
round(max(biome.summary$Oldest_1[1:1000]),2)
round(sd(biome.summary$Oldest_1[1:1000]),2)
hdi(biome.summary$Oldest_1[1:1000],credMas=0.95)

require(corHMM)
require(scales)
require(phyloch)

load("~/trebouxia_vortex/asr_2state_vegetative_regime_openvsmixedclosed/pp2s_vegetative_regime_openvsmixedclosed_MCC.Rsave")

dat<-read.csv(file="/xxxx/asr_2state_vegetative_regime_openvsmixedclosed/OTU_97.5_BIOCLIM_SUMMARY_MODIFIED_UNIQUES_vegetative_regimes.csv",stringsAsFactors=FALSE)
tips.temp<-dat$habcodopenvsmixedclosed[order(match(dat$OTU,pp1$phy$tip.label))]
tips.temp[tips.temp %in% "0"]<-"dark green"
tips.temp[tips.temp %in% "1"]<-"yellow"
tips.temp[tips.temp %in% "?"]<-"white"


clades<-list(pp1$phy$tip.label[grep("A_",pp1$phy$tip.label)],pp1$phy$tip.label[grep("G_",pp1$phy$tip.label)],pp1$phy$tip.label[grep("I_",pp1$phy$tip.label)],pp1$phy$tip.label[grep("S_",pp1$phy$tip.label)])
mycols<-c("#543005", "#8E0152","#F0E442","#0072B2")
mycols<-alpha(mycols,0.75)
cld.cols<-edge.color(pp1$phy,clades,col = mycols)

png("/xxxx/trebouxia_975_vegetative_regime_asr_2state_UNIQUES_29jan2020_habcodopenvsmixedclosed.png",width=6,height=11,units="in",res=2400)
plot(pp1$phy,cex=0.8,edge.color=cld.cols,edge.width=3)
nodelabels(pie=pp1$states,cex=0.5,piecol=c("dark green","yellow"))
legend(x=-6,y=70,legend=c("A (45 OTU's)","G (14 OTU's)", "I (10 OTU's)", "S (12 OTU's)"),lty=1,col=mycols,cex=1,bty="n",lwd=8,inset=0.05)
text(x=-3,y=70,"Clade",cex=1,font=2,adj=c(0,0))

legend(x=-6,y=82,c("Forested, Forested & Non-Forested","Non-Forested"),col=c("dark green","yellow"),cex=1,bty="n",pt.cex=2,pch=19,inset=0.05)
text(x=-3,y=82,"Vegetative Regime",cex=1,font=2,adj=c(0,0))

append2tips(pp1$phy,pch=19,col=tips.temp,align=TRUE,cex=1)

axisPhylo()

title(xlab="MY Before Present",adj=0.5)

dev.off()



######################
#Read back in ASR for vegetative regime and get stats for individual clades
######################


#######################
#habcodopenvsmixedclosed


#ASR
require(corHMM)
require(phytools)
require(phangorn)

path<-"/xxxx/asr_2state_vegetative_regime_openvsmixedclosed/"
	
	clades<-c("A","C","I","S")
	load(file=paste(path,"pp2s_vegetative_regime_openvsmixedclosed_",df$Tree[1],".Rsave",sep=""))
	tips<-pp1$phy$tip.label
	atip<-tips[grep("A_",tips)]
	ctip<-tips[grep("G_",tips)]
	itip<-tips[grep("I_",tips)]
	stip<-tips[grep("S_",tips)]
	

	#make empty data frame
	cols<-c("Tree","Root_State","Oldest_0","Oldest_1","A_Mono","C_Mono","I_Mono","S_Mono","A_State","C_State","I_State","S_State","A_Age","C_Age","I_Age","S_Age","A_Oldest_0","C_Oldest_0","I_Oldest_0","S_Oldest_0","A_Oldest_1","C_Oldest_1","I_Oldest_1","S_Oldest_1")
	df<-as.data.frame(matrix(nrow=1001,ncol=length(cols)))
	colnames(df)<-cols
	df$Tree[1:1000]<-1:1000
	df$Tree[1001]<-"MCC"
	pp1<-NULL
	for(x in 1:nrow(df)){
		load(file=paste(path,"pp2s_vegetative_regime_openvsmixedclosed_",df$Tree[x],".Rsave",sep=""))
		pp1.temp<-as.data.frame(pp1$states)
		pp1.temp$Age<-NA
		#get ages of each node
		pp1.temp$Age<-branching.times(pp1$phy)

		#get states
		pp1.temp$State<-NA
		#for each row, find most likely state, and if ties, select the first value.  Subtract by 1 because states are 0-2, vs 1-3
		pp1.temp$State<-max.col(pp1.temp[1:2],ties.method=("random"))-1

		#root state
		df$Root_State[x]<-pp1.temp$State[1]

		#get oldest 0 value
		df$Oldest_0[x]<-max(pp1.temp[pp1.temp$State==0,"Age"])

		#get oldest 1 value
		df$Oldest_1[x]<-max(pp1.temp[pp1.temp$State==1,"Age"])


		#findMRCA(pp1$phy,tips=atip,type="node")
		#find mrca of A
		anode<-findMRCA(pp1$phy,tips=atip,type="node")
		#get descendents from this node
		a.des<-pp1$phy$tip.label[Descendants(pp1$phy,node=anode,type="tips")[[1]]]
		#if all of the descendents are in clade A put TRUE (otherwise FALSE)
		df$A_Mono[x]<-all(a.des %in% atip)
		#get state for that node, but need to subtract number of tips
		df$A_State[x]<-pp1.temp$State[anode-length(pp1$phy$tip.label)]
		#get age for that node
		df$A_Age[x]<-pp1.temp$Age[anode-length(pp1$phy$tip.label)]
		#get ALL descendents and subtract number of tips
		ades.nodes<-Descendants(pp1$phy,node=anode,type="all")-length(pp1$phy$tip.label)
		#drop tips
		ades.nodes<-ades.nodes[ades.nodes>0]
		#subset pp1.temp to only include these nodes
		a.temp<-pp1.temp[ades.nodes,]
		#if mrca is in state 0, then just use that age
		if(df$A_State[x]==0){
			df$A_Oldest_0[x]<-df$A_Age[x]
		}
		#if mrca is NOT in state 0, then find oldest age of State 0 within clade A
		if(df$A_State[x]!=0){
			#if any nodes in clade have state 0 (leave as NA if state 0 not present in nodes)
			if(any(a.temp$State==0)){
				df$A_Oldest_0[x]<-max(a.temp[a.temp$State==0,"Age"])
			}
		}
		if(df$A_State[x]==1){
			df$A_Oldest_1[x]<-df$A_Age[x]
		}
		#if mrca is NOT in state 1, then find oldest age of State 1 within clade A
		if(df$A_State[x]!=1){
			#if any nodes in clade have state 1 (leave as NA if state 1 not present in nodes)
			if(any(a.temp$State==1)){
				df$A_Oldest_1[x]<-max(a.temp[a.temp$State==1,"Age"])
			}
		}
		a.temp<-NULL


		#find mrca of C
		cnode<-findMRCA(pp1$phy,tips=ctip,type="node")
		#get descendents from this node
		c.des<-pp1$phy$tip.label[Descendants(pp1$phy,node=cnode,type="tips")[[1]]]
		#if all of the descendents are in clade C put TRUE (otherwise FALSE)
		df$C_Mono[x]<-all(c.des %in% ctip)
		#get state for that node, but need to subtract number of tips
		df$C_State[x]<-pp1.temp$State[cnode-length(pp1$phy$tip.label)]
		#get age for that node
		df$C_Age[x]<-pp1.temp$Age[cnode-length(pp1$phy$tip.label)]
		#get ALL descendents and subtract number of tips
		cdes.nodes<-Descendants(pp1$phy,node=cnode,type="all")-length(pp1$phy$tip.label)
		#drop tips
		cdes.nodes<-cdes.nodes[cdes.nodes>0]
		#subset pp1.temp to only include these nodes
		c.temp<-pp1.temp[cdes.nodes,]
		#if mrca is in state 0, then just use that age
		if(df$C_State[x]==0){
			df$C_Oldest_0[x]<-df$C_Age[x]
		}
		#if mrca is NOT in state 0, then find oldest age of State 0 within clade C
		if(df$C_State[x]!=0){
			#if any nodes in clade have state 0 (leave as NA if state 0 not present in nodes)
			if(any(c.temp$State==0)){
				df$C_Oldest_0[x]<-max(c.temp[c.temp$State==0,"Age"])
			}
		}
		if(df$C_State[x]==1){
			df$C_Oldest_1[x]<-df$C_Age[x]
		}
		#if mrca is NOT in state 1, then find oldest age of State 1 within clade C
		if(df$C_State[x]!=1){
			#if any nodes in clade have state 1 (leave as NA if state 1 not present in nodes)
			if(any(c.temp$State==1)){
				df$C_Oldest_1[x]<-max(c.temp[c.temp$State==1,"Age"])
			}
		}
		c.temp<-NULL
		
		
		
		
		#find mrca of I
		inode<-findMRCA(pp1$phy,tips=itip,type="node")
		#get descendents from this node
		i.des<-pp1$phy$tip.label[Descendants(pp1$phy,node=inode,type="tips")[[1]]]
		#if all of the descendents are in clade I put TRUE (otherwise FALSE)
		df$I_Mono[x]<-all(i.des %in% itip)
		#get state for that node, but need to subtract number of tips
		df$I_State[x]<-pp1.temp$State[inode-length(pp1$phy$tip.label)]
		#get age for that node
		df$I_Age[x]<-pp1.temp$Age[inode-length(pp1$phy$tip.label)]
		#get ALL descendents and subtract number of tips
		ides.nodes<-Descendants(pp1$phy,node=inode,type="all")-length(pp1$phy$tip.label)
		#drop tips
		ides.nodes<-ides.nodes[ides.nodes>0]
		#subset pp1.temp to only include these nodes
		i.temp<-pp1.temp[ides.nodes,]
		#if mrca is in state 0, then just use that age
		if(df$I_State[x]==0){
			df$I_Oldest_0[x]<-df$I_Age[x]
		}
		#if mrca is NOT in state 0, then find oldest age of State 0 within clade I
		if(df$I_State[x]!=0){
			#if any nodes in clade have state 0 (leave as NA if state 0 not present in nodes)
			if(any(i.temp$State==0)){
				df$I_Oldest_0[x]<-max(i.temp[i.temp$State==0,"Age"])
			}
		}
		if(df$I_State[x]==1){
			df$I_Oldest_1[x]<-df$I_Age[x]
		}
		#if mrca is NOT in state 1, then find oldest age of State 1 within clade I
		if(df$I_State[x]!=1){
			#if any nodes in clade have state 1 (leave as NA if state 1 not present in nodes)
			if(any(i.temp$State==1)){
				df$I_Oldest_1[x]<-max(i.temp[i.temp$State==1,"Age"])
			}
		}
		i.temp<-NULL
	
		
		


		#find mrca of S
		snode<-findMRCA(pp1$phy,tips=stip,type="node")
		#get descendents from this node
		s.des<-pp1$phy$tip.label[Descendants(pp1$phy,node=snode,type="tips")[[1]]]
		#if all of the descendents are in clade S put TRUE (otherwise FALSE)
		df$S_Mono[x]<-all(s.des %in% stip)
		#get state for that node, but need to subtract number of tips
		df$S_State[x]<-pp1.temp$State[snode-length(pp1$phy$tip.label)]
		#get age for that node
		df$S_Age[x]<-pp1.temp$Age[snode-length(pp1$phy$tip.label)]
		#get ALL descendents and subtract number of tips
		sdes.nodes<-Descendants(pp1$phy,node=snode,type="all")-length(pp1$phy$tip.label)
		#drop tips
		sdes.nodes<-sdes.nodes[sdes.nodes>0]
		#subset pp1.temp to only include these nodes
		s.temp<-pp1.temp[sdes.nodes,]
		#if mrca is in state 0, then just use that age
		if(df$S_State[x]==0){
			df$S_Oldest_0[x]<-df$S_Age[x]
		}
		#if mrca is NOT in state 0, then find oldest age of State 0 within clade S
		if(df$S_State[x]!=0){
			#if any nodes in clade have state 0 (leave as NA if state 0 not present in nodes)
			if(any(s.temp$State==0)){
				df$S_Oldest_0[x]<-max(s.temp[s.temp$State==0,"Age"])
			}
		}
		if(df$S_State[x]==1){
			df$S_Oldest_1[x]<-df$S_Age[x]
		}
		#if mrca is NOT in state 1, then find oldest age of State 1 within clade S
		if(df$S_State[x]!=1){
			#if any nodes in clade have state 1 (leave as NA if state 1 not present in nodes)
			if(any(s.temp$State==1)){
				df$S_Oldest_1[x]<-max(s.temp[s.temp$State==1,"Age"])
			}
		}
		s.temp<-NULL
		
		#turn back to null...
		pp1.temp<-NULL
		pp1<-NULL
	}


biome.summary<-df


#Root state
require("HDInterval")
table(biome.summary$Root_State[1:1000])
round(min(biome.summary$Oldest_0[1:1000]),2)
round(mean(biome.summary$Oldest_0[1:1000]),2)
round(max(biome.summary$Oldest_0[1:1000]),2)
round(sd(biome.summary$Oldest_0[1:1000]),2)
hdi(biome.summary$Oldest_0[1:1000],credMas=0.95)


round(min(biome.summary$Oldest_1[1:1000]),2)
round(mean(biome.summary$Oldest_1[1:1000]),2)
round(max(biome.summary$Oldest_1[1:1000]),2)
round(sd(biome.summary$Oldest_1[1:1000]),2)
hdi(biome.summary$Oldest_1[1:1000],credMas=0.95)



#only retain estimates when clade monophyletic
table(biome.summary$A_State[1:1000][biome.summary$A_Mono[1:1000]==TRUE])
table(biome.summary$C_State[1:1000][biome.summary$C_Mono[1:1000]==TRUE])
table(biome.summary$I_State[1:1000][biome.summary$I_Mono[1:1000]==TRUE])
table(biome.summary$S_State[1:1000][biome.summary$S_Mono[1:1000]==TRUE])


#how many are monophyletic and have state 0 (get rid of na's for if state doesn't exist in clade)
sum(!is.na(biome.summary$A_Oldest_0[1:1000][biome.summary$A_Mono==TRUE]))
round(mean(biome.summary$A_Oldest_0[1:1000][biome.summary$A_Mono==TRUE],na.rm=TRUE),2)
hdi(biome.summary$A_Oldest_0[1:1000][biome.summary$A_Mono==TRUE],na.rm=TRUE,credMas=0.95)

sum(!is.na(biome.summary$C_Oldest_0[1:1000][biome.summary$C_Mono==TRUE]))
round(mean(biome.summary$C_Oldest_0[1:1000][biome.summary$C_Mono==TRUE],na.rm=TRUE),2)
hdi(biome.summary$C_Oldest_0[1:1000][biome.summary$C_Mono==TRUE],na.rm=TRUE,credMas=0.95)

sum(!is.na(biome.summary$I_Oldest_0[1:1000][biome.summary$I_Mono==TRUE]))
round(mean(biome.summary$I_Oldest_0[1:1000][biome.summary$I_Mono==TRUE],na.rm=TRUE),2)
hdi(biome.summary$I_Oldest_0[1:1000][biome.summary$I_Mono==TRUE],na.rm=TRUE,credMas=0.95)

sum(!is.na(biome.summary$S_Oldest_0[1:1000][biome.summary$S_Mono==TRUE]))
round(mean(biome.summary$S_Oldest_0[1:1000][biome.summary$S_Mono==TRUE],na.rm=TRUE),2)
hdi(biome.summary$S_Oldest_0[1:1000][biome.summary$S_Mono==TRUE],na.rm=TRUE,credMas=0.95)




#how many are monophyletic and have state 1 (get rid of na's for if state doesn't exist in clade)
sum(!is.na(biome.summary$A_Oldest_1[1:1000][biome.summary$A_Mono==TRUE]))
round(mean(biome.summary$A_Oldest_1[1:1000][biome.summary$A_Mono==TRUE],na.rm=TRUE),2)
hdi(biome.summary$A_Oldest_1[1:1000][biome.summary$A_Mono==TRUE],na.rm=TRUE,credMas=0.95)

sum(!is.na(biome.summary$C_Oldest_1[1:1000][biome.summary$C_Mono==TRUE]))
round(mean(biome.summary$C_Oldest_1[1:1000][biome.summary$C_Mono==TRUE],na.rm=TRUE),2)
hdi(biome.summary$C_Oldest_1[1:1000][biome.summary$C_Mono==TRUE],na.rm=TRUE,credMas=0.95)

sum(!is.na(biome.summary$I_Oldest_1[1:1000][biome.summary$I_Mono==TRUE]))
round(mean(biome.summary$I_Oldest_1[1:1000][biome.summary$I_Mono==TRUE],na.rm=TRUE),2)
hdi(biome.summary$I_Oldest_1[1:1000][biome.summary$I_Mono==TRUE],na.rm=TRUE,credMas=0.95)

sum(!is.na(biome.summary$S_Oldest_1[1:1000][biome.summary$S_Mono==TRUE]))
round(mean(biome.summary$S_Oldest_1[1:1000][biome.summary$S_Mono==TRUE],na.rm=TRUE),2)
hdi(biome.summary$S_Oldest_1[1:1000][biome.summary$S_Mono==TRUE],na.rm=TRUE,credMas=0.95)


write.csv(biome.summary,paste(path,"pp2s_vegetative_regime_summary_openvsmixedclosed_including_subclades.csv",sep=""),row.names=FALSE)




######################
######################

#PLOT PHYLOGENY W 95%HPD
require(phyloch)
#duplicate OTU's to drop
drops<-c("uncultured_Trebouxia_photobiont_HQ026172","Trebouxia_arboricola_AJ007385","Trebouxia_sp_OTU_A07_KR913092","Trebouxia_sp_OTU_A01_KR912550")

#out groups
outs<-c("Asterochloris_excentrica_AM905993","Asterochloris_gaertneri_AM905997","Asterochloris_phycobiontica_AM900490","Vulcanochloris_canariensis_KR952329","Vulcanochloris_guanchorum_KR952330","Vulcanochloris_symbiotica_KR952322")

#combine to drop
outs<-c(outs,drops)

repseqs<-read.csv(file="/xxxx/MASTER_accession_list_INFOADDED.csv",stringsAsFactors=FALSE)
repseqs<-repseqs[repseqs$RepSeq_97.5_Raw_28_Oct_2019 %in% "Yes",]
repseqs<-repseqs[,c("Organism_Acc_No","OTU_97.5_Raw_28_Oct_2019")]

#clean up MCC tree
tr<-read.beast(file="/xxxx/burnin25pcn_resampevery20k_mcc_medhts.tre")
tr<-drop.tip2(tr,outs)
#rename tips
for(x in 1:length(tr$tip.label)){
	tr$tip.label[tr$tip.label %in% tr$tip.label[x]]<-repseqs$OTU_97.5_Raw_28_Oct_2019[repseqs$Organism_Acc_No %in% tr$tip.label[x]]
}

#change G to C
tr$tip.label<-gsub("G_","C_",tr$tip.label)

png("/xxxx/trebouxia_975_phylo_w_hpd_pp.png",width=8,height=10,units="in",res=2400)
plot(ladderize(tr,FALSE),cex=0.4,edge.width=0.25,x.lim=c(-150, 300))
HPDbars(ladderize(tr,FALSE),col="darkolivegreen3",lwd=5)
node.support(tr$posterior, cutoff = 0.95, mode = "dots",col="black")
axisPhylo(cex.axis=0.6)
dev.off()


#PLOT ML PHYLOGENY W Bootstrap
require(phyloch)
#duplicate OTU's to drop
drops<-c("uncultured_Trebouxia_photobiont_HQ026172","Trebouxia_arboricola_AJ007385","Trebouxia_sp_OTU_A07_KR913092","Trebouxia_sp_OTU_A01_KR912550")

#out groups
outs<-c("Asterochloris_excentrica_AM905993","Asterochloris_gaertneri_AM905997","Asterochloris_phycobiontica_AM900490","Vulcanochloris_canariensis_KR952329","Vulcanochloris_guanchorum_KR952330","Vulcanochloris_symbiotica_KR952322")

#combine to drop
outs<-c(outs,drops)

repseqs<-read.csv(file="/xxxx/MASTER_accession_list_INFOADDED.csv",stringsAsFactors=FALSE)
repseqs<-repseqs[repseqs$RepSeq_97.5_Raw_28_Oct_2019 %in% "Yes",]
repseqs<-repseqs[,c("Organism_Acc_No","OTU_97.5_Raw_28_Oct_2019")]

#clean up ML tree
tr<-read.tree("/xxxx/RAxML_bipartitions.result")
tr<-drop.tip2(tr,outs)
#rename tips
for(x in 1:length(tr$tip.label)){
	tr$tip.label[tr$tip.label %in% tr$tip.label[x]]<-repseqs$OTU_97.5_Raw_28_Oct_2019[repseqs$Organism_Acc_No %in% tr$tip.label[x]]
}

#change G to C
tr$tip.label<-gsub("G_","C_",tr$tip.label)

png("/xxxx/trebouxia_975_phylo_w_ml_w_bp.png",width=8,height=10,units="in",res=2400)
plot(ladderize(tr,FALSE),cex=0.4,edge.width=0.5)
node.support(tr$node.label,cutoff=70,mode="dots",col="black")
dev.off()






#KEEP Outgroups
#PLOT PHYLOGENY W 95%HPD
require(phyloch)
#duplicate OTU's to drop
drops<-c("uncultured_Trebouxia_photobiont_HQ026172","Trebouxia_arboricola_AJ007385","Trebouxia_sp_OTU_A07_KR913092","Trebouxia_sp_OTU_A01_KR912550")

#out groups
outs<-c("Asterochloris_excentrica_AM905993","Asterochloris_gaertneri_AM905997","Asterochloris_phycobiontica_AM900490","Vulcanochloris_canariensis_KR952329","Vulcanochloris_guanchorum_KR952330","Vulcanochloris_symbiotica_KR952322")

repseqs<-read.csv(file="/xxxx/MASTER_accession_list_INFOADDED.csv",stringsAsFactors=FALSE)
repseqs<-repseqs[repseqs$RepSeq_97.5_Raw_28_Oct_2019 %in% "Yes",]
repseqs<-repseqs[,c("Organism_Acc_No","OTU_97.5_Raw_28_Oct_2019")]

#clean up MCC tree
tr<-read.beast(file="/xxxx/burnin25pcn_resampevery20k_mcc_medhts.tre")
tr<-drop.tip2(tr,drops)
#rename tips
for(x in 1:length(tr$tip.label)){
	if(isTRUE(grep("Trebouxia",tr$tip.label[x])==1)){
		tr$tip.label[tr$tip.label %in% tr$tip.label[x]]<-repseqs$OTU_97.5_Raw_28_Oct_2019[repseqs$Organism_Acc_No %in% tr$tip.label[x]]
	}
}

#change G to C
tr$tip.label<-gsub("G_","C_",tr$tip.label)

#make it so root does not have a support value
tr$posterior[1]<-0

png("/xxxx/trebouxia_975_phylo_w_hpd_pp_wouts.png",width=8,height=10,units="in",res=2400)
plot(ladderize(tr,FALSE),cex=0.4,edge.width=0.25,x.lim=c(-150, 300))
HPDbars(ladderize(tr,FALSE),col="darkolivegreen3",lwd=5)
node.support(tr$posterior, cutoff = 0.99, mode = "dots",col="black")
axisPhylo(cex.axis=0.6)
dev.off()


#PLOT ML PHYLOGENY W Bootstrap
require(phyloch)
#duplicate OTU's to drop
drops<-c("uncultured_Trebouxia_photobiont_HQ026172","Trebouxia_arboricola_AJ007385","Trebouxia_sp_OTU_A07_KR913092","Trebouxia_sp_OTU_A01_KR912550")

#out groups
outs<-c("Asterochloris_excentrica_AM905993","Asterochloris_gaertneri_AM905997","Asterochloris_phycobiontica_AM900490","Vulcanochloris_canariensis_KR952329","Vulcanochloris_guanchorum_KR952330","Vulcanochloris_symbiotica_KR952322")

repseqs<-read.csv(file="/xxxx/MASTER_accession_list_INFOADDED.csv",stringsAsFactors=FALSE)
repseqs<-repseqs[repseqs$RepSeq_97.5_Raw_28_Oct_2019 %in% "Yes",]
repseqs<-repseqs[,c("Organism_Acc_No","OTU_97.5_Raw_28_Oct_2019")]

#clean up ML tree
tr<-read.tree("/xxxx/RAxML_bipartitions.result")
tr<-drop.tip2(tr,drops)
#rename tips
for(x in 1:length(tr$tip.label)){
	if(isTRUE(grep("Trebouxia",tr$tip.label[x])==1)){
		tr$tip.label[tr$tip.label %in% tr$tip.label[x]]<-repseqs$OTU_97.5_Raw_28_Oct_2019[repseqs$Organism_Acc_No %in% tr$tip.label[x]]
	}
}

#change G to C
tr$tip.label<-gsub("G_","C_",tr$tip.label)

#make it so root does not have a support value
tr$node.label[1]<-0

png("/xxxx/trebouxia_975_phylo_w_ml_w_bp_wouts.png",width=8,height=10,units="in",res=2400)
plot(ladderize(tr,FALSE),cex=0.4,edge.width=0.5)
node.support(tr$node.label,cutoff=70,mode="dots",col="black")
add.scale.bar(x=1.2,y=-4,cex=0.5,lwd=0.4)
dev.off()








######################
######################
######################
ggbiplot.mod<-function (pcobj, choices = 1:2, scale = 1, pc.biplot = TRUE, 
    obs.scale = 1 - scale, var.scale = scale, groups = NULL, 
    ellipse = FALSE, ellipse.prob = 0.68, labels = NULL, labels.size = 3, 
    alpha = 1, var.axes = TRUE, circle = FALSE, var.col="darkred",circle.prob = 0.69, 
    varname.size = 3, varname.adjust = 1.5, varname.abbrev = FALSE, 
    ...) 
{
    library(ggplot2)
    library(plyr)
    library(scales)
    library(grid)
    stopifnot(length(choices) == 2)
    if (inherits(pcobj, "prcomp")) {
        nobs.factor <- sqrt(nrow(pcobj$x) - 1)
        d <- pcobj$sdev
        u <- sweep(pcobj$x, 2, 1/(d * nobs.factor), FUN = "*")
        v <- pcobj$rotation
    }
    else if (inherits(pcobj, "princomp")) {
        nobs.factor <- sqrt(pcobj$n.obs)
        d <- pcobj$sdev
        u <- sweep(pcobj$scores, 2, 1/(d * nobs.factor), FUN = "*")
        v <- pcobj$loadings
    }
    else if (inherits(pcobj, "PCA")) {
        nobs.factor <- sqrt(nrow(pcobj$call$X))
        d <- unlist(sqrt(pcobj$eig)[1])
        u <- sweep(pcobj$ind$coord, 2, 1/(d * nobs.factor), FUN = "*")
        v <- sweep(pcobj$var$coord, 2, sqrt(pcobj$eig[1:ncol(pcobj$var$coord), 
            1]), FUN = "/")
    }
    else if (inherits(pcobj, "lda")) {
        nobs.factor <- sqrt(pcobj$N)
        d <- pcobj$svd
        u <- predict(pcobj)$x/nobs.factor
        v <- pcobj$scaling
        d.total <- sum(d^2)
    }
    else {
        stop("Expected a object of class prcomp, princomp, PCA, or lda")
    }
    choices <- pmin(choices, ncol(u))
    df.u <- as.data.frame(sweep(u[, choices], 2, d[choices]^obs.scale, 
        FUN = "*"))
    v <- sweep(v, 2, d^var.scale, FUN = "*")
    df.v <- as.data.frame(v[, choices])
    names(df.u) <- c("xvar", "yvar")
    names(df.v) <- names(df.u)
    if (pc.biplot) {
        df.u <- df.u * nobs.factor
    }
    r <- sqrt(qchisq(circle.prob, df = 2)) * prod(colMeans(df.u^2))^(1/4)
    v.scale <- rowSums(v^2)
    df.v <- r * df.v/sqrt(max(v.scale))
    if (obs.scale == 0) {
        u.axis.labs <- paste("standardized PC", choices, sep = "")
    }
    else {
        u.axis.labs <- paste("PC", choices, sep = "")
    }
    u.axis.labs <- paste(u.axis.labs, sprintf("(%0.1f%% explained var.)", 
        100 * pcobj$sdev[choices]^2/sum(pcobj$sdev^2)))
    if (!is.null(labels)) {
        df.u$labels <- labels
    }
    if (!is.null(groups)) {
        df.u$groups <- groups
    }
    if (varname.abbrev) {
        df.v$varname <- abbreviate(rownames(v))
    }
    else {
        df.v$varname <- rownames(v)
    }
    df.v$angle <- with(df.v, (180/pi) * atan(yvar/xvar))
    df.v$hjust = with(df.v, (1 - varname.adjust * sign(xvar))/2)
    g <- ggplot(data = df.u, aes(x = xvar, y = yvar)) + xlab(u.axis.labs[1]) + 
        ylab(u.axis.labs[2]) + coord_equal()
    if (var.axes) {
        if (circle) {
            theta <- c(seq(-pi, pi, length = 50), seq(pi, -pi, 
                length = 50))
            circle <- data.frame(xvar = r * cos(theta), yvar = r * 
                sin(theta))
            g <- g + geom_path(data = circle, color = muted("white"), 
                size = 1/2, alpha = 1/3)
        }
        g <- g + geom_segment(data = df.v, aes(x = 0, y = 0, 
            xend = xvar, yend = yvar), arrow = arrow(length = unit(1/2, 
            "picas")), color = var.col)
    }
    if (!is.null(df.u$labels)) {
        if (!is.null(df.u$groups)) {
            g <- g + geom_text(aes(label = labels, color = groups), 
                size = labels.size)
        }
        else {
            g <- g + geom_text(aes(label = labels), size = labels.size)
        }
    }
    else {
        if (!is.null(df.u$groups)) {
            g <- g + geom_point(aes(color = groups), alpha = alpha)
        }
        else {
            g <- g + geom_point(alpha = alpha)
        }
    }
    if (!is.null(df.u$groups) && ellipse) {
        theta <- c(seq(-pi, pi, length = 50), seq(pi, -pi, length = 50))
        circle <- cbind(cos(theta), sin(theta))
        ell <- ddply(df.u, "groups", function(x) {
            if (nrow(x) <= 2) {
                return(NULL)
            }
            sigma <- var(cbind(x$xvar, x$yvar))
            mu <- c(mean(x$xvar), mean(x$yvar))
            ed <- sqrt(qchisq(ellipse.prob, df = 2))
            data.frame(sweep(circle %*% chol(sigma) * ed, 2, 
                mu, FUN = "+"), groups = x$groups[1])
        })
        names(ell)[1:2] <- c("xvar", "yvar")
        g <- g + geom_path(data = ell, aes(color = groups, group = groups))
    }
    if (var.axes) {
        g <- g + geom_text(data = df.v, aes(label = varname, 
            x = xvar, y = yvar, angle = angle, hjust = hjust), 
            color = var.col, size = varname.size)
    }
    return(g)
}


