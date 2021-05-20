#Need to make a correction - see below  - made correct correction, but then reverted it.  it's actually found in a couple places (Svalbard & Antarctica, but go with Antarctica associated w Usnea aurantiacoatra)

mcs.mod<-read.csv(file="/xxxx/MASTER_accession_list_INFOADDED_MODIFIED.csv",stringsAsFactors=FALSE)


#JX518612 this is listed as both from Umbilicaria lyngei in Svalbard AND as from Usnea aurantiacoatra from Antarctica in Li et al. (2013) - NCBI has as from Usnea, so go with that.  Need to update the geography on it

mcs.mod$OTU_97.5_Raw_28_Oct_2019_Modified[mcs.mod$Acc_No %in% "JX518612"]
#S_97.5_1

table(mcs.mod$Country_Detailed[mcs.mod$OTU_97.5_Raw_28_Oct_2019_Modified %in% "S_97.5_1"])
table(mcs.mod$Country[mcs.mod$OTU_97.5_Raw_28_Oct_2019_Modified %in% "S_97.5_1"])
table(mcs.mod$ISO3Code[mcs.mod$OTU_97.5_Raw_28_Oct_2019_Modified %in% "S_97.5_1"])
table(mcs.mod$GEO3SubRegion[mcs.mod$OTU_97.5_Raw_28_Oct_2019_Modified %in% "S_97.5_1"])
table(mcs.mod$GEO3MajorRegion[mcs.mod$OTU_97.5_Raw_28_Oct_2019_Modified %in% "S_97.5_1"])
table(mcs.mod$Continent[mcs.mod$OTU_97.5_Raw_28_Oct_2019_Modified %in% "S_97.5_1"])

#it's already reported from Antarctica and Norway, so just need to adjust number of accessions from a Norway/Antarctica, and does not affect number of OTUs.  No Lat Long given, so OK.

#specific Acc No(s)
#Country_Detailed listed as Svalbard
mcs.mod$Country_Detailed[mcs.mod$Acc_No %in% "JX518612"]<-"Antarctica"

#Country listed as Norway
mcs.mod$Country[mcs.mod$Acc_No %in% "JX518612"]<-"Antarctica"

#ISO3Code listed as NOR
mcs.mod$ISO3Code[mcs.mod$Acc_No %in% "JX518612"]<-"ATA"

#GEO3SubRegion listed as "Western Europe"
mcs.mod$GEO3SubRegion[mcs.mod$Acc_No %in% "JX518612"]<-"Antarctic"

#GEO3MajorRegion listed as Europe
mcs.mod$GEO3MajorRegion[mcs.mod$Acc_No %in% "JX518612"]<-"Polar"

#Continent was listed as Europe"
mcs.mod$Continent[mcs.mod$Acc_No %in% "JX518612"]<-"Antarctica"

#Continent was listed as Europe"
mcs.mod$Geo_Man_Cur[mcs.mod$Acc_No %in% "JX518612"]<-"Yes"


table(mcs.mod$Country_Detailed[mcs.mod$OTU_97.5_Raw_28_Oct_2019_Modified %in% "S_97.5_1"])
table(mcs.mod$Country[mcs.mod$OTU_97.5_Raw_28_Oct_2019_Modified %in% "S_97.5_1"])
table(mcs.mod$ISO3Code[mcs.mod$OTU_97.5_Raw_28_Oct_2019_Modified %in% "S_97.5_1"])
table(mcs.mod$GEO3SubRegion[mcs.mod$OTU_97.5_Raw_28_Oct_2019_Modified %in% "S_97.5_1"])
table(mcs.mod$GEO3MajorRegion[mcs.mod$OTU_97.5_Raw_28_Oct_2019_Modified %in% "S_97.5_1"])
table(mcs.mod$Continent[mcs.mod$OTU_97.5_Raw_28_Oct_2019_Modified %in% "S_97.5_1"])

write.csv(mcs.mod,file="/xxxx/MASTER_accession_list_INFOADDED_MODIFIED_12april2020.csv",row.names=FALSE)









#Re-plot to remove one sequence from Svalbard and add one to Antarctica

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

me<-read.csv(file="/xxxx/MASTER_accession_list_INFOADDED_MODIFIED_12april2020.csv",stringsAsFactors=FALSE)
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
pdf("/xxxx/trebouxia_seqs_per_country_29oct2019_highres_MODIFIED_12april2020.pdf",width=11,height=11)
gg
dev.off()
#which countries have most
nrow(me)
sort(table(me$Country))
sum(table(me$Country))



