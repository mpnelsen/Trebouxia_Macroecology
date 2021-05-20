
tr<-read.nexus(file="/xxxx/burnin25pcn_resampevery20k_mcc_medhts_OTUs_renamed_bioclim_retained_MODIFIED.tre")
taxa<-read.csv(file="/xxxx/OTU_97.5_BIOCLIM_SUMMARY_MODIFIED_UNIQUES_wSE_corrected.csv",stringsAsFactors=FALSE,row.names=1)
tz<-taxa[,c("Mean_Bio1","Mean_Bio12")]
tz<-as.matrix(t(tz))
tz<-tz[,match(tr$tip.label, colnames(tz))]
rownames(tz)<-c("Trait1","Trait2")

write.tree(tr,file="/xxxx/Nelsen.tre")
write.csv(tz,file="/xxxx/Nelsen.csv")



#############################################################################
#############################################################################
#############################################################################
#############################################################################
#summarize.lasso.convergence.output FUNCTION FROM SLATER & FRISCIA (2019)
summarize.lasso.convergence.output <- function(model) {
  bt <- branching.times(model$tree)
  shift.times.max <- bt[match(model$tree$edge[model$shift.configuration, 1], names(bt))]
  shift.crown <- model$tree$edge[model$shift.configuration, 2]
  
  shift.times.min <- bt[match( shift.crown, names(bt))]
  
  shift.taxa <- list()
  
  for(i in 1:length(shift.crown)) shift.taxa[[i]]<- (tips(model$tree, shift.crown[i]))
  
  
  names(shift.taxa) <- names(model$shift.configuration)
  ordered.list <- list()
  list.order <- order(as.numeric(unlist(lapply(shift.taxa, length))))
  for(i in 1:length(list.order)) {
    ordered.list[[i]] <- shift.taxa[[list.order[i]]]
  }
  
  names(ordered.list) <- names(shift.taxa)[list.order]
  
  for(i in 1:(length(ordered.list)-1)) {
    for(j in (i+1): (length(ordered.list))) {
      if(!is.na(match(ordered.list[[i]], ordered.list[[j]]))) {
        ordered.list[[j]] <- ordered.list[[j]][-match(ordered.list[[i]], ordered.list[[j]])]
      }
    }
  }
  convergent.regimes <- list()
  new.regimes<-unique(names(ordered.list))
  for(i in 1:length(new.regimes)){
    xx <- which(names(ordered.list)==new.regimes[[i]])
    tmp <- character()
    for(j in 1:length(xx)) {
      tmp <-  append(tmp, ordered.list[[xx[j]]])
    }
    convergent.regimes[[i]] <-tmp
    rm(tmp)
  }
  
  convergent.regimes[[length(new.regimes)+1]] <- setdiff(model$tree$tip.label, unlist(convergent.regimes)) 
  names(convergent.regimes) <- c(new.regimes, "basal")
  convergent.regimes  
  as.character(unlist(convergent.regimes))
  reg <- names(convergent.regimes)
  yy <- character()
  for(i in 1:length(reg)) {
    yy<-append(yy, rep(reg[i], length(convergent.regimes[[i]])))
  }
  return(cbind(as.character(unlist(convergent.regimes)), yy))
}

#from l1ou github page, but modified asterisk to indicate regime number (basal=1)
plot.l1ou.mod <- function (model, palette = NA, 
                       edge.shift.ann=TRUE,  edge.shift.adj=c(0.5,-.025),
                       edge.label=c(), asterisk=FALSE, Number = TRUE,
                       edge.label.ann=FALSE, edge.label.adj=c(0.5,    1), 
                       edge.label.pos=NA,
                       edge.ann.cex = 1, 
                       plot.bar = TRUE, bar.axis = TRUE, ...) 
{
    tree = model$tree
    s.c = model$shift.configuration
    stopifnot(identical(tree$edge, reorder(tree, "postorder")$edge))
    nShifts = model$nShifts
    nEdges = Nedge(tree)
    if (bar.axis) 
        par(oma = c(3, 0, 0, 3))
    Y = as.matrix(model$Y)
    stopifnot(identical(rownames(Y), tree$tip.label))

    if (bar.axis) 
        par(oma = c(3, 0, 0, 3))

    if (plot.bar) {
        #changed width of barplots from 1 to 0.5
        layout(matrix(c(1+ncol(Y),1:ncol(Y)), nrow=1), 
               widths = c(2,rep(0.5, ncol(Y)))
               )
    }

    #NOTE: assiging colors the shifts
    if (all(is.na(palette))) {
        palette = c(sample(rainbow(nShifts)), "gray")
        if( !is.null(names(s.c)) ){
            ids = unique(names(s.c))
            tmp = sample(rainbow(length(ids)))
	    for( id in ids ){
		    if(id == "0"){
			    palette[which(names(s.c) == id)] = "gray"  #background 
			    next
		    }
		    palette[which(names(s.c)==id)] = tmp[which(ids==id)]
	    }
        }
    }

    stopifnot(length(palette) == model$nShifts + 1)

    edgecol = rep(palette[nShifts + 1], nEdges)
    counter = 1
    Z = model$l1ou.options$Z
    if(length(s.c) > 0)
        for (shift in sort(s.c, decreasing = T)) {
            edgecol[[shift]] = palette[[which(s.c == shift)]]
            tips = which(Z[, shift] > 0)
            for (tip in tips) {
                edgecol[which(Z[tip, 1:shift] > 0)] = palette[[which(s.c == 
                    shift)]]
            }
            counter = counter + 1
        }



    ##A dummy plot just to get the plotting order
    plot.phylo(tree, plot=FALSE)
    lastPP = get("last_plot.phylo", envir = .PlotPhyloEnv)
    o = order(lastPP$yy[1:length(tree$tip.label)])
    par.new.default <- par()$new ##just to be careful with the global variable
    par(new=TRUE)

    #NOTE: plotting bar plot .....
    if (plot.bar) {
        nTips = length(tree$tip.label)
        barcol = rep("gray", nTips)
        for (i in 1:nTips) {
            barcol[[i]] = edgecol[which(tree$edge[, 2] == i)]
        }
        if (bar.axis) 
        	#changed from par(mar = c(0, 0, 0, 3))
            par(mar = c(0, 0, 0, 2))
        for (i in 1:ncol(Y)) {
            normy = (Y[, i] - mean(Y[, i], na.rm=TRUE))/sd(Y[, i], na.rm=TRUE)
            barplot(as.vector(normy[o]), border = FALSE, col = barcol[o], 
                    horiz = TRUE, names.arg = "", xaxt = "n")
            if (bar.axis){
                axis(1, at = range(normy, na.rm=TRUE), 
                     labels = round(range(normy, na.rm=TRUE), 
                                    digits = 2))
            }
            if (!is.null(colnames(Y)) && length(colnames(Y)) > 
                (i - 1)) 
                #changed cex argument from cex = 1
                mtext(colnames(Y)[[i]],cex = 0.75, line = +1, side = 1)
        }
    }

    #NOTE: plotting the tree etc etc
    plot.phylo(tree, edge.color = edgecol, no.margin = TRUE, 
        ...)

    if (length(s.c) > 0) {
        if (asterisk) {
            Z = l1ou:::generate_design_matrix(tree, type = "apprX")
            for (idx in 1:length(s.c)) {
                sP = s.c[[idx]]
                pos = max(Z[, sP])
                edge.labels = rep(NA, length(tree$edge[, 1]))
                edge.labels[sP] = "*"
                edgelabels(edge.labels, cex = 3 * edge.ann.cex, 
                  adj = c(0.5, 0.8), frame = "none", date = pos)
            }
        }	
        #if add regime number (basal=1)
        if (Number) {
            Z = l1ou:::generate_design_matrix(tree, type = "apprX")
            for (idx in 1:length(s.c)) {
                sP = s.c[[idx]]
                pos = max(Z[, sP])
                edge.labels = rep(NA, length(tree$edge[, 1]))
                edge.labels[sP] = 1+as.numeric(names(s.c)[idx])
                edgelabels(edge.labels, cex = 1 * edge.ann.cex, 
                  adj = c(1.2, 0.5), frame = "none", date = pos)
            }
        }
    }
    if (edge.shift.ann) {
        eLabels = rep(NA, nEdges)
        for (shift in s.c) {
            eLabels[shift] = paste(round(model$shift.values[which(s.c == 
                shift), ], digits = 2), collapse = ",")
        }
        edgelabels(eLabels, cex = edge.ann.cex, adj = edge.shift.adj, 
            frame = "none")
    }
    if (edge.label.ann) {
        if (length(tree$edge.label) == 0) {
            if (length(edge.label) == 0) {
                stop("no edge labels are provided via tree$edge.label or edge.label!")
            }
            tree$edge.label = edge.label
        }
        Z = l1ou:::generate_design_matrix(tree, type = "apprX")
        if (!is.na(edge.label.pos)) 
            if (edge.label.pos < 0 || edge.label.pos > 1) 
                stop("edge.label.pos should be between 0 and 1")
        for (idx in 1:length(tree$edge.label)) {
            if (is.na(tree$edge.label[[idx]])) 
                next
            pos = max(Z[, idx])
            if (!is.na(edge.label.pos)) {
                pos = pos - edge.label.pos * tree$edge.length[[idx]]
            }
            edge.labels = rep(NA, length(tree$edge[, 1]))
            edge.labels[[idx]] = tree$edge.label[[idx]]
            edgelabels(edge.labels, cex = edge.ann.cex, adj = edge.label.adj, 
                frame = "none", date = pos)
        }
    }
    par(new=par.new.default)
}




#####MCC Tree
#make empty data frame
#save ebalpha,likelihood, AIC, AICc,  AICcweight
mods<-c("ER","SYM","ARD","EREBD","SYMEBD","ARDEBD","EREBI","SYMEBI","ARDEBI")
#mods<-c("ER","SYM","EREBD","SYMEBD","EREBI","SYMEBI")
vals<-c("Alpha","Likelihood","AIC","AICc","AICcWeight")
mod.vals<-apply(expand.grid(mods, vals), 1, paste, collapse="_")
mod.vals<-c("Tree","L1OU_Orig_Shifts","L1OU_Orig_pBIC","L1OU_Conv_Shifts","L1OU_Conv_Regimes","L1OU_Conv_pBIC",mod.vals,"Best_Model")
df<-as.data.frame(matrix(nrow=101,ncol=length(mod.vals)))
colnames(df)<-mod.vals
df$Tree[1:100]<-1:100
df$Tree[101]<-"MCC"


#read data and subset
require(geiger)
require(l1ou)
taxa<-read.csv(file="/xxxx/OTU_97.5_BIOCLIM_SUMMARY_MODIFIED_UNIQUES_wSE_corrected.csv",stringsAsFactors=FALSE,row.names=1)
dat<-taxa[,c("Mean_Bio1","Mean_Bio12")]
colnames(dat)<-c("Bio1","Bio12")

#read trees
tr<-read.tree(file="/xxxx/Nelsen.tre")
tr<-ladderize(tr,FALSE)
#WRITE IT AND READ IN AGAIN
write.tree(tr,file="/xxxx/Nelsen_ladderized_2state.tre")
tr<-read.tree(file="/xxxx/Nelsen_ladderized_2state.tre")

#phylogenetic lasso to identify regimes
rlga.lasso.meansandextremes <- adjust_data(tr, dat, normalize = F)
RLGAModel.meansandextremes <- estimate_shift_configuration(rlga.lasso.meansandextremes$tree, rlga.lasso.meansandextremes$Y, rescale=T, root.model = "OUfixedRoot", criterion="pBIC", nCores=15, max.nShifts=42)
#RLGAModel.meansandextremes

#number of shifts
df[101,"L1OU_Orig_Shifts"]<-RLGAModel.meansandextremes$nShifts
#pBIC score
df[101,"L1OU_Orig_pBIC"]<-RLGAModel.meansandextremes$score

#identify convergent regimes
conv.reg.rlga1.meansandextremes <- estimate_convergent_regimes(RLGAModel.meansandextremes, method="backward", criterion = "pBIC", fixed.alpha = FALSE,nCores=15)
#conv.reg.rlga1.meansandextremes

#number of shifts	
df[101,"L1OU_Conv_Shifts"]<-conv.reg.rlga1.meansandextremes$nShifts
#number of unique regimes...NOTE...this DOES NOT INCLUDE THE "basal" regime - this is added.
df[101,"L1OU_Conv_Regimes"]<-length(unique(names(conv.reg.rlga1.meansandextremes$shift.configuration)))+1
#pBIC score
df[101,"L1OU_Conv_pBIC"]<-conv.reg.rlga1.meansandextremes$score

#summarize.lasso.convergence.output function from Slater & Friscia (2019)
convergent.rlga.regimes <-summarize.lasso.convergence.output(conv.reg.rlga1.meansandextremes)
dis.dat<-convergent.rlga.regimes[,2]
names(dis.dat)<-convergent.rlga.regimes[,1]
er.none<-fitDiscrete(tr,dis.dat,model="ER",transform="none",ncores=15,control=list(niter=500))
df[101,c("ER_Likelihood","ER_AIC","ER_AICc")]<-c(er.none$opt$lnL,er.none$opt$aic,er.none$opt$aicc)
sym.none<-fitDiscrete(tr,dis.dat,model="SYM",transform="none",ncores=15,control=list(niter=500))
df[101,c("SYM_Likelihood","SYM_AIC","SYM_AICc")]<-c(sym.none$opt$lnL,sym.none$opt$aic,sym.none$opt$aicc)
ard.none<-fitDiscrete(tr,dis.dat,model="ARD",transform="none",ncores=15,control=list(niter=500))
df[101,c("ARD_Likelihood","ARD_AIC","ARD_AICc")]<-c(ard.none$opt$lnL,ard.none$opt$aic,ard.none$opt$aicc)

er.eb.dec<-fitDiscrete(tr,dis.dat,model="ER",transform="EB",ncores=15,control=list(niter=500),bounds=list(a=c(min=-10,max=-0.000001)))
df[101,c("EREBD_Alpha","EREBD_Likelihood","EREBD_AIC","EREBD_AICc")]<-c(er.eb.dec$opt$a,er.eb.dec$opt$lnL,er.eb.dec$opt$aic,er.eb.dec$opt$aicc)
sym.eb.dec<-fitDiscrete(tr,dis.dat,model="SYM",transform="EB",ncores=15,control=list(niter=500),bounds=list(a=c(min=-10,max=-0.000001)))
df[101,c("SYMEBD_Alpha","SYMEBD_Likelihood","SYMEBD_AIC","SYMEBD_AICc")]<-c(sym.eb.dec$opt$a,sym.eb.dec$opt$lnL,sym.eb.dec$opt$aic,sym.eb.dec$opt$aicc)
ard.eb.dec<-fitDiscrete(tr,dis.dat,model="ARD",transform="EB",ncores=15,control=list(niter=500),bounds=list(a=c(min=-10,max=-0.000001)))
df[101,c("ARDEBD_Alpha","ARDEBD_Likelihood","ARDEBD_AIC","ARDEBD_AICc")]<-c(ard.eb.dec$opt$a,ard.eb.dec$opt$lnL,ard.eb.dec$opt$aic,ard.eb.dec$opt$aicc)

er.eb.inc<-fitDiscrete(tr,dis.dat,model="ER",transform="EB",ncores=15,control=list(niter=500),bounds=list(a=c(min=0.000001,max=10)))
df[101,c("EREBI_Alpha","EREBI_Likelihood","EREBI_AIC","EREBI_AICc")]<-c(er.eb.inc$opt$a,er.eb.inc$opt$lnL,er.eb.inc$opt$aic,er.eb.inc$opt$aicc)
sym.eb.inc<-fitDiscrete(tr,dis.dat,model="SYM",transform="EB",ncores=15,control=list(niter=500),bounds=list(a=c(min=0.000001,max=10)))
df[101,c("SYMEBI_Alpha","SYMEBI_Likelihood","SYMEBI_AIC","SYMEBI_AICc")]<-c(sym.eb.inc$opt$a,sym.eb.inc$opt$lnL,sym.eb.inc$opt$aic,sym.eb.inc$opt$aicc)
ard.eb.inc<-fitDiscrete(tr,dis.dat,model="ARD",transform="EB",ncores=15,control=list(niter=500),bounds=list(a=c(min=0.000001,max=10)))
df[101,c("ARDEBI_Alpha","ARDEBI_Likelihood","ARDEBI_AIC","ARDEBI_AICc")]<-c(ard.eb.inc$opt$a,ard.eb.inc$opt$lnL,ard.eb.inc$opt$aic,ard.eb.inc$opt$aicc)

AICc.scores<-c(er.none$opt$aicc,sym.none$opt$aicc,ard.none$opt$aicc,er.eb.dec$opt$aicc,sym.eb.dec$opt$aicc,ard.eb.dec$opt$aicc,er.eb.inc$opt$aicc,sym.eb.inc$opt$aicc,ard.eb.inc$opt$aicc)
names(AICc.scores)<-c("ER","SYM","ARD","EREBD","SYMEBD","ARDEBD","EREBI","SYMEBI","ARDEBI")
#names(AICc.scores)<-c("ER","SYM","EREBD","SYMEBD","EREBI","SYMEBI")
AICc.weights<-aicw(AICc.scores)
AICc.weights

df[101,c("ER_AICcWeight","SYM_AICcWeight","ARD_AICcWeight","EREBD_AICcWeight","SYMEBD_AICcWeight","ARDEBD_AICcWeight","EREBI_AICcWeight","SYMEBI_AICcWeight","ARDEBI_AICcWeight")]<-AICc.weights$w
df[101,"Best_Model"]<-rownames(AICc.weights)[AICc.weights$w %in% max(AICc.weights$w)]

#save everything now.
save(rlga.lasso.meansandextremes,RLGAModel.meansandextremes,conv.reg.rlga1.meansandextremes,convergent.rlga.regimes,dis.dat,er.none,sym.none,ard.none,er.eb.dec,sym.eb.dec,ard.eb.dec,er.eb.inc,sym.eb.inc,ard.eb.inc,AICc.weights,AICc.scores,file=paste("/xxxx/l1ou_eb_analyses_tree_","MCC_ladderized_2state",".RData",sep=""))

#save spreadsheet
write.csv(df,file="/xxxx/l1ou_eb_analyses_wladderizedMCC_2state.csv",row.names=FALSE)

df$L1OU_Conv_Regimes[101]
df$L1OU_Conv_Shifts[101]
conv.reg.rlga1.meansandextremes$shift.configuration
table(names(conv.reg.rlga1.meansandextremes$shift.configuration))




#re-plot MCC tree with different colors and add regime category
require(pals)
require(scales)
require(viridis)
require(l1ou)
require(scico)
require(phyloch)

#topology is identical between l1ou and corHMM
load(paste("/xxxx/l1ou_eb_analyses_tree_","MCC_ladderized_2state",".RData",sep=""))
#main.cols <- viridis_pal()(length(unique(names(conv.reg.rlga1.meansandextremes$shift.configuration))))
#main.cols <- rev(viridis_pal()(length(unique(names(conv.reg.rlga1.meansandextremes$shift.configuration)))))
#main.cols <- parula(length(unique(names(conv.reg.rlga1.meansandextremes$shift.configuration))))
#main.cols <- rev(parula(length(unique(names(conv.reg.rlga1.meansandextremes$shift.configuration)))))
#main.cols <- stepped2(length(unique(names(conv.reg.rlga1.meansandextremes$shift.configuration))))
#main.cols <- rev(stepped2(length(unique(names(conv.reg.rlga1.meansandextremes$shift.configuration)))))
#main.cols <- scico(length(unique(names(conv.reg.rlga1.meansandextremes$shift.configuration))),palette="batlow")
#main.cols <- rev(scico(length(unique(names(conv.reg.rlga1.meansandextremes$shift.configuration))),palette="batlow"))
#main.cols <- scico(length(unique(names(conv.reg.rlga1.meansandextremes$shift.configuration))),palette="hawaii")
#main.cols <- rev(scico(length(unique(names(conv.reg.rlga1.meansandextremes$shift.configuration))),palette="hawaii"))
#main.cols <- scico(length(unique(names(conv.reg.rlga1.meansandextremes$shift.configuration))),palette="tofino")
#main.cols <- scico(length(unique(names(conv.reg.rlga1.meansandextremes$shift.configuration))),palette="oleron")
#yellow was so faint in tokyo that i added another and dropped it, in hopes of dropping yellow
main.cols <- scico(length(unique(names(conv.reg.rlga1.meansandextremes$shift.configuration)))+1,palette="tokyo")
main.cols<-rev(main.cols[1:(length(main.cols)-1)])
#main.cols <- scico(length(unique(names(conv.reg.rlga1.meansandextremes$shift.configuration))),palette="tokyo")
#main.cols <- scico(length(unique(names(conv.reg.rlga1.meansandextremes$shift.configuration))),palette="acton")
#main.cols <- scico(length(unique(names(conv.reg.rlga1.meansandextremes$shift.configuration))),palette="bilbao")
#main.cols <- scico(length(unique(names(conv.reg.rlga1.meansandextremes$shift.configuration))),palette="bamako")
#main.cols <- scico(length(unique(names(conv.reg.rlga1.meansandextremes$shift.configuration))),palette="imola")
#main.cols <- scico(length(unique(names(conv.reg.rlga1.meansandextremes$shift.configuration))),palette="corkO")
#main.cols <- scico(length(unique(names(conv.reg.rlga1.meansandextremes$shift.configuration))),palette="romaO")
my.cols.auto <- rep("grey", (length(conv.reg.rlga1.meansandextremes$shift.configuration)+1))
for(x in 1:length(conv.reg.rlga1.meansandextremes$shift.configuration)){
	my.cols.auto[c(which(names(conv.reg.rlga1.meansandextremes$shift.configuration) %in% x))]<-main.cols[x]
}

#change names of clade G OTUs to clade C
attributes(conv.reg.rlga1.meansandextremes)
rownames(conv.reg.rlga1.meansandextremes$Y)<-gsub("G_","C_",rownames(conv.reg.rlga1.meansandextremes$Y))
conv.reg.rlga1.meansandextremes$tree$tip.label<-gsub("G_","C_",conv.reg.rlga1.meansandextremes$tree$tip.label)

#change Bio to BIO
colnames(conv.reg.rlga1.meansandextremes$Y)<-gsub("Bio","BIO",colnames(conv.reg.rlga1.meansandextremes$Y))

#pdf(file="l1ou_.meansandextremes_test_autocols_numbered.pdf", width=14, height=7)
#pdf(file="l1ou_.meansandextremes_test_autocols_numbered_viridisrev.pdf", width=14, height=7)
#pdf(file="l1ou_.meansandextremes_test_autocols_numbered_parula.pdf", width=14, height=7)
#pdf(file="l1ou_.meansandextremes_test_autocols_numbered_parularev.pdf", width=14, height=7)
#pdf(file="l1ou_.meansandextremes_test_autocols_numbered_stepped2.pdf", width=14, height=7)
#pdf(file="l1ou_.meansandextremes_test_autocols_numbered_stepped2rev.pdf", width=14, height=7)
#pdf(file="l1ou_.meansandextremes_test_autocols_numbered_batlow.pdf", width=14, height=7)
#pdf(file="l1ou_.meansandextremes_test_autocols_numbered_batlowrev.pdf", width=14, height=7)
#pdf(file="l1ou_.meansandextremes_test_autocols_numbered_hawaii.pdf", width=14, height=7)
#pdf(file="l1ou_.meansandextremes_test_autocols_numbered_hawaiirev.pdf", width=14, height=7)
#pdf(file="l1ou_.meansandextremes_test_autocols_numbered_tofino.pdf", width=14, height=7)
#pdf(file="l1ou_.meansandextremes_test_autocols_numbered_oleron.pdf", width=14, height=7)
#pdf(file="l1ou_.meansandextremes_test_autocols_numbered_tokyo.pdf", width=14, height=7)
#pdf(file="l1ou_.meansandextremes_test_autocols_numbered_acton.pdf", width=14, height=7)
#pdf(file="l1ou_.meansandextremes_test_autocols_numbered_bilbao.pdf", width=14, height=7)
#pdf(file="l1ou_.meansandextremes_test_autocols_numbered_bamako.pdf", width=14, height=7)
#pdf(file="l1ou_.meansandextremes_test_autocols_numbered_imola.pdf", width=14, height=7)
#pdf(file="l1ou_.meansandextremes_test_autocols_numbered_corkO.pdf", width=14, height=7)
#pdf(file="l1ou_.meansandextremes_test_autocols_numbered_tokyo_w_veg_wladderizedMCC_2state_updated.pdf", width=7, height=7)
tiff(file="l1ou_.meansandextremes_test_autocols_numbered_tokyo_w_veg_wladderizedMCC_2state_updated.tiff", width=7, height=7, units="in", res=600)

#pdf(file="l1ou_.meansandextremes_test_autocols_numbered_romaO.pdf", width=14, height=7)
plot.l1ou.mod(conv.reg.rlga1.meansandextremes,palette = my.cols.auto,edge.shift.ann = FALSE,asterisk=FALSE,Number = TRUE,edge.width=3) 

#now load vegetative regime and plot node labels
load("/xxxx/asr_2state_vegetative_regime_openvsmixedclosed/pp2s_vegetative_regime_openvsmixedclosed_MCC.Rsave")
pp1$phy$tip.label<-gsub("G_","C_",pp1$phy$tip.label)
#pp1$phy$tip.label<-gsub("_"," ",pp1$phy$tip.label)

#tip states for vegetative regime
veg<-read.csv(file="/xxxx/asr_2state_vegetative_regime_openvsmixedclosed/OTU_97.5_BIOCLIM_SUMMARY_MODIFIED_UNIQUES_vegetative_regimes.csv",stringsAsFactors=FALSE)

#change from clade G to C
veg$OTU<-gsub("G_","C_",veg$OTU)
#veg$OTU<-gsub("_"," ",veg$OTU)

tips.veg<-veg$habcodopenvsmixedclosed[order(match(veg$OTU,pp1$phy$tip.label))]
tips.veg[tips.veg %in% "0"]<-"dark green"
tips.veg[tips.veg %in% "1"]<-"wheat3"
tips.veg[tips.veg %in% "?"]<-"white"

#remove border around pies http://blog.phytools.org/2016/10/plotting-multiple-pie-charts-at-nodes.html
par(fg="transparent")
nodelabels(pie=pp1$states,cex=0.75,piecol=c("dark green","wheat3"))
par(fg="black")
append2tips(pp1$phy,pch=19,col=tips.veg,align=TRUE,cex=1)

#Plot numbers again over pies - taken from l1ou plot function and modified slightly
tree = conv.reg.rlga1.meansandextremes$tree
s.c = conv.reg.rlga1.meansandextremes$shift.configuration
edge.ann.cex<-1
Z = l1ou:::generate_design_matrix(tree, type = "apprX")
for (idx in 1:length(s.c)) {
    sP = s.c[[idx]]
    pos = max(Z[, sP])
    edge.labels = rep(NA, length(tree$edge[, 1]))
    edge.labels[sP] = 1+as.numeric(names(s.c)[idx])
    edgelabels(edge.labels, cex = 1 * edge.ann.cex, adj = c(1.2, 0.5), frame = "none", date = pos)
}

legend(x=-6,y=82,c("Forested Only,","Forested & Non-Forested","","Non-Forested Only"),col=c("dark green","white","white","wheat3"),cex=1,bty="n",pt.cex=2,pch=19,inset=0.05)
text(x=-3,y=82,"Vegetative Regime (Node Pie Charts/Tip Circles)",cex=1,font=2,adj=c(0,0))

#add timescale
#read timescale in
timescale_ics2020<-read.csv(file="~/agaricomycotina_ecm_vortex/timescale_ics2020.csv")

#reduce to just periods of interest
timescale_ics2020<-timescale_ics2020[timescale_ics2020$Type %in% "Period",]
timey<-timescale_ics2020[timescale_ics2020$Start<550,]
timey$Name<-c(NA,"Ng","Pg","K","J","T","P","C","D","S","O","Ca")

require(phytools)

#make rgb color
for(x in 1:nrow(timey)){
	timey$RGB[x]<-rgb(timey$Col_R[x]/255,timey$Col_G[x]/255,timey$Col_B[x]/255)
}

#upper<-max(nodeHeights(pp1$phy))
upper<-140

rect(xleft=0-1.55,xright=upper-timey$End[4]-1.55,ytop=0,ybottom=-10,col=timey$RGB[4])
text(x=mean(c(0,upper-timey$End[4]))-5,y=-1.5,timey$Name[4],cex=1,font=1,adj=c(0,0))

rect(xleft=upper-timey$Start[3]-1.55,xright=upper-timey$End[3]-1.55,ytop=0,ybottom=-10,col=timey$RGB[3])
text(x=mean(c(upper-timey$Start[3],upper-timey$End[3]))-5,y=-1.5,timey$Name[3],cex=1,font=1,adj=c(0,0))

rect(xleft=upper-timey$Start[2]-1.55,xright=upper-timey$End[2]-1.55,ytop=0,ybottom=-10,col=timey$RGB[2])
text(x=mean(c(upper-timey$Start[2],upper-timey$End[2]))-5,y=-1.5,timey$Name[2],cex=1,font=1,adj=c(0,0))

rect(xleft=upper-timey$Start[1]-1.55,xright=upper-1.55,ytop=0,ybottom=-10,col=timey$RGB[1])
text(x=mean(c(upper-timey$Start[1],upper))-5,y=-1.5,timey$Name[1],cex=1,font=1,adj=c(0,0))

axisPhylo()
mtext("MY Before Present", side = 1, line = 1.85, cex=0.65, at=70)

#add clade designations
text(x=175,y=54,"A",cex=1,font=2,adj=c(0,0))
text(x=175,y=31,"I",cex=1,font=2,adj=c(0,0))
text(x=175,y=19,"C",cex=1,font=2,adj=c(0,0))
text(x=175,y=6,"S",cex=1,font=2,adj=c(0,0))

segments(x0=172,x1=172,y0=81.25,y1=36.75,col="black")
segments(x0=172,x1=172,y0=36.25,y1=26.75,col="black")
segments(x0=172,x1=172,y0=26.25,y1=12.75,col="black")
segments(x0=172,x1=172,y0=12.25,y1=0.75,col="black")

dev.off()












################mean and extremes
#####Posterior Trees
require(geiger)
require(l1ou)
#read data and subset
taxa<-read.csv(file="/xxxx/OTU_97.5_BIOCLIM_SUMMARY_MODIFIED_UNIQUES_wSE_corrected.csv",stringsAsFactors=FALSE,row.names=1)
dat<-taxa[,c("Mean_Bio1","Mean_Bio12")]

#randomly select 100 trees out of the previous 1000 from the posterior
#trees<-read.nexus(file="/xxxx/burnin25pcn_resampevery20k_OTUs_renamed_bioclim_retained_1krandomlyselected_MODIFIED.trees")
#randomly select 100
#r100<-sort(sample(1:1000,100,replace=FALSE))
#r100<-c(5, 6, 16, 18, 21, 32, 47, 56, 69, 85, 86, 95, 109, 122, 129, 134, 143, 148, 152, 163, 175, 184, 202, 230, 232, 241, 273, 289, 294, 299, 300, 320, 344, 395, 402, 409, 412, 427, 439, 442, 444, 445, 448, 463, 473, 478, 500, 501, 503, 516, 523, 541, 551, 563, 564, 565, 568, 569, 574, 578, 584, 587, 592, 604, 614, 616, 635, 654, 655, 656, 657, 663, 682, 685, 703, 709, 723, 728, 729, 742, 773, 784, 807, 808, 811, 849, 854, 857, 864, 871, 877, 897, 904, 925, 940, 943, 944, 948, 969, 988)
#tr<-trees[r100]
#write.nexus(tr,file="/xxxx/burnin25pcn_resampevery20k_OTUs_renamed_bioclim_retained_100randomlyselected_MODIFIED.trees")

#read results
df<-read.csv(file="/xxxx/l1ou_eb_analyses_wladderizedMCC_2state.csv",stringsAsFactors=FALSE)


#read 100 trees
tr<-read.nexus(file="/xxxx/burnin25pcn_resampevery20k_OTUs_renamed_bioclim_retained_100randomlyselected_MODIFIED.trees")

tr[[1]]$tip.label
for(x in 1:length(tr)){
	tr[[x]]<-ladderize(tr[[x]],FALSE)
}

#WRITE IT AND READ IN AGAIN
write.tree(tr,file="/xxxx/burnin25pcn_resampevery20k_OTUs_renamed_bioclim_retained_100randomlyselected_MODIFIED_ladderized_2state.tre")
tr<-read.tree(file="/xxxx/burnin25pcn_resampevery20k_OTUs_renamed_bioclim_retained_100randomlyselected_MODIFIED_ladderized_2state.tre")
tr[[1]]$tip.label


for(x in 1:length(tr)){
	#blank everything out from previous round
	rlga.lasso.meansandextremes<-NULL
	conv.reg.rlga1.meansandextremes<-NULL
	convergent.rlga.regimes<-NULL
	dis.dat<-NULL
	er.none<-NULL
	sym.none<-NULL
	#ard.none<-NULL
	er.eb.dec<-NULL
	sym.eb.dec<-NULL
	#ard.eb.dec<-NULL
	er.eb.inc<-NULL
	sym.eb.inc<-NULL
	#ard.eb.inc<-NULL
	AICc.scores<-NULL
	AICc.weights<-NULL
	
	#use phylogenetic lasso to detect shifts in regimes and identify convergent regimes
	cat(paste("Starting tree",x,"\n",sep=" "))
	rlga.lasso.meansandextremes <- adjust_data(tr[[x]], dat, normalize = F)
	RLGAModel.meansandextremes <- estimate_shift_configuration(rlga.lasso.meansandextremes$tree, rlga.lasso.meansandextremes$Y, rescale=T, root.model = "OUfixedRoot", criterion="pBIC", nCores=15, max.nShifts=42)
	#number of shifts	
	df[x,"L1OU_Orig_Shifts"]<-RLGAModel.meansandextremes$nShifts
	#pBIC score
	df[x,"L1OU_Orig_pBIC"]<-RLGAModel.meansandextremes$score
	
	#identify convergente regimes
	conv.reg.rlga1.meansandextremes <- estimate_convergent_regimes(RLGAModel.meansandextremes, method="backward", criterion = "pBIC", fixed.alpha = FALSE,nCores=15)
	#number of shifts	
	df[x,"L1OU_Conv_Shifts"]<-conv.reg.rlga1.meansandextremes$nShifts
	#number of unique regimes...NOTE...this DOES NOT INCLUDE THE "basal" regime - this is added below.
	df[x,"L1OU_Conv_Regimes"]<-length(unique(names(conv.reg.rlga1.meansandextremes$shift.configuration)))+1
	#pBIC score
	df[x,"L1OU_Conv_pBIC"]<-conv.reg.rlga1.meansandextremes$score

	#summarize.lasso.convergence.output function is from Slater & Friscia (2019)
	convergent.rlga.regimes <-try(summarize.lasso.convergence.output(conv.reg.rlga1.meansandextremes))
	dis.dat<-convergent.rlga.regimes[,2]
	names(dis.dat)<-convergent.rlga.regimes[,1]
	
	cat(paste("\t\t\t...fitting time-constant models for tree",x,"\n",sep=" "))
	#fit time-constant models
	er.none<-fitDiscrete(tr[[x]],dis.dat,model="ER",transform="none",ncores=15,control=list(niter=500))	
	df[x,c("ER_Likelihood","ER_AIC","ER_AICc")]<-c(er.none$opt$lnL,er.none$opt$aic,er.none$opt$aicc)
	sym.none<-fitDiscrete(tr[[x]],dis.dat,model="SYM",transform="none",ncores=15,control=list(niter=500))
	df[x,c("SYM_Likelihood","SYM_AIC","SYM_AICc")]<-c(sym.none$opt$lnL,sym.none$opt$aic,sym.none$opt$aicc)
	ard.none<-fitDiscrete(tr[[x]],dis.dat,model="ARD",transform="none",ncores=15,control=list(niter=500))
	df[x,c("ARD_Likelihood","ARD_AIC","ARD_AICc")]<-c(ard.none$opt$lnL,ard.none$opt$aic,ard.none$opt$aicc)

	cat(paste("\t\t\t...fitting decreasing models for tree",x,"\n",sep=" "))
	#fit time-variant (decreasing) models
	er.eb.dec<-fitDiscrete(tr[[x]],dis.dat,model="ER",transform="EB",ncores=15,control=list(niter=500),bounds=list(a=c(min=-10,max=-0.000001)))
	df[x,c("EREBD_Alpha","EREBD_Likelihood","EREBD_AIC","EREBD_AICc")]<-c(er.eb.dec$opt$a,er.eb.dec$opt$lnL,er.eb.dec$opt$aic,er.eb.dec$opt$aicc)
	sym.eb.dec<-fitDiscrete(tr[[x]],dis.dat,model="SYM",transform="EB",ncores=15,control=list(niter=500),bounds=list(a=c(min=-10,max=-0.000001)))
	df[x,c("SYMEBD_Alpha","SYMEBD_Likelihood","SYMEBD_AIC","SYMEBD_AICc")]<-c(sym.eb.dec$opt$a,sym.eb.dec$opt$lnL,sym.eb.dec$opt$aic,sym.eb.dec$opt$aicc)
	ard.eb.dec<-fitDiscrete(tr[[x]],dis.dat,model="ARD",transform="EB",ncores=15,control=list(niter=500),bounds=list(a=c(min=-10,max=-0.000001)))
	df[x,c("ARDEBD_Alpha","ARDEBD_Likelihood","ARDEBD_AIC","ARDEBD_AICc")]<-c(ard.eb.dec$opt$a,ard.eb.dec$opt$lnL,ard.eb.dec$opt$aic,ard.eb.dec$opt$aicc)
	
	cat(paste("\t\t\t...fitting increasing models for tree",x,"\n",sep=" "))
	#fit time-variant (increasing) models
	er.eb.inc<-fitDiscrete(tr[[x]],dis.dat,model="ER",transform="EB",ncores=15,control=list(niter=500),bounds=list(a=c(min=0.000001,max=10)))
	df[x,c("EREBI_Alpha","EREBI_Likelihood","EREBI_AIC","EREBI_AICc")]<-c(er.eb.inc$opt$a,er.eb.inc$opt$lnL,er.eb.inc$opt$aic,er.eb.inc$opt$aicc)
	sym.eb.inc<-fitDiscrete(tr[[x]],dis.dat,model="SYM",transform="EB",ncores=15,control=list(niter=500),bounds=list(a=c(min=0.000001,max=10)))
	df[x,c("SYMEBI_Alpha","SYMEBI_Likelihood","SYMEBI_AIC","SYMEBI_AICc")]<-c(sym.eb.inc$opt$a,sym.eb.inc$opt$lnL,sym.eb.inc$opt$aic,sym.eb.inc$opt$aicc)
	ard.eb.inc<-fitDiscrete(tr[[x]],dis.dat,model="ARD",transform="EB",ncores=15,control=list(niter=500),bounds=list(a=c(min=0.000001,max=10)))
	df[x,c("ARDEBI_Alpha","ARDEBI_Likelihood","ARDEBI_AIC","ARDEBI_AICc")]<-c(ard.eb.inc$opt$a,ard.eb.inc$opt$lnL,ard.eb.inc$opt$aic,ard.eb.inc$opt$aicc)
	
	cat(paste("\t\t\t...finishing up for tree",x,"\n",sep=" "))
	#Get AICc Weights and identify model w best fit
	AICc.scores<-c(er.none$opt$aicc,sym.none$opt$aicc,ard.none$opt$aicc,er.eb.dec$opt$aicc,sym.eb.dec$opt$aicc,ard.eb.dec$opt$aicc,er.eb.inc$opt$aicc,sym.eb.inc$opt$aicc,ard.eb.inc$opt$aicc)
	#AICc.scores<-c(er.none$opt$aicc,sym.none$opt$aicc,er.eb.dec$opt$aicc,sym.eb.dec$opt$aicc,er.eb.inc$opt$aicc,sym.eb.inc$opt$aicc)
	names(AICc.scores)<-c("ER","SYM","ARD","EREBD","SYMEBD","ARDEBD","EREBI","SYMEBI","ARDEBI")
	#names(AICc.scores)<-c("ER","SYM","EREBD","SYMEBD","EREBI","SYMEBI")
	AICc.weights<-aicw(AICc.scores)
	df[x,c("ER_AICcWeight","SYM_AICcWeight","ARD_AICcWeight","EREBD_AICcWeight","SYMEBD_AICcWeight","ARDEBD_AICcWeight","EREBI_AICcWeight","SYMEBI_AICcWeight","ARDEBI_AICcWeight")]<-AICc.weights$w
	#df[x,c("ER_AICcWeight","SYM_AICcWeight","EREBD_AICcWeight","SYMEBD_AICcWeight","EREBI_AICcWeight","SYMEBI_AICcWeight")]<-AICc.weights$w
	df[x,"Best_Model"]<-rownames(AICc.weights)[AICc.weights$w %in% max(AICc.weights$w)]
	write.csv(df,file="/xxxx/l1ou_eb_analyses_wladderizedMCC_2state.csv",row.names=FALSE)
	save(rlga.lasso.meansandextremes,RLGAModel.meansandextremes,conv.reg.rlga1.meansandextremes,convergent.rlga.regimes,dis.dat,er.none,sym.none,ard.none,er.eb.dec,sym.eb.dec,ard.eb.dec,er.eb.inc,sym.eb.inc,ard.eb.inc,AICc.weights,AICc.scores,file=paste("/xxxx/l1ou_eb_analyses_tree_",x,"_2state.RData",sep=""))
	cat(paste("\t\t\t\t\t...finished tree",x,"\n",sep=" "))
}



#read back in the results...
dat<-read.csv(file="/xxxx/l1ou_eb_analyses_wladderizedMCC_2state.csv",stringsAsFactors=FALSE)

dat$L1OU_Orig_Shifts[101]
dat$L1OU_Conv_Shifts[101]

#MCC tree regimes
dat$L1OU_Conv_Regimes[101]

#Posterior shifts - should be same
summary(dat$L1OU_Orig_Shifts[1:100])
summary(dat$L1OU_Conv_Shifts[1:100])

#Posterior tree regimes
summary(dat$L1OU_Conv_Regimes[1:100])


table(dat$Best_Model[1:100])
dat$Best_Model[101]

dat$EREBI_Alpha[101]

#look at alpha
dat$EREBI_Alpha[dat$Best_Model_ERONLY[1:100]=="EREBI"]
summary(dat$EREBI_Alpha[dat$Best_Model_ERONLY[1:100]=="EREBI"])
dat$ER_AICcWeight_ERONLY[dat$Best_Model_ERONLY[1:100]=="EREBI"]
summary(dat$ER_AICcWeight_ERONLY[dat$Best_Model_ERONLY[1:100]=="EREBI"])

#look at weights...
dat$EREBI_AICcWeight_ERONLY[dat$Best_Model_ERONLY[1:100]=="EREBI"]-dat$ER_AICcWeight_ERONLY[dat$Best_Model_ERONLY[1:100]=="EREBI"]
dat$EREBI_AICcWeight_ERONLY[101]-dat$ER_AICcWeight_ERONLY[101]


#plot this?
#column for each tree and show AICc Weight for each tree

table(dat$Best_Model[1:100])




#get info about average/sd for each variable
round(mean(conv.reg.rlga1.meansandextremes$Y[,"BIO1"]),1)
round(mean(conv.reg.rlga1.meansandextremes$Y[,"BIO5"]),1)
round(mean(conv.reg.rlga1.meansandextremes$Y[,"BIO6"]),1)
round(mean(conv.reg.rlga1.meansandextremes$Y[,"BIO12"]),1)
round(mean(conv.reg.rlga1.meansandextremes$Y[,"BIO13"]),1)
round(mean(conv.reg.rlga1.meansandextremes$Y[,"BIO14"]),1)

round(sd(conv.reg.rlga1.meansandextremes$Y[,"BIO1"]),1)
round(sd(conv.reg.rlga1.meansandextremes$Y[,"BIO5"]),1)
round(sd(conv.reg.rlga1.meansandextremes$Y[,"BIO6"]),1)
round(sd(conv.reg.rlga1.meansandextremes$Y[,"BIO12"]),1)
round(sd(conv.reg.rlga1.meansandextremes$Y[,"BIO13"]),1)
round(sd(conv.reg.rlga1.meansandextremes$Y[,"BIO14"]),1)

#check numbers of sites for each OTU in convergent 
taxa<-read.csv(file="/xxxx/OTU_97.5_BIOCLIM_SUMMARY_MODIFIED_UNIQUES_wSE_corrected.csv",stringsAsFactors=FALSE,row.names=1)
taxa[1:10,1:3]


#most convergent regimes include OTUs w samples from multiple sites.  Regime 3 is an exception.  Regime 6 has two of three OTUs w single, Regime 9 has 6/8 singletons...
reg2<-c("G_97.5_8","I_97.5_3","I_97.5_8")
length(reg2)
taxa[rownames(taxa) %in% reg2,1:3]

reg3<-c("I_97.5_12","A_97.5_72")
length(reg3)
taxa[rownames(taxa) %in% reg3,1:3]

reg6<-c("A_97.5_64","A_97.5_22","A_97.5_49")
length(reg6)
taxa[rownames(taxa) %in% reg6,1:3]

reg7<-c("A_97.5_12","A_97.5_33","A_97.5_16")
length(reg7)
taxa[rownames(taxa) %in% reg7,1:3]

reg9<-c("S_97.5_10","S_97.5_21","S_97.5_22","S_97.5_7","S_97.5_1","A_97.5_1","A_97.5_21","A_97.5_25")
length(reg9)
taxa[rownames(taxa) %in% reg9,1:3]

reg11<-c("A_97.5_8","A_97.5_31")
length(reg11)
taxa[rownames(taxa) %in% reg11,1:3]

