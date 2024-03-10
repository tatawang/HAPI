######################### Functions  #########################
#########################  generate the missing emission table by SCC
floor_dec <- function(x, level=1) round(x - 5*10^(-level-1), level)

load_files <- function(pathf){
	INVdata = list()
	filelist <- list.files( path = pathf, pattern="*.csv")
	for (i in 1:length(filelist)){
		if (i==1){
			read.csv( paste(pathf, filelist[i], sep=""), comment.char = '#',head=TRUE,sep="," ) -> tmpa
			INVdata = rbind(INVdata,tmpa)
			colnamelist <- colnames(tmpa)
		}else {
			read.csv( paste(pathf, filelist[i], sep=""), comment.char = '#',head=TRUE,sep="," ) -> tmpb
			colnames(tmpb) <-colnamelist
			INVdata = rbind(INVdata,tmpb)
			}
		}
	return(INVdata)
}


gen_state_name_list <- function(input){
	output<-list()
	for (j in 1:length(input)){
			output_a <- state_list[which(input[j] == state_list$FIPS),"Name",]
			output <- cbind(output,output_a)
		}
		return(output)
}

add_state_lab <- function(input, state_list){
	if("state" %in% colnames(input)){
		for (j in 1:length(input[,1])){
			input$state_t[j] <- state_list[which(input$state[j] == state_list$FIPS),"Name",]
		}	
	} else{
		input$state <-  floor_dec(input$Region/1000)
		for (j in 1:length(input[,1])){
			input$state_t[j] <- state_list[which(input$state[j] == state_list$FIPS),"Name",]
		}
	}
	return(input)
}

gen_state_sum <- function(ct_scc_input_a, SPC, state_name_list){
	report <- array(NA, dim = c(length(state_name_list),length(SPC)))
	for(s in (1:length(SPC))){
		pre_report <- tapply(ct_scc_input_a[,SPC[s]],ct_scc_input_a$state_t, sum) 
		for(t in 1:length(dimnames(pre_report)[[1]]))
			report[which(state_name_list==dimnames(pre_report)[[1]][t]), s] <- pre_report[dimnames(pre_report)[[1]][t]]
	}
	colnames(report) <- SPC
	rownames(report) <- state_name_list
	return(report)
}

make_rep_county_scc <- function(INV_input, state_num_list, MSPC_CODE, repSPC ){
	INV_input_argd <-INV_input[which(INV_input$poll %in% MSPC_CODE & INV_input$stat %in% state_num_list), ]
	examtable = tapply(INV_input_argd$ann_value, list(INV_input_argd$region_cd, INV_input_argd$poll,INV_input_argd$scc), sum, na.rm=TRUE)
	cl= length(examtable[,1,1])
	sccl = length(examtable[1,1,])
	xtable = list()
	for (i in 1:cl){
		examtable_a = cbind(rep(dimnames(examtable)[[1]][i], each = sccl ),dimnames(examtable)[[3]],t(examtable[i,,]))
		xtable = rbind(xtable , examtable_a)
	}
	colist = list()
	for ( s in 3:(length(colnames(xtable))-1)){
		colist = rbind(colist, repSPC[which(MSPC_CODE == colnames(xtable)[s])])
	}
	colist = rbind('Region','SCC',colist,"I.VOC_INV")
	colnames(xtable) <- colist
	if (length(colist)< (length(MSPC_CODE)+2)){
		miss_spc <- repSPC[!(repSPC %in% colist[3:length(colist),1] )]
		print(paste("Missing ",miss_spc ,"in inventory, need gap fill with empty column", sep=""))
		for (i in 1:length(miss_spc)){
			xtable <- cbind(xtable, NA)
		}
		colnames(xtable)[(length(colist)+1):(length(colist)+length(miss_spc))] <- miss_spc
	}
	return(xtable)
}

make_rep_county_scc_mkup <- function(INV_input, state_list, MSPC_CODE, SPC ){
	INV_input_argd <-INV_input[which(INV_input$poll %in% MSPC_CODE), ]
	examtable = tapply(INV_input_argd$ann_value, list(INV_input_argd$region_cd, INV_input_argd$poll,INV_input_argd$scc), sum, na.rm=TRUE)
	cl= length(examtable[,1,1])
	sccl = length(examtable[1,1,])
	#print(cl, sccl)
	xtable = list()
	for (i in 1:cl){
		examtable_a = cbind(rep(dimnames(examtable)[[1]][i], each = sccl ),dimnames(examtable)[[3]],t(examtable[i,,]))
		xtable = rbind(xtable , examtable_a)
	}
	colist = list()
	for ( s in 3:(length(colnames(xtable))-1)){
		colist = rbind(colist, repSPC[which(MSPC_CODE == colnames(xtable)[s])])
	}
	colist = rbind('Region','SCC',colist,"I.VOC_INV")
	colnames(xtable) <- colist
	if (length(colist)< (length(MSPC_CODE)+2)){
		miss_spc <- repSPC[!(repSPC %in% colist[3:length(colist),1] )]
		print(paste("Missing ",miss_spc ,"in inventory, add empty column for missing species", sep=""))
		for (i in 1:length(miss_spc)){
			xtable <- cbind(xtable, NA)
		}
		colnames(xtable)[(length(colist)+1):(length(colist)+length(miss_spc))] <- miss_spc
	}
	return(xtable)
}


Missing_EM_SCC <- function( sector, sector_mkup,species ){
	sector <- sector[which(sector$I.XYLENES + sector$I.TOLUENE + sector$I.STYRENE + sector$I.ETHYLBENZ + sector$ACROLEIN + sector$BUTADIE < sector$I.VOC_INV  & sector$I.VOC_INV > 0),]
	No_STEX_org <- sector[which(sector$I.TOLUENE == 0 & sector$I.XYLENE == 0 & sector$I.STYRENE == 0 & sector$I.ETHYLBENZ == 0 & sector$ACROLEIN == 0 & sector$BUTADIE == 0),] ## & sector$ACROLEIN == 0 & sector$BUTADIE == 0
	W_STEX_org <- sector[which(sector$I.TOLUENE != 0 | sector$I.XYLENE != 0 | sector$I.STYRENE != 0 | sector$I.ETHYLBENZ != 0| sector$ACROLEIN != 0 | sector$BUTADIE != 0),] ## | sector$ACROLEIN != 0 | sector$BUTADIE != 0
	W_STEX_mkup <- sector_mkup[which(sector_mkup$I.TOLUENE != 0 | sector_mkup$I.XYLENE != 0 | sector_mkup$I.STYRENE != 0 | sector_mkup$I.ETHYLBENZ != 0| sector_mkup$ACROLEIN != 0 | sector_mkup$BUTADIE != 0),]
	No_STEX <- No_STEX_org[which(No_STEX_org$I.VOC_INV > 0 ),] 
	W_STEX <- W_STEX_org[which(W_STEX_org$I.VOC_INV  > 0 ),]
	W_STEX_mkup <- W_STEX_mkup[which(W_STEX_mkup$I.VOC_INV  > 0 ),]
	SCC_noSTEX <- unique(No_STEX$SCC)
	SCC_wSTEX <- unique(W_STEX$SCC)
	SCC_wSTEX_mkup <- unique(W_STEX_mkup$SCC)
	pqSCC <- SCC_noSTEX[SCC_noSTEX %in% SCC_wSTEX]
	pqSCC_mkup_org <- SCC_noSTEX[SCC_noSTEX %in% SCC_wSTEX_mkup]
	pqSCC_mkup_adj <- pqSCC_mkup_org[which(!(pqSCC_mkup_org %in% pqSCC))]
	No_STEX_DESC <- list()
	for (x in 1:length(SCC_noSTEX)){
		No_STEX_DESC <- rbind(No_STEX_DESC, No_STEX[which(No_STEX$SCC == SCC_noSTEX[x])[1],c("SCC","I.VOC_INV")])
	}
	#write.csv(No_STEX_DESC, "nonpt_noSTEX.csv")
	W_STEX_DESC <- list()
	for (x in 1:length(SCC_wSTEX)){
		W_STEX_DESC <- rbind(W_STEX_DESC, W_STEX[which(W_STEX$SCC == SCC_wSTEX[x])[1],c("SCC","I.VOC_INV")])
	}
	#write.csv(W_STEX_DESC, "nonpt_wSTEX.csv")
	##############
	##print(pqSCC)
	pq_sum_VOCwSTEX = 0.  ### the no missing part SCC with STEX available !
	pq_sum_TOLUENEwSTEX = 0.  ### the no missing part SCC with STEX available !
	pq_sum_VOC = 0. ### for the missing STEX with SCC
	W_STEX[(W_STEX$SCC %in% pqSCC),] ->W_STEX_misSCC
	No_STEX[(No_STEX$SCC %in% pqSCC),] ->No_STEX_misSCC
	No_STEX[(No_STEX$SCC %in% pqSCC_mkup_adj),] ->No_STEX_misSCC_mkup
	W_STEX_mkup[(W_STEX_mkup$SCC %in% pqSCC_mkup_adj),] ->W_STEX_mkup_misSCC
#	tapply(W_STEX_misSCC$I.VOC_INV + W_STEX_misSCC$EVP__VOC_INV , W_STEX_misSCC$SCC, sum )-> sumVOCbySCC
#	tapply(W_STEX_misSCC[,species], W_STEX_misSCC$SCC, sum )-> sumSpcbySCC
	tapply(W_STEX_misSCC$I.VOC_INV , W_STEX_misSCC$SCC, sum )-> sumVOCbySCC
	tapply(W_STEX_misSCC[,species], W_STEX_misSCC$SCC, sum )-> sumSpcbySCC
	tapply(W_STEX_mkup_misSCC$I.VOC_INV , W_STEX_mkup_misSCC$SCC, sum )-> sumVOCbySCC_mkup
	tapply(W_STEX_mkup_misSCC[,species], W_STEX_mkup_misSCC$SCC, sum )-> sumSpcbySCC_mkup
	tapply(No_STEX_misSCC$I.VOC_INV , No_STEX_misSCC$SCC, sum )-> sumVOCnoSTEXbySCC
	tapply(No_STEX_misSCC_mkup$I.VOC_INV , No_STEX_misSCC_mkup$SCC, sum )-> sumVOCnoSTEXbySCC_mkup
	missing_ems_estimate <-  sumVOCnoSTEXbySCC * sumSpcbySCC / sumVOCbySCC
	missing_ems_estimate_mkup <-  sumVOCnoSTEXbySCC_mkup * sumSpcbySCC_mkup / sumVOCbySCC_mkup
	checktable<- (cbind( sumSpcbySCC, sumVOCbySCC, sumSpcbySCC/sumVOCbySCC, sumVOCnoSTEXbySCC  ,missing_ems_estimate))
	if(length(missing_ems_estimate_mkup)>1){
    		checktable2<- (cbind( sumSpcbySCC_mkup, sumVOCbySCC_mkup, sumSpcbySCC_mkup/sumVOCbySCC_mkup, sumVOCnoSTEXbySCC_mkup  ,missing_ems_estimate_mkup))
    	checktable <- rbind(checktable, checktable2)
    }
    print(paste("Total IVOC:",sum(sector$I.VOC_INV)," ; Missing HAPs IVOC:", sum(No_STEX$I.VOC_INV), " ; IVOC can do HAPI:",sum(checktable[,4])," ; Total VOC can not do HAPI:",round((sum(No_STEX$I.VOC_INV)-sum(checktable[,4])),0)," ; HAPI % for VOC", round((sum(checktable[,4]))/sum(No_STEX$I.VOC_INV)*100,0),"%", sep="") )
    print(paste(" Total missing SCC is ",length(SCC_noSTEX)," ; Local match SCC is ", length(pqSCC), " ; Other State match SCC is ", length(pqSCC_mkup_adj),sep="" ))
    colnames(checktable)<-c(paste(species,"wSTEX"),"; VOCwSTEX","T/Vratio","VOCwoSTEX","Missing EMIS")
    checktable <- checktable[which(checktable[,2] != 0),]
    return(checktable)
}

#########################  generate the only missing SCC table (No SCC aggregate, for INV data input )

Missing_EM_LIST <- function( sector){
	sector <- sector[which(sector$I.XYLENES + sector$I.TOLUENE + sector$I.STYRENE + sector$I.ETHYLBENZ + sector$ACROLEIN + sector$BUTADIE < sector$I.VOC_INV  & sector$I.VOC_INV > 0),]
	No_STEX_org <- sector[which(sector$I.TOLUENE == 0 & sector$I.XYLENE == 0 & sector$I.STYRENE == 0 & sector$I.ETHYLBENZ == 0 & sector$ACROLEIN == 0 & sector$BUTADIE == 0),] ### & sector$ACROLEIN == 0 & sector$BUTADIE == 0
	W_STEX_org <- sector[which(sector$I.TOLUENE != 0 | sector$I.XYLENE != 0 | sector$I.STYRENE != 0 | sector$I.ETHYLBENZ != 0 | sector$ACROLEIN != 0 | sector$BUTADIE != 0),] ## | sector$ACROLEIN != 0 | sector$BUTADIE != 0
	No_STEX <- No_STEX_org[which(No_STEX_org$I.VOC_INV > 0 ),] 
	W_STEX <- W_STEX_org[which(W_STEX_org$I.VOC_INV  > 0 ),] 
	SCC_noSTEX <- unique(No_STEX$SCC)
	SCC_wSTEX <- unique(W_STEX$SCC)
	pqSCC <- SCC_noSTEX[SCC_noSTEX %in% SCC_wSTEX]
	##print(pqSCC)
	pq_sum_VOCwSTEX = 0.  ### the no missing part SCC with STEX available !
	pq_sum_TOLUENEwSTEX = 0.  ### the no missing part SCC with STEX available !
	pq_sum_VOC = 0. ### for the missing STEX with SCC
	W_STEX[(W_STEX$SCC %in% pqSCC),] ->W_STEX_misSCC
	No_STEX[(No_STEX$SCC %in% pqSCC),] ->No_STEX_misSCC
    return(No_STEX_misSCC)
}

######################### make np_oilgas INV smoke input data #########################
#np_oilgas_2011NEIv2_STEX_missing.csv
write_missing_np_INV <- function(name, MISSING_DF, No_STEX_misSCC, MSPC, MSPC_CODE ){	
	sink(name)
	cat("country_cd,region_cd,tribal_code,census_tract_cd,shape_id,scc,emis_type,poll,ann_value,ann_pct_red,control_ids,control_measures,current_cost,cumulative_cost,projection_factor,reg_codes,calc_method,calc_year,date_updated,data_set_id,jan_value,feb_value,mar_value,apr_value,may_value,jun_value,jul_value,aug_value,sep_value,oct_value,nov_value,dec_value,jan_pctred,feb_pctred,mar_pctred,apr_pctred,may_pctred,jun_pctred,jul_pctred,aug_pctred,sep_pctred,oct_pctred,nov_pctred,dec_pctred,comment")
	cat("\n")
	No_STEX_misSCC[is.na(No_STEX_misSCC)] <- ""
	for (l in 1: length(No_STEX_misSCC[,1])){
		for (s in 1:length(MSPC)){
cat('"US"',',"',MISSING_DF$Region[l],'",,,,"',MISSING_DF$SCC[l],'",,"',MSPC_CODE[s],'",',MISSING_DF[l,MSPC[s]],',,,,,,,,,,20210828,"2021NIEHS",,,,,,,,,,,,,,,,,,,,,,,,,',sep="")
			cat('\n')
		}
	}
	sink()
}
#########################  make pt INV smoke input data  #########################
#name <- "ptnonipm_2011NEIv2_STEXAB_missing.csv"
write_missing_pt_INV <- function(name, PT_MISSING_DF, No_STEX_misSCC, MSPC, MSPC_CODE ){	
	sink(name)
	cat("country_cd,region_cd,tribal_code,facility_id,unit_id,rel_point_id,process_id,agy_facility_id,agy_unit_id,agy_rel_point_id,agy_process_id,scc,poll,ann_value,ann_pct_red,facility_name,erptype,stkhgt,stkdiam,stktemp,stkflow,stkvel,naics,longitude,latitude,ll_datum,horiz_coll_mthd,design_capacity,design_capacity_units,reg_codes,fac_source_type,unit_type_code,control_ids,control_measures,current_cost,cumulative_cost,projection_factor,submitter_id,calc_method,data_set_id,facil_category_code,oris_facility_code,oris_boiler_id,ipm_yn,calc_year,date_updated,fug_height,fug_width_ydim,fug_length_xdim,fug_angle,zipcode,annual_avg_hours_per_year,jan_value,feb_value,mar_value,apr_value,may_value,jun_value,jul_value,aug_value,sep_value,oct_value,nov_value,dec_value,jan_pctred,feb_pctred,mar_pctred,apr_pctred,may_pctred,jun_pctred,jul_pctred,aug_pctred,sep_pctred,oct_pctred,nov_pctred,dec_pctred,comment")
	cat("\n")
	PT_MISSING_DF[is.na(PT_MISSING_DF)] <- ""
	for (l in 1: length(PT_MISSING_DF[,1])){
		for (s in 1:length(MSPC)){
			cat('"US"',',"',PT_MISSING_DF$region_cd[l],'","',PT_MISSING_DF$tribal_code[l],'","',PT_MISSING_DF$facility_id[l],'","',PT_MISSING_DF$unit_id[l],'","',PT_MISSING_DF$rel_point_id[l]
		,'","',PT_MISSING_DF$process_id[l],'","',PT_MISSING_DF$agy_facility_id[l],'","',PT_MISSING_DF$agy_unit_id[l],'","',PT_MISSING_DF$agy_rel_point_id[l],'","'
		,PT_MISSING_DF$agy_process_id[l],'","',PT_MISSING_DF$scc[l],'","',MSPC_CODE[s],'",',PT_MISSING_DF[l,MSPC[s]],',"',PT_MISSING_DF$ann_pct_red[l],'","',PT_MISSING_DF$facility_name[l],
		'","',PT_MISSING_DF$erptype[l],'",',PT_MISSING_DF$stkhgt[l],',',PT_MISSING_DF$stkdiam[l],',',PT_MISSING_DF$stktemp[l],',',PT_MISSING_DF$stkflow[l],',',
		PT_MISSING_DF$stkvel[l],',"',PT_MISSING_DF$naics[l],'",',PT_MISSING_DF$longitude[l],',',PT_MISSING_DF$latitude[l],
		',,,,,,,,,,,,,,,,,,,,"2011","20210828",,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,"LAST"',sep="")
			cat('\n')
		}
	}
	sink()
}
#########################          #########################
######################### Functions  #########################

