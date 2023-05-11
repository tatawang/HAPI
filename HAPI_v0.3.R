#setwd("/Users/chi-tsanwang/Documents/NIEHS_SBTEX_EMISSION/EMIS_REPORT_COUNTY_SCC/")
setwd("/Users/ctw/OneDrive/NIEHS_SBTEX_EMISSION/EMIS_REPORT_COUNTY_SCC/")




setwd("/Users/ctw/Downloads/smoke_report_2014/")


read.csv("rep_np_oilgas_2014fd_inv_county_scc.csv",
head=TRUE,skip=7 ,sep=",")->np_oilgas
read.csv("rep_np_oilgas_2015fd_inv_county_scc.csv",
head=TRUE,skip=0 ,sep=",")->np_oilgas



read.table("rep_cmv_c1c2_2014fd_inv_county_scc.txt",
head=TRUE,,skip=6 ,sep="|",colClasses=c("Region"="character"))->cmvc1c2

txtpath = "/Users/ctw/Downloads/smoke_report_2014/"
txtfilesn = list.files( path = txtpath, pattern="*rep_nonpt*")
nonpt=list()
for(i in 1:length(txtfilesn)){
	nonpt = rbind(nonpt,read.csv(txtfilesn[i],head=TRUE,skip=7,sep=","))
}



######################### ptnonipm for 2014
read.csv("rep_ptnonipm_2015fd_inv_county_scc.csv",head=TRUE,sep=",")->ptnonipm
read.csv("ptnonipm_2014NEIv2_POINT_20171103_final_comment_newline_fix_02jan2018_nf_v3.csv", head=TRUE, skip=25 ,sep=",")->INV_ptnonipm
read.csv("ptnonipm_2015v2_POINT_20180131_02feb2018_v0.csv", head=TRUE, skip=17 ,sep=",")->INV_ptnonipm


######################### pt_oilgas for 2014
read.csv("rep_pt_oilgas_2015fd_inv_county_scc.csv",skip=0,head=TRUE,sep=",")->ptoilgas

read.csv("oilgas_2014NEIv2_POINT_20171103_final_13nov2017_v0.csv", head=TRUE ,skip=17,sep=",")->INV_ptoilgas1
read.csv("offshore_oilgas_2014NEIv2_POINT_20171103_final_13nov2017_v0.csv", head=TRUE, skip=17 ,sep=",")->INV_ptoilgas2

read.csv("pt_oilgas_2015v2_POINT_20180131_calcyear2015_14feb2018_v0.csv", head=TRUE ,skip=18,sep=",")->INV_ptoilgas1
read.csv("2015fd_from_pt_oilgas_2015v2_POINT_20180131_calcyear2014_14feb2018_v0.csv", head=TRUE, skip=30 ,sep=",")->INV_ptoilgas2
read.csv("offshore_oilgas_2015v2_POINT_20180131_02feb2018_v0.csv", head=TRUE, skip=17 ,sep=",")->INV_ptoilgas3

INV_ptoilgas <- rbind(INV_ptoilgas1,INV_ptoilgas2,INV_ptoilgas3  )

######################### pt_egu for 2014
read.csv("rep_ptegu_2015fd_inv_county_scc.csv",skip=0,head=TRUE,sep=",")->ptegu

read.csv("ptegu_2014NEIv2_POINT_20171103_final_21dec2017_nf_v2.csv", head=TRUE ,skip=39,sep=",")->INV_ptegu


########################  read the inventory report  #################################

################ for nonpt only can read multiple csv file one time ###################
txtpath = "/Users/chi-tsanwang/Documents/NIEHS_SBTEX_EMISSION/EMIS_REPORT_COUNTY_SCC/"
txtfilesn = list.files( path = txtpath, pattern="*rep_nonpt*")

nonpt=list()
for(i in 1:length(txtfilesn)){
	nonpt = rbind(nonpt,read.csv(txtfilesn[i],head=TRUE,sep=","))
}

################ for nonroad only can read multiple csv file one time ###################
txtpath = "/Users/chi-tsanwang/Documents/NIEHS_SBTEX_EMISSION/EMIS_REPORT_COUNTY_SCC/"
txtfilesn = list.files( path = txtpath, pattern="*rep_nonroad*")

nonroad=list()
for(i in 1:length(txtfilesn)){
	nonroad = rbind(nonroad,read.csv(txtfilesn[i],head=TRUE,sep=","))
}

################ for onroad only can read multiple csv file one time ###################






read.csv("rep_nonroad_may_2011ek_NIEHS_inv_county_scc.txt", head=TRUE ,sep=",",colClasses=c("Region"="character"))->nonroad

read.csv("rep_nonroad_apr_2011ek_NIEHS_inv_county_scc.txt", head=TRUE ,sep=",",colClasses=c("Region"="character"))->nonroad




############################. other initial inv report data.   ###############################

read.csv("rep_ptegu_2011ek_NIEHS_inv_county_scc.txt", head=TRUE ,sep=",",colClasses=c("Region"="character"))->ptegu
read.csv("rep_pt_oilgas_2011ek_NIEHS_inv_county_scc.txt", head=TRUE ,sep=",",colClasses=c("Region"="character"))->pt_oilgas
read.csv("rep_ptnonipm_2011ek_NIEHS_inv_county_scc.txt", head=TRUE ,sep=",",colClasses=c("Region"="character"))->ptnonipm
read.csv("rep_np_oilgas_2011ek_NIEHS_inv_county_scc.txt",
head=TRUE ,sep=",",colClasses=c("Region"="character","SCC"="character"))->np_oilgas

#######  other sectors
read.csv("rep_cmv_2011ek_NIEHS_inv_county_scc.csv", head=TRUE ,sep=",",colClasses=c("Region"="character"))->cmv
read.csv("rep_rwc_2011ek_NIEHS_inv_county_scc.txt", head=TRUE ,sep=",",colClasses=c("Region"="character"))->rwc
read.csv("rep_rail_2011ek_NIEHS_inv_county_scc.txt", head=TRUE ,sep=",",colClasses=c("Region"="character"))->rail
#################################



#################  adjusted inv report data (after adding the missing data) ##################

######.  adjusted ptnonipm.  ##########
read.csv("rep_ptnonipm_2011ek_NIEHS_inv_county_scc_adj.txt", head=TRUE ,sep=",",
colClasses=c("Region"="character"))->ptnonipm

######.  adjusted np_oilgas  ##########
read.csv("rep_np_oilgas_2011ek_NIEHS_inv_county_scc_adj.txt", head=TRUE ,sep=",",colClasses=c("Region"="character"))->np_oilgas

######.  adjusted pt_oilgas  ##########
read.csv("rep_pt_oilgas_2011ek_NIEHS_inv_county_scc_adj.txt", head=TRUE ,sep=",",colClasses=c("Region"="character"))->pt_oilgas


######.  adjusted ptegu.     ##########

######################### Read SMOKE INV input data for point source#########################
######################### Only Point Source needed
######################### ptnonipm

read.csv("ptnonipm_2011NEIv2_POINT_18dec2015_v10.csv", head=TRUE ,sep=",",colClasses=c("region_cd"="character"))->INV_ptnonipm0
read.csv("refueling_2011NEIv2_POINT_04dec2014_v2.csv", head=TRUE ,sep=",",colClasses=c("region_cd"="character"))->INV_ptnonipm1
read.csv("ethanol_plants_2011NEIv2_POINT_03feb2015_v1.csv", head=TRUE ,sep=",",colClasses=c("region_cd"="character"))->INV_ptnonipm2
INV_ptnonipm <- rbind(INV_ptnonipm0,INV_ptnonipm1,INV_ptnonipm2 )

######################### pt_oilgas

read.csv("pt_oilgas_2011NEIv2_POINT_03feb2015_v4.csv", head=TRUE ,sep=",",colClasses=c("region_cd"="character"))->INV_pt_oilgas

######################### ptegu

read.csv("2011NEIv2_POINT_ptegu_2011ei_15dec2015_v6.csv", head=TRUE ,sep=",",colClasses=c("region_cd"="character"))->INV_ptegu




######################### Functions  #########################
#########################  generate the missing emission table by SCC

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
		No_STEX_DESC <- rbind(No_STEX_DESC, No_STEX[which(No_STEX$SCC == SCC_noSTEX[x])[1],c("SCC","SCC.Description","I.VOC_INV")])
	}
	write.csv(No_STEX_DESC, "nonpt_noSTEX.csv")
	W_STEX_DESC <- list()
	for (x in 1:length(SCC_wSTEX)){
		W_STEX_DESC <- rbind(W_STEX_DESC, W_STEX[which(W_STEX$SCC == SCC_wSTEX[x])[1],c("SCC","SCC.Description","I.VOC_INV")])
	}
	write.csv(W_STEX_DESC, "nonpt_wSTEX.csv")
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
#########################          #########################
######################### Process 1 data and setup        #########################

Array <- array(NA, dim = c(6,5))
SPC=c("I.TOLUENE","I.XYLENES","I.STYRENE","I.ETHYLBENZ","ACROLEIN","BUTADIE","HEXANE")
MSPC=c("M.TOLUENE","M.XYLENES","M.STYRENE","M.ETHYLBENZ","M.ACROLEIN","M.BUTADIENE","M.HEXANE")
MSPC_CODE <- c("108883","95476","100425","100414","107028","106990","110543")

#Datalist <- array(nonpt, ptegu, pt_oilgas, ptnonipm, np_oilgas)
Datanamelist <- c("nonpt","ptegu","pt_oilgas","ptnonipm","np_oilgas")

State_List1 = c("Alabama             ","Georgia             ","Mississippi         ","Florida             ","Louisiana           ","Texas               ")
#State_List2 = c("Alabama       ","Georgia       ","Mississippi   ","Florida       ","Louisiana     ","Texas         ")

State_List2 = c(" Alabama      "," Georgia      "," Mississippi  "," Florida      "," Louisiana    "," Texas        ")

State_List3 = c(" Alabama             "," Georgia             "," Mississippi         "," Florida             "," Louisiana           "," Texas               ")
State_List4 = c(" Alabama        "," Georgia        "," Mississippi    "," Florida        "," Louisiana      "," Texas          ")
State_List5 = c(" Alabama       "," Georgia       "," Mississippi   "," Florida       "," Louisiana     "," Texas         ")

#nonpt[which(nonpt$State %in% State_List1),] -> nonpt_6ST
ptegu[which(ptegu$State %in% State_List1),] -> ptegu_6ST
pt_oilgas[which(pt_oilgas$State %in% State_List2),] -> pt_oilgas_6ST
ptnonipm[which(ptnonipm$State %in% State_List1),] -> ptnonipm_6ST
np_oilgas[which(np_oilgas$State %in% State_List2),] -> np_oilgas_6ST


###2014##############################################################################
cmvc1c2[which(cmvc1c2$State %in% State_List3),] -> cmvc1c2_6ST
nonpt[which(nonpt$State %in% State_List3),] -> nonpt_6ST
ptnonipm[which(ptnonipm$State %in% State_List3),] -> ptnonipm_6ST
ptoilgas[which(ptoilgas$State %in% State_List4),] -> ptoilgas_6ST
ptegu[which(ptegu$State %in% State_List5),] -> ptegu_6ST
np_oilgas[which(np_oilgas$State %in% State_List2),] -> np_oilgas_6ST



####################################################################################
np_oilgas_adj[which(np_oilgas_adj$State %in% State_List1),] -> np_oilgas_adj_6ST
ptnonipm_adj[which(ptnonipm_adj$State %in% State_List1),] -> ptnonipm_6ST_adj
pt_oilgas_adj[which(pt_oilgas_adj$State %in% State_List2),] -> pt_oilgas_6ST_adj



rwc[which(rwc$State %in% State_List1),] -> rwc_6ST
cmv[which(cmv$State %in% State_List1),] -> cmv_6ST
rail[which(rail$State %in% State_List1),] -> rail_6ST
nonroad[which(nonroad$State %in% State_List1),] -> nonroad_6ST
agfire[which(agfire$State %in% State_List1),] -> agfire_6ST
######################### Process 2A calculation for missing   #########################

sector = np_oilgas_6ST
sector_mkup =np_oilgas

sector = nonpt_6ST
sector_mkup = nonpt


sector = np_oilgas_adj_6ST
sector = ptnonipm_6ST_adj
sector = ptnonipm_6ST
sector = pt_oilgas_6ST
sector = pt_oilgas_6ST_adj

sector = rwc_6ST
sector = cmv_6ST
sector = rail_6ST
sector = nonroad_6ST
sector = agfire_6ST
sector =cmvc1c2_6ST

sector = nonpt_6ST
sector_mkup = nonpt

No_STEX_misSCC <- Missing_EM_LIST(sector)
MISSDATA <- array(NA, dim = c(length(No_STEX_misSCC[,1]),length(SPC)))
for (j in 1:length(SPC)){
		out  <- Missing_EM_SCC(sector, sector_mkup ,SPC[j])
		print(sum(out[,5]))
		for (n in 1:length(No_STEX_misSCC[,1])){
			RO = out[toString(No_STEX_misSCC[n,5]),3]
				MISSDATA[n,j] = No_STEX_misSCC[n,"I.VOC_INV"]*RO
		}
}

colnames(MISSDATA) <- MSPC
MISSING_DF <- cbind(No_STEX_misSCC, MISSDATA)

#MU_INV<- INV_ptnonipm


tapply((MISSING_DF$M.TOLUENE+MISSING_DF$M.XYLENES+MISSING_DF$M.STYRENE+MISSING_DF$M.ACROLEIN+MISSING_DF$M.BUTADIENE + MISSING_DF$M.ETHYLBENZ),MISSING_DF$State, sum)

STLIST = list()
for(s in 1:length(MSPC)){
	STLIST <- cbind(STLIST,tapply(MISSING_DF[,MSPC[s]], MISSING_DF$State, sum))
}

colnames(STLIST) <- MSPC
options(scipen = 999) 
name <- "np_oilgas_2015NEI_STEXABH_v2.csv"
write_missing_np_INV(name, MISSING_DF, No_STEX_misSCC, MSPC, MSPC_CODE )

######################### Process 2B calculation for missing   #########################


INV_sector <- INV_ptoilgas
INV_sector <- INV_ptegu


sector <- ptoilgas_6ST
sector_mkup <- ptoilgas

sector <- ptegu_6ST
sector_mkup <- ptegu

sector = ptnonipm_6ST
sector_mkup = ptnonipm
INV_sector <- INV_ptnonipm

name <- "pt_oilgas_2011NEIv2_STEXAB_missing_v2.csv"
name <- "ptegu_2011NEIv2_STEXAB_missing_v2.csv"


name <- "ptnonipm_STEXAB_missing_2015v2.csv"
name <- "ptoilgas_STEXAB_missing_2015v2.csv"
name <- "ptegu_STEXAB_missing_2015v2.csv"

INV_mis =list()
No_STEX_misSCC <- Missing_EM_LIST(sector)
for (x in 1:length(No_STEX_misSCC[,1])){
	INV_mis <- rbind(INV_mis, INV_sector[which(INV_sector$region_cd == No_STEX_misSCC$Region[x] & INV_sector$scc == No_STEX_misSCC$SCC[x]),])
}
INV_VOC_mis <-INV_mis[which(INV_mis$poll == "VOC"),]


PT_MISSDATA <- array(NA, dim = c(length(INV_VOC_mis[,1]),length(SPC)))
for (j in 1:length(SPC)){
		out  <- Missing_EM_SCC(sector,sector_mkup , SPC[j])
		print(sum(out[,5]))
#		ratiotable <- cbind(ratiotable,out[,3])
		for (n in 1:length(INV_VOC_mis[,1])){
			PT_MISSDATA[n,j] <- INV_VOC_mis[n,"ann_value"] * out[toString(INV_VOC_mis[n,"scc"]),3]
		}
}
colnames(PT_MISSDATA) <- MSPC
PT_MISSING_DF <- cbind(INV_VOC_mis, PT_MISSDATA)


options(scipen = 999) 
write_missing_pt_INV(name,PT_MISSING_DF, No_STEX_misSCC, MSPC, MSPC_CODE)





















