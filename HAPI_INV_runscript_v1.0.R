setwd("/Users/tatawang/HAPI_v1/")
state_list <- read.csv("/Users/tatawang/HAPI_v1/US_state_FIPS.csv")
source("/Users/tatawang/HAPI_v1/HAPI_functions_v1.R")

secs_list <- list("pt_oilgas","np_oilgas","nonpt","ptegu","np_solvents")
pt_list <- list("Y","N","N","Y","N")
year_list <- list(2008)

SPC=c("I.TOLUENE","I.XYLENES","I.STYRENE","I.ETHYLBENZ","ACROLEIN","BUTADIE") # for final report, no VOC, only HAPS
repSPC=c("I.TOLUENE","I.XYLENES","I.STYRENE","I.ETHYLBENZ","ACROLEIN","BUTADIE", "I.VOC_INV") # for county report level usage
MSPC=c("M.TOLUENE","M.XYLENES","M.STYRENE","M.ETHYLBENZ","M.ACROLEIN","M.BUTADIENE")# report missing HAPs
MSPC_CODE <- c("108883","95476","100425","100414","107028","106990", "VOC")# CAS number and species for grap species in inventory 
###select target state
state_num_list <- c(24,36,39,42,51,54) #state_fips code list
state_name_list <- c("Ohio","West.Virginia","Pennsylvania","New.York","Virginia","Maryland")

############### only adjust before this part#######
for(y in 1: length(year_list)){
	for(s in 1: length(secs_list)){

sec <- secs_list[s]
year <- year_list[y]
pt <- pt_list[s]

sec = "ptnonipm"
year <- 2008
pt= "Y"
y = 1
s = 1

print(paste(year,sec))
pathf <- paste('/Users/tatawang/HAPI/', year_list[y],'EQT/',sec,'/',sep="")
invdat = load_files(pathf)
INV_input <- invdat 
INV_input$state <- floor_dec(INV_input$region_cd/1000)

output <- make_rep_county_scc(INV_input, state_num_list, MSPC_CODE, repSPC)
write.csv(output, paste(year,sec,"_t.csv",sep=""),row.names=FALSE)

read.csv(paste(year,sec,"_t.csv",sep="")) -> ct_scc_input
ct_scc_input[is.na(ct_scc_input)] <-0
ct_scc_input_a <- add_state_lab(ct_scc_input,state_list)

state_sum_org <- gen_state_sum(ct_scc_input_a, SPC, state_name_list)# summary report table for org

output <- make_rep_county_scc_mkup(INV_input, state_list, MSPC_CODE, repSPC) 
write.csv(output, paste(year,sec,"_mk.csv",sep=""),row.names=FALSE)

read.csv(paste(year,sec,"_mk.csv",sep="")) -> ct_scc_input_mkup 
ct_scc_input_mkup[is.na(ct_scc_input_mkup)] <-0

sector_mkup = ct_scc_input_mkup
sector = ct_scc_input
INV_sector = invdat

############################
if (pt=="N"){
	No_STEX_misSCC <- Missing_EM_LIST(sector)
	if (length(No_STEX_misSCC[,1]>0)){
	MISSDATA <- array(NA, dim = c(length(No_STEX_misSCC[,1]),length(SPC)))
	for (j in 1:length(SPC)){
			out  <- Missing_EM_SCC(sector, sector_mkup ,SPC[j])
			print(sum(out[,5]))
			for (n in 1:length(No_STEX_misSCC[,1])){
				RO = out[toString(No_STEX_misSCC[n,'SCC']),3]
					MISSDATA[n,j] = No_STEX_misSCC[n,"I.VOC_INV"]*RO
			}
	}
	colnames(MISSDATA) <- MSPC
	MISSING_DF <- cbind(No_STEX_misSCC, MISSDATA)
	#MU_INV<- INV_ptnonipm	
	options(scipen = 999) 
	name <- paste(sec,year,"_v1_STEXAB_missing.csv", sep="")
	write_missing_np_INV(name, MISSING_DF, No_STEX_misSCC, MSPC, MSPC_CODE )
	MISSING_DF$state <- floor_dec(MISSING_DF$Region/1000)
	MISSING_DF_a <-add_state_lab(MISSING_DF, state_list)
	state_sum_miss <- gen_state_sum(MISSING_DF_a, MSPC, state_name_list) 
	QA_report<-cbind(state_sum_org,state_sum_miss)
	} else{
		state_sum_miss = array(0, dim = c(length(state_num_list),length(SPC)))
		QA_report<-cbind(state_sum_org,state_sum_miss)
	}
	write.csv(QA_report, paste("QA_report_v1",year,sec,".csv", sep=""))
} else if (pt == "Y"){
	#########################
	INV_mis =list()
	No_STEX_misSCC <- Missing_EM_LIST(sector)
	if (length(No_STEX_misSCC[,1]>0)){
	for (x in 1:length(No_STEX_misSCC[,1])){
		INV_mis <- rbind(INV_mis, INV_sector[which(INV_sector$region_cd == No_STEX_misSCC$Region[x] & INV_sector$scc == 	No_STEX_misSCC$SCC[x]),])
	}
	INV_VOC_mis <-INV_mis[which(INV_mis$poll == "VOC"),]
	#STEXAB <- c("I.TOLUENE","I.XYLENES","I.STYRENE","I.ETHYLBENZ","ACROLEIN","BUTADIE")
	PT_MISSDATA <- array(NA, dim = c(length(INV_VOC_mis[,1]),length(SPC)))
	for (j in 1:(length(SPC))){
			out  <- Missing_EM_SCC(sector,sector_mkup , SPC[j])
			print(sum(out[,5]))
	#		ratiotable <- cbind(ratiotable,out[,3])
			for (n in 1:length(INV_VOC_mis[,1])){
				PT_MISSDATA[n,j] <- INV_VOC_mis[n,"ann_value"] * out[toString(INV_VOC_mis[n,"scc"]),3]
			}
	}
	colnames(PT_MISSDATA) <- MSPC
	PT_MISSING_DF <- cbind(INV_VOC_mis, PT_MISSDATA)
	name <- paste(sec,year,"_v1_STEXAB_missing.csv", sep="")
	options(scipen = 999) 
	write_missing_pt_INV(name,PT_MISSING_DF, No_STEX_misSCC, MSPC, MSPC_CODE)

	PT_MISSING_DF$state <- floor_dec(PT_MISSING_DF$region_cd/1000)
	PT_MISSING_DF_a <-add_state_lab(PT_MISSING_DF, state_list)
	state_sum_miss <- gen_state_sum(PT_MISSING_DF_a, MSPC, state_name_list) 
	QA_report<-cbind(state_sum_org,state_sum_miss)
	} else{
		state_sum_miss = array(0, dim = c(length(state_num_list),length(SPC)))
		QA_report<-cbind(state_sum_org,state_sum_miss)	
	}
	write.csv(QA_report, paste("QA_report_v1",year,sec,".csv", sep=""))
}
	#########################
}
}