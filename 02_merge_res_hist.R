

in_dir <- "01_cuts"
out_dir <- "01_cuts"

pot_polys <- vect(file.path(in_dir,"all_pot_polys.gpkg"))
pot_polys_dt <- as.data.table(pot_polys)

#exploring the other results layers
in_dir <- "D:/Spatial Data/RESULTS/RESULTS_activities_Jan 1 2024/RSLT_ACTIVITY_TREATMENT_SVW"
res_bc <- vect(file.path(in_dir,"RSLT_ACTRT_polygon.shp"))
res_bc_act <- as.data.table(res_bc)

in_dir <- "D:/Spatial Data/RESULTS/RESULTS_silviculture/RSLT_FOREST_COVER_SILV_SVW"
silv_bc <- vect(file.path(in_dir,"RSLT_FCSLV_polygon.shp"))
res_bc_silv <- as.data.table(silv_bc)

#learn about the data:
silv_layer <- bcdc_describe_feature("258bb088-4113-47b1-b568-ce20bd64e3e3") %>%
  collect()
silv_layer_dt <- data.table(silv_layer)
silv_layer_dt[col_name == "FOREST_COVER_SILV_TYPE"]
silv_sa <- bcdc_query_geodata("258bb088-4113-47b1-b568-ce20bd64e3e3") %>%
  filter(INTERSECTS(tsa_bounds))


in_dir <- "D:/Spatial Data/RESULTS/RESULTS_silviculture_reserve/RSLT_FOREST_COVER_RESERVE_SVW"
silv_rfc_bc <- vect(file.path(in_dir,"RSLT_FCRES_polygon.shp"))
fcr_bc_silv <- as.data.table(silv_rfc_bc)



pot_poly_act <- merge(pot_polys_dt, res_bc_act[,.(OPEN_ID, MAP_LABEL, ACT_BASE, ACT_TECH, ACT_METH,
                                                  START_DT, COMPL_DT, CTLTRTMNTR,
                                                  DISTURB_CD, SYS_CD, SYS_VR_CD, CUT_PH_CD)], 
                      by.x = "OPENING_ID", 
                      by.y = "OPEN_ID", 
                      all.x = TRUE)
pot_poly_act <- unique(pot_poly_act)

pot_act_silv <- merge(pot_poly_act, res_bc_silv[,.(OPEN_ID,  SU_ID, PLY_NO, PLY_AR, PLY_NET_AR,
                                                   STK_ST_CD, STK_TP_CD,RES_CD, RES_OBJ_CD,
                                                   CVR_PAT_CD, REENTRY_YR, REF_YR, SITE_INDEX,
                                                   SI_SRC_CD,BEC_SI_SRS,SIL_IMP_YN, FC_SILV_TP,
                                                   S_TSP_HA, S_TWS_HA, S_WS_HA, S_FG_HA,
                                                   S_CC_PCT, S_BSL_AR, S_SPP1_CD, S_SPP1_PC,
                                                   S_SPP1_AG, S_SPP1_HT,S_SPP2_CD, S_SPP2_PC,
                                                   S_SPP2_AG, S_SPP2_HT, S_SPP3_CD, S_SPP3_PC,
                                                   S_SPP4_CD, S_SPP4_PC, S_SPP5_CD,S_SPP5_PC,
                                                   S_MOR_YN)], 
                      by.x = "OPENING_ID", 
                      by.y = "OPEN_ID", 
                      all.x = TRUE)
pot_act_silv <- unique(pot_act_silv)
#pot_act_silv_fc <- merge(pot_act_silv, fcr_bc_silv[,.(OPEN_ID,PLY_NO, PLY_AR, RES_CD,
 #                                                     RES_OBJ_CD)], 
  #                    by.x = "OPENING_ID", 
   #                   by.y = "OPEN_ID", 
    #                  all.x = TRUE)
#pot_act_silv_fc <- unique(pot_act_silv_fc)

#get rid of the surveys:
pot_act_silv_nsu <- pot_act_silv[ACT_BASE != "SU"|is.na(ACT_BASE)]
                                   
uniq_id <- unique(pot_act_silv$OPENING_ID)
pot_polys_detailed <- pot_polys_dt

for(iii in 1:length(uniq_id)){
  
  if(nrow(pot_act_silv_nsu[OPENING_ID == uniq_id[iii]]) > 1){
    
  }else{
    pot_polys_detailed[OPENING_ID == uniq_id[iii], 
                       `:=`(MAP_LABEL ACT_BASE ACT_TECH ACT_METH)]
  }
  
}

dcast(pot_act_silv, OPENING_ID ~ ., fun.aggregate =  toString)

