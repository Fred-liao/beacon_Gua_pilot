##Script for generating tables and figures for Liao et al., JESEE 2019 paper
library(data.table)
load('beacon_mother_comparison.RData')


####Figures####
#Figure 2.
#mother
mean(PEO_daily3[,total_time])
mean(PEO_daily3[,Kitchen_time2])
mean(PEO_daily3[,SAP_time2])
mean(PEO_daily3[,HOP_time2])
mean(PEO_daily3[,Outside_time2])

mean(PEO_daily3[phase == "01"|phase == "02",total_time])
mean(PEO_daily3[phase == "01"|phase == "02",Kitchen_time2])
mean(PEO_daily3[phase == "01"|phase == "02",SAP_time2])
mean(PEO_daily3[phase == "01"|phase == "02",HOP_time2])
mean(PEO_daily3[phase == "01"|phase == "02",Outside_time2])

mean(PEO_daily3[phase == "03"|phase == "04",total_time])
mean(PEO_daily3[phase == "03"|phase == "04",Kitchen_time2])
mean(PEO_daily3[phase == "03"|phase == "04",SAP_time2])
mean(PEO_daily3[phase == "03"|phase == "04",HOP_time2])
mean(PEO_daily3[phase == "03"|phase == "04",Outside_time2])

#Children
mean(Children_PM3[phase == "01"|phase == "02"]$Total_time,na.rm = T)
mean(Children_PM3[phase == "01"|phase == "02"]$HOP_time)
mean(Children_PM3[phase == "01"|phase == "02"]$PEO_time)
mean(Children_PM3[phase == "01"|phase == "02"]$KAP_time)
mean(Children_PM3[phase == "01"]$SAP_time)
mean(Children_PM3[phase == "01"|phase == "02"]$Out_time)

mean(Children_PM3[phase == "03"|phase == "04"]$Total_time,na.rm = T)
mean(Children_PM3[phase == "03"|phase == "04"]$HOP_time)
mean(Children_PM3[phase == "03"|phase == "04"]$PEO_time)
mean(Children_PM3[phase == "03"|phase == "04"]$KAP_time)
mean(Children_PM3[phase == "03"|phase == "04"]$SAP_time)
mean(Children_PM3[phase == "03"|phase == "04"]$Out_time)


mean(Children_PM3$Total_time,na.rm = T);sd(Children_PM3$Total_time,na.rm = T)
mean(Children_PM3$HOP_time);sd(Children_PM3$HOP_time)
mean(Children_PM3$PEO_time);sd(Children_PM3$PEO_time)
mean(Children_PM3$KAP_time);sd(Children_PM3$KAP_time)
mean(Children_PM3$SAP_time);sd(Children_PM3$SAP_time)
mean(Children_PM3$Out_time);sd(Children_PM3$Out_time)



##Figure 3

PEO_daily3[,KAP_IE := Kitchen_indirect_PM2*Kitchen_time2/total_time]
PEO_daily3[,SAP_IE := SAP_indirect_PM2*SAP_time2/total_time]
PEO_daily3[,HOP_IE := HOP_indirect_PM2*HOP_time2/total_time]
PEO_daily3[,Total_IE :=(KAP_IE+SAP_IE+HOP_IE)]

mean(PEO_daily3[phase == "01"|phase == "02",KAP_IE],na.rm = T)
mean(PEO_daily3[phase == "01"|phase == "02",SAP_IE],na.rm = T)
mean(PEO_daily3[phase == "01"|phase == "02",HOP_IE],na.rm = T)
mean(PEO_daily3[phase == "01"|phase == "02",Total_IE],na.rm = T)

mean(PEO_daily3[phase == "03"|phase == "04",KAP_IE],na.rm = T)
mean(PEO_daily3[phase == "03"|phase == "04",SAP_IE],na.rm = T)
mean(PEO_daily3[phase == "03"|phase == "04",HOP_IE],na.rm = T)
mean(PEO_daily3[phase == "03"|phase == "04",Total_IE],na.rm = T)

###Figure 4
PEO_daily3[,ave_d_in := (PM_direct2 + PM_indirect2)/2]
PEO_daily3[,diff_d_in := PM_direct2 - PM_indirect2]
PEO_daily3[,ave_d_k := (PM_direct2 + PM_kitchen2)/2]
PEO_daily3[,diff_d_k := PM_direct2 - PM_kitchen2]

p1 = ggplot(PEO_daily3, aes(x = ave_d_in, y = diff_d_in)) +
      geom_point(alpha = 0.7, size = 0.7) +
      geom_hline(yintercept = mean(PEO_daily3$diff_d_in), colour = "blue", size = 0.5) +
      geom_hline(yintercept = mean(PEO_daily3$diff_d_in) - (1.96 * sd(PEO_daily3$diff_d_in)), colour = "red", size = 0.5) +
      geom_hline(yintercept = mean(PEO_daily3$diff_d_in) + (1.96 * sd(PEO_daily3$diff_d_in)), colour = "red", size = 0.5) +
      ylab(expression(paste("Diff. Between Direct and Indirect Measure (", mu, "g/m" ^3,")", sep = ""))) +
      scale_color_discrete(labels  = c("BL1","BL2","FU1","FU2"), name = "Phase")+
      scale_y_continuous(limits = c(-1200,400), breaks = c(-1200,-1000,-800,-600,-400,-200,0,200,400))+
      scale_x_continuous(limits = c(0,1000), breaks = c(0,200,400,600,800,1000))+
      xlab(expression(paste("Average of Direct and Indirect Measure (", mu, "g/m" ^3,")", sep = ""))) +
      #ggtitle("A:  Bland-Altman plot between direct and indirect measure")+
      theme_bw()

p2 = ggplot(PEO_daily3, aes(x = ave_d_k, y = diff_d_k)) +
      geom_point(alpha = 0.7, size = 0.7) +
      geom_hline(yintercept = mean(PEO_daily3$diff_d_k,na.rm = T), colour = "blue", size = 0.5) +
      geom_hline(yintercept = mean(PEO_daily3$diff_d_k,na.rm = T) - (1.96 * sd(PEO_daily3$diff_d_k,na.rm = T)), colour = "red", size = 0.5) +
      geom_hline(yintercept = mean(PEO_daily3$diff_d_k,na.rm = T) + (1.96 * sd(PEO_daily3$diff_d_k,na.rm = T)), colour = "red", size = 0.5) +
      ylab(expression(paste("Diff. Between Direct and Kitchen Measure (", mu, "g/m" ^3,")", sep = ""))) +
      scale_color_discrete(labels  = c("BL1","BL2","FU1","FU2"), name = "Phase")+
      scale_y_continuous(limits = c(-1200,400), breaks = c(-1200,-1000,-800,-600,-400,-200,0,200,400,400))+
      scale_x_continuous(limits = c(0,1000), breaks = c(0,200,400,600,800,1000))+
      xlab(expression(paste("Average of Direct and Kitchen Measure (", mu, "g/m" ^3,")", sep = ""))) +
      #ggtitle("B:  Bland-Altman plot between direct and kitchen area measure")+
      theme_bw()
ggarrange(p1, p2, ncol = 2)



###Figure 5
Children_PM3[is.na(KAP_PM),KAP_PM := 0]
Children_PM3[is.na(SAP_PM),SAP_PM := 0]
Children_PM3[is.na(HOP_PM),HOP_PM := 0]
Children_PM3[is.na(PEO_PM),PEO_PM := 0]


Children_PM3[,KAP_IE := KAP_PM*KAP_time/Total_time]
Children_PM3[,SAP_IE := SAP_PM*SAP_time/Total_time]
Children_PM3[,HOP_IE := HOP_PM*HOP_time/Total_time]
Children_PM3[,PEO_IE := PEO_PM*PEO_time/Total_time]


mean(Children_PM3[phase == "01"|phase == "02",KAP_IE],na.rm = T)
mean(Children_PM3[phase == "01"|phase == "02",SAP_IE],na.rm = T)
mean(Children_PM3[phase == "01"|phase == "02",HOP_IE],na.rm = T)
mean(Children_PM3[phase == "01"|phase == "02",PEO_IE],na.rm = T)


Children_PM3[,Total_IE :=(KAP_IE+SAP_IE+HOP_IE)]
mean(Children_PM3[phase == "01"|phase == "02",Total_IE],na.rm = T)

mean(Children_PM3[phase == "03"|phase == "04",KAP_IE],na.rm = T)
mean(Children_PM3[phase == "03"|phase == "04",SAP_IE],na.rm = T)
mean(Children_PM3[phase == "03"|phase == "04",HOP_IE],na.rm = T)
mean(Children_PM3[phase == "03"|phase == "04",PEO_IE],na.rm = T)


####Tables####
#Table 1
PM_summary2

##Table 2
#direct
mean(PEO_daily3[phase == "01"|phase == "02"]$PM_direct2, na.rm = T );sd(PEO_daily3[phase == "01"|phase == "02"]$PM_direct2, na.rm = T )
median(PEO_daily3[phase == "01"|phase == "02"]$PM_direct2, na.rm = T );IQR(PEO_daily3[phase == "01"|phase == "02"]$PM_direct2, na.rm = T )

mean(PEO_daily3[phase == "03"|phase == "04"]$PM_direct2, na.rm = T );sd(PEO_daily3[phase == "03"|phase == "04"]$PM_direct2, na.rm = T )
median(PEO_daily3[phase == "03"|phase == "04"]$PM_direct2, na.rm = T );IQR(PEO_daily3[phase == "03"|phase == "04"]$PM_direct2, na.rm = T )

mean(PEO_daily3$PM_direct2, na.rm = T );sd(PEO_daily3$PM_direct2, na.rm = T )
median(PEO_daily3$PM_direct2, na.rm = T );IQR(PEO_daily3$PM_direct2, na.rm = T )

#indirect
mean(PEO_daily3[phase == "01"|phase == "02"]$PM_indirect2, na.rm = T );sd(PEO_daily3[phase == "01"|phase == "02"]$PM_indirect2, na.rm = T )
median(PEO_daily3[phase == "01"|phase == "02"]$PM_indirect2, na.rm = T );IQR(PEO_daily3[phase == "01"|phase == "02"]$PM_indirect2, na.rm = T )

mean(PEO_daily3[phase == "03"|phase == "04"]$PM_indirect2, na.rm = T );sd(PEO_daily3[phase == "03"|phase == "04"]$PM_indirect2, na.rm = T )
median(PEO_daily3[phase == "03"|phase == "04"]$PM_indirect2, na.rm = T );IQR(PEO_daily3[phase == "03"|phase == "04"]$PM_indirect2, na.rm = T )

mean(PEO_daily3$PM_indirect2, na.rm = T );sd(PEO_daily3$PM_indirect2, na.rm = T )
median(PEO_daily3$PM_indirect2, na.rm = T );IQR(PEO_daily3$PM_indirect2, na.rm = T )
#correlation

cor(PEO_daily3[phase == "01"|phase == "02"]$PM_direct2, PEO_daily3[phase == "01"|phase == "02"]$PM_indirect2, 
    use = "pairwise.complete.obs", method = "spearman")
cor(PEO_daily3[phase == "03"|phase == "04"]$PM_direct2, PEO_daily3[phase == "03"|phase == "04"]$PM_indirect2, 
    use = "pairwise.complete.obs", method = "spearman")
cor(PEO_daily3$PM_direct2, PEO_daily3$PM_kitchen2,
    use = "pairwise.complete.obs", method = "spearman")

##Table 3
RMSE = function(m, o){
      sqrt(mean((m - o)^2))
}
RMSE(PEO_daily3$PM_indirect2,PEO_daily3$PM_direct2)
RMSE(PEO_daily3[phase == "01"|phase == "02"]$PM_indirect2,PEO_daily3[phase == "01"|phase == "02"]$PM_direct2)
RMSE(PEO_daily3[phase == "03"|phase == "04"]$PM_indirect2,PEO_daily3[phase == "03"|phase == "04"]$PM_direct2)

RMSE(PEO_daily3[!is.na(PM_kitchen2)]$PM_kitchen2,PEO_daily3[!is.na(PM_kitchen2)]$PM_direct2)
RMSE(PEO_daily3[!is.na(PM_kitchen2)&(phase == "01"|phase == "02")]$PM_kitchen2,
     PEO_daily3[!is.na(PM_kitchen2)&(phase == "01"|phase == "02")]$PM_direct2)
RMSE(PEO_daily3[!is.na(PM_kitchen2)&(phase == "03"|phase == "04")]$PM_kitchen2,
     PEO_daily3[!is.na(PM_kitchen2)&(phase == "03"|phase == "04")]$PM_direct2)

#bias
mean(PEO_daily3$PM_direct2-PEO_daily3$PM_indirect2)
mean(PEO_daily3[phase == "01"|phase == "02"]$PM_direct2-PEO_daily3[phase == "01"|phase == "02"]$PM_indirect2)
mean(PEO_daily3[phase == "03"|phase == "04"]$PM_direct2-PEO_daily3[phase == "03"|phase == "04"]$PM_indirect2)

mean(PEO_daily3$PM_direct2-PEO_daily3$PM_kitchen2, na.rm = T)
mean(PEO_daily3[phase == "01"|phase == "02"]$PM_direct2-PEO_daily3[phase == "01"|phase == "02"]$PM_kitchen2, na.rm = T)
mean(PEO_daily3[phase == "03"|phase == "04"]$PM_direct2-PEO_daily3[phase == "03"|phase == "04"]$PM_kitchen2, na.rm = T)


##Table 4
mean(Children_PM3[phase=="01"|phase =="02"]$PEC_indirect_PM, na.rm = T);sd(Children_PM3[phase=="01"|phase =="02"]$PEC_indirect_PM, na.rm = T)
median(Children_PM3[phase=="01"|phase =="02"]$PEC_indirect_PM, na.rm = T);IQR(Children_PM3[phase=="01"|phase =="02"]$PEC_indirect_PM, na.rm = T)

mean(Children_PM3[phase=="03"|phase =="04"]$PEC_indirect_PM, na.rm = T);sd(Children_PM3[phase=="03"|phase =="04"]$PEC_indirect_PM, na.rm = T)
median(Children_PM3[phase=="03"|phase =="04"]$PEC_indirect_PM, na.rm = T);IQR(Children_PM3[phase=="03"|phase =="04"]$PEC_indirect_PM, na.rm = T)

mean(Children_PM3$PEC_indirect_PM, na.rm = T);sd(Children_PM3$PEC_indirect_PM, na.rm = T)
median(Children_PM3$PEC_indirect_PM, na.rm = T);IQR(Children_PM3$PEC_indirect_PM, na.rm = T)




