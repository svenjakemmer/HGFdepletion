# Theme Systems Biology and Applications

theme_SBA <- function (base_size = 7, base_family = "") 
{
  colors <- list(medium = c(gray = "#737373", red = "#F15A60", 
                            green = "#7AC36A", blue = "#5A9BD4", orange = "#FAA75B", 
                            purple = "#9E67AB", maroon = "#CE7058", magenta = "#D77FB4"), 
                 dark = c(black = "#010202", red = "#EE2E2F", green = "#008C48", 
                          blue = "#185AA9", orange = "#F47D23", purple = "#662C91", 
                          maroon = "#A21D21", magenta = "#B43894"), 
                 light = c(gray = "#CCCCCC", red = "#F2AFAD", green = "#D9E4AA", blue = "#B8D2EC", 
                           orange = "#F3D1B0", purple = "#D5B2D4", maroon = "#DDB9A9", 
                           magenta = "#EBC0DA"))
  gray <- colors$medium["gray"]
  black <- colors$dark["black"]
  theme_bw(base_size = base_size, base_family = base_family) + 
    theme(line = element_line(colour = "black"), 
          rect = element_rect(fill = "white",colour = NA), text = element_text(colour = "black"), 
          axis.text = element_text(size = rel(1), colour = "black"), 
          axis.text.x = element_text(margin = unit(c(2, 4, 2, 4), "mm")), 
          axis.text.y = element_text(margin = unit(c(4, 2, 4, 2), "mm")), 
          axis.title.x = element_text(margin = margin(t = -5, r = 0, b = 0, l = 0)),
          axis.title.y = element_text(margin = margin(t = 0, r = -10, b = 0, l = 0)),
          axis.ticks = element_line(colour = "black"), 
          axis.ticks.length = unit(1, "mm"),
          legend.key = element_rect(colour = NA), 
          panel.border = element_rect(colour = "black"), 
          panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(), 
          panel.background = element_blank(),
          strip.background = element_rect(fill = "white", colour = NA), strip.text = element_text(size = rel(1)))
  
}

dMod_colors <- c("#000000", "#C5000B", "#0084D1", "#579D1C", "#FF950E", "#4B1F6F", "#CC79A7","#006400", "#F0E442", "#8B4513", rep("gray", 100))
SBA_colors <- c("#000000","#FFD942", "#FF950E","#0084D1","#0147A6","#006400", rep("gray", 100))

scale_color_SBA <- function (...) 
{
  scale_color_manual(..., values = SBA_colors)
}

scale_fill_SBA <- function (...) 
{
  scale_fill_manual(..., values = c("transparent","#FFD942", "#FF950E","#0084D1","#0147A6","#006400", rep("gray", 100)))
}

condition_levels <- c( "cetuximab_T47D_TGF",    "cetuximab_T47D_BTC",    "cetuximab_T47D_HRG",   "cetuximab_T47D_EGF",  
                       "cetuximab_MCF7_TGF",    "cetuximab_MCF7_BTC",    "cetuximab_MCF7_HRG",    "cetuximab_MCF7_EGF", 
                       "erlotinib_T47D_TGF",    "erlotinib_T47D_BTC"  ,  "erlotinib_T47D_HRG" ,   "erlotinib_T47D_EGF"  ,  
                       "erlotinib_MCF7_EGF" ,   "erlotinib_MCF7_TGF"   , "erlotinib_MCF7_BTC"  ,  "erlotinib_MCF7_HRG" ,
                       "pertuzumab_T47D_TGF" ,  "pertuzumab_T47D_BTC"   ,"pertuzumab_T47D_HRG"  , "pertuzumab_T47D_EGF" ,   
                       "pertuzumab_MCF7_TGF"  , "pertuzumab_MCF7_BTC",   "pertuzumab_MCF7_HRG"   ,"pertuzumab_MCF7_EGF"  ,
                       "trastuzumab_T47D_HRG"  ,"trastuzumab_T47D_EGF",  "trastuzumab_T47D_TGF",  "trastuzumab_T47D_BTC" ,
                       "trastuzumab_MCF7_TGF",  "trastuzumab_MCF7_BTC" , "trastuzumab_MCF7_HRG" , "trastuzumab_MCF7_EGF"  , 
                       "lumretuzumab_T47D_TGF", "lumretuzumab_T47D_BTC" ,"lumretuzumab_T47D_HRG" ,"lumretuzumab_T47D_EGF" ,
                       "lumretuzumab_MCF7_TGF" ,"lumretuzumab_MCF7_BTC", "lumretuzumab_MCF7_HRG", "lumretuzumab_MCF7_EGF",
                       "no_inhibitor_T47D_BTC", "no_inhibitor_T47D_HRG" ,"no_inhibitor_T47D_EGF" ,"no_inhibitor_T47D_TGF",
                       "no_inhibitor_MCF7_TGF" ,"no_inhibitor_MCF7_BTC" ,"no_inhibitor_MCF7_HRG" ,"no_inhibitor_MCF7_EGF")