library(tidyverse)
library(ggplot2)
library(ggthemes)
library(patchwork)

#-----------------------------------------
# import data
#-----------------------------------------

Data <- read_csv2("data.csv")
Data <- Data %>% rename(Luzerner = v_100, Sex = v_6, Altergruppe = v_22, Beziehungstatus = v_7, Quartier = v_113, Zuzug =
                          v_99, Bildung = v_142, HH = v_14, Kinder_Schule= v_158, Erwerb=v_15, Lebensqualallg=v_206, ArbeitsplatzLuzern=v_66,
                        Corona=v_224)



#-----------------------------------------
# carole preprocessing
#-----------------------------------------

df <- Data

colnames(df)

drop <- c("rts6466760",
          "rts6466864",
          "rts6466868",
          "rts6466874",
          "rts6466875",
          "rts6466878",
          "rts6466879",
          "rts6466883",
          "rts6467471",
          "rts6467480",
          "rts6467680",
          "rts6467691",
          "rts6468126",
          "rts6468129",
          "rts6468397",
          "rts6469068",
          "rts6469074",
          "rts6469300",
          "rts6469580",
          "rts6484772", 
          "browser",
          "referer",
          "device_type",
          "quota",
          "quota_assignment",
          "page_history",
          "hflip",
          "vflip",
          "output_mode",
          "javascript",
          "flash",
          "session_id",
          "language",
          "cleaned",
          "ats",
          "datetime",
          "date_of_last_access",
          "date_of_first_mail",
          "external_lfdn",
          "tester",
          "dispcode",
          "v_371")



df <- dplyr::select(df, -drop)


write.csv2(df, file="cleaned_data.csv")
