################################################
#### code to source Table 1 - assessment sheet## KVDR
################################################

tab1 <- as.data.frame(matrix(data=NA,nrow = 16, ncol= 13))
tab1[1:4,1]   <- c("VME habitat","200-400m","400-800m",">800m") 
tab1[5:8,1]   <- c("VME index Medium and High","200-400m","400-800m",">800m")
tab1[9:12,1] <- c("VME index Low","200-400m","400-800m",">800m")
tab1[13:16,1] <- c("VME physical elements","Seamounts","Banks","Coral mounds")

# get existing VMEs
tab1[2:4,2]   <- c(length(which(reg$Exist_VMEs==3 & reg$cat =="de2_4")),length(which(reg$Exist_VMEs==3 & reg$cat =="de4_8")),
                   length(which(reg$Exist_VMEs==3 & reg$cat =="de8")))
tab1[6:8,2]   <- c(length(which(reg$Exist_VMEs %in% c(2,1) & reg$cat =="de2_4")),length(which(reg$Exist_VMEs %in% c(2,1) & reg$cat =="de4_8")),
                   length(which(reg$Exist_VMEs %in% c(2,1) & reg$cat =="de8")))
tab1[10:12,2] <- c(length(which(reg$Exist_VMEs == 0 & reg$cat =="de2_4")),length(which(reg$Exist_VMEs == 0 & reg$cat =="de4_8")),
                   length(which(reg$Exist_VMEs == 0 & reg$cat =="de8")))
tab1[14,2]    <- reg %>% select(seaMt) %>% sum(na.rm=T) 
tab1[15,2]    <- reg %>% select(banks) %>% sum(na.rm=T) 
tab1[16,2]    <- reg %>% select(corMd) %>% sum(na.rm=T)


# now get overlap with fished area
tab1[c(2:4,6:8,10:12),c(3,4,5,8,9,10)] <- 0

if (1 %in% reg$FareaM){
  tt <- table(factor(reg$cat)[reg$FareaM==1],factor(reg$Exist_VMEs)[reg$FareaM==1]==3) 
  if(length(which(colnames(tt) =="TRUE")) == 1){tab1[2:4,3] <- tt[,which(colnames(tt) =="TRUE")]}
  tt <- table(reg$cat[reg$FareaM==1],reg$Exist_VMEs[reg$FareaM==1] %in% c(2,1))
  if(length(which(colnames(tt) =="TRUE")) == 1){tab1[6:8,3] <- tt[,which(colnames(tt) =="TRUE")]}
  tt <- table(reg$cat[reg$FareaM==1],reg$Exist_VMEs[reg$FareaM==1] %in% c(0))
  if(length(which(colnames(tt) =="TRUE")) == 1){tab1[10:12,3] <- tt[,which(colnames(tt) =="TRUE")]}
  tab1[14:16,3]  <- reg %>% filter(FareaM==1) %>% select(seaMt, banks, corMd) %>% purrr::map_dfr(sum, na.rm = T) %>% t()
  }
# 
# if (1 %in% reg$FareaS){
#   tt <- table(factor(reg$cat)[reg$FareaS==1],factor(reg$Exist_VMEs)[reg$FareaS==1]==3)
#   if(length(which(colnames(tt) =="TRUE")) == 1){tab1[2:4,4] <- tt[,which(colnames(tt) =="TRUE")]}
#   tt <- table(factor(reg$cat)[reg$FareaS==1],factor(reg$Exist_VMEs)[reg$FareaS==1] %in% c(2,1))
#   if(length(which(colnames(tt) =="TRUE")) == 1){ tab1[6:8,4] <- tt[,which(colnames(tt) =="TRUE")]}
#   tt <- table(factor(reg$cat)[reg$FareaS==1],factor(reg$Exist_VMEs)[reg$FareaS==1] %in% c(0))
#   if(length(which(colnames(tt) =="TRUE")) == 1){ tab1[10:12,4] <- tt[,which(colnames(tt) =="TRUE")]}
#   tab1[14:16,4]  <- reg %>% filter(FareaS==1) %>% select(seaMt, banks, corMd) %>% purrr::map_dfr(sum, na.rm = T) %>% t()
# }

# if (1 %in% reg$FareaC){
#   tt <- table(factor(reg$cat)[reg$FareaC==1],factor(reg$Exist_VMEs)[reg$FareaC==1]==3)
#   if(length(which(colnames(tt) =="TRUE")) == 1){ tab1[2:4,5] <- tt[,which(colnames(tt) =="TRUE")]}
#   tt <- table(factor(reg$cat)[reg$FareaC==1],factor(reg$Exist_VMEs)[reg$FareaC==1] %in% c(2,1))
#   if(length(which(colnames(tt) =="TRUE")) == 1){ tab1[6:8,5] <- tt[,which(colnames(tt) =="TRUE")]}
#   tt <- table(factor(reg$cat)[reg$FareaC==1],factor(reg$Exist_VMEs)[reg$FareaC==1] %in% c(0))
#   if(length(which(colnames(tt) =="TRUE")) == 1){ tab1[10:12,5] <- tt[,which(colnames(tt) =="TRUE")]}
#   tab1[14:16,5]  <- reg %>% filter(FareaC==1) %>% select(seaMt, banks, corMd) %>% purrr::map_dfr(sum, na.rm = T) %>% t()
# }

# overlap for the new assessment

# get newest VME grid
tab1[2:4,7]   <- c(length(which(reg$Tot_VMEs==3 & reg$cat =="de2_4")),length(which(reg$Tot_VMEs==3 & reg$cat =="de4_8")),
                   length(which(reg$Tot_VMEs==3 & reg$cat =="de8")))
tab1[6:8,7]   <- c(length(which(reg$Tot_VMEs %in% c(2,1) & reg$cat =="de2_4")),length(which(reg$Tot_VMEs %in% c(2,1) & reg$cat =="de4_8")),
                   length(which(reg$Tot_VMEs %in% c(2,1) & reg$cat =="de8")))
tab1[10:12,7] <- c(length(which(reg$Tot_VMEs == 0 & reg$cat =="de2_4")),length(which(reg$Tot_VMEs == 0 & reg$cat =="de4_8")),
                   length(which(reg$Tot_VMEs == 0 & reg$cat =="de8")))
# fysical elements
tab1[14,7]    <- reg %>% select(seaMt) %>% sum(na.rm=T)
tab1[15,7]    <- reg %>% select(banks) %>% sum(na.rm=T)
tab1[16,7]    <- reg %>% select(corMd) %>% sum(na.rm=T)

# get overlap with fisheries
if (1 %in% reg$FareaM){
  tt <- table(factor(reg$cat)[reg$FareaM==1],factor(reg$Tot_VMEs)[reg$FareaM==1]==3) 
  if(length(which(colnames(tt) =="TRUE")) == 1){tab1[2:4,8] <- tt[,which(colnames(tt) =="TRUE")]}
  tt <- table(reg$cat[reg$FareaM==1],reg$Tot_VMEs[reg$FareaM==1] %in% c(2,1))
  if(length(which(colnames(tt) =="TRUE")) == 1){tab1[6:8,8] <- tt[,which(colnames(tt) =="TRUE")]}
  tt <- table(reg$cat[reg$FareaM==1],reg$Tot_VMEs[reg$FareaM==1] %in% c(0))
  if(length(which(colnames(tt) =="TRUE")) == 1){tab1[10:12,8] <- tt[,which(colnames(tt) =="TRUE")]}
  tab1[14:16,8]  <- reg %>% filter(FareaM==1) %>% select(seaMt, banks, corMd) %>% purrr::map_dfr(sum, na.rm = T) %>% t()
}
# if (1 %in% reg$FareaS){
#   tt <- table(factor(reg$cat)[reg$FareaS==1],factor(reg$New_VMEs)[reg$FareaS==1]==3) 
#   if(length(which(colnames(tt) =="TRUE")) == 1){tab1[2:4,9] <- tt[,which(colnames(tt) =="TRUE")]}
#   tt <- table(reg$cat[reg$FareaS==1],reg$New_VMEs[reg$FareaS==1] %in% c(2,1))
#   if(length(which(colnames(tt) =="TRUE")) == 1){tab1[6:8,9] <- tt[,which(colnames(tt) =="TRUE")]}
#   tt <- table(reg$cat[reg$FareaS==1],reg$New_VMEs[reg$FareaS==1] %in% c(0))
#   if(length(which(colnames(tt) =="TRUE")) == 1){tab1[10:12,9] <- tt[,which(colnames(tt) =="TRUE")]}
#   # tab1[14:16,9]  <- reg %>% filter(FareaS==1) %>% select(seaMt, banks, corMd) %>% purrr::map_dfr(sum, na.rm = T) %>% t()
# }
# if (1 %in% reg$FareaC){
#   tt <- table(factor(reg$cat)[reg$FareaC==1],factor(reg$New_VMEs)[reg$FareaC==1]==3) 
#   if(length(which(colnames(tt) =="TRUE")) == 1){tab1[2:4,10] <- tt[,which(colnames(tt) =="TRUE")]}
#   tt <- table(reg$cat[reg$FareaC==1],reg$New_VMEs[reg$FareaC==1] %in% c(2,1))
#   if(length(which(colnames(tt) =="TRUE")) == 1){tab1[6:8,10] <- tt[,which(colnames(tt) =="TRUE")]}
#   tt <- table(reg$cat[reg$FareaC==1],reg$New_VMEs[reg$FareaC==1] %in% c(0))
#   if(length(which(colnames(tt) =="TRUE")) == 1){tab1[10:12,10] <- tt[,which(colnames(tt) =="TRUE")]}
#   # tab1[14:16,10]  <- reg %>% filter(FareaC==1) %>% select(seaMt, banks, corMd) %>% purrr::map_dfr(sum, na.rm = T) %>% t()
# }

## Now only select the updated/newly created c-squares
tab1[2:4,12]   <- c(length(which(reg$New_VMEs==3 & reg$cat =="de2_4")),length(which(reg$New_VMEs==3 & reg$cat =="de4_8")),
                    length(which(reg$New_VMEs==3 & reg$cat =="de8")))
tab1[6:8,12]   <- c(length(which(reg$New_VMEs %in% c(2,1) & reg$cat =="de2_4")),length(which(reg$New_VMEs %in% c(2,1) & reg$cat =="de4_8")),
                    length(which(reg$New_VMEs %in% c(2,1) & reg$cat =="de8")))
tab1[10:12,12] <- c(length(which(reg$New_VMEs == 0 & reg$cat =="de2_4")),length(which(reg$New_VMEs == 0 & reg$cat =="de4_8")),
                    length(which(reg$New_VMEs == 0 & reg$cat =="de8")))
tab1[14:16,12] <- 0 # no changes in geophysical elements

# get overlap with fisheries
if (1 %in% reg$FareaM){
  tt <- table(factor(reg$cat)[reg$FareaM==1],factor(reg$New_VMEs)[reg$FareaM==1]==3) 
  if(length(which(colnames(tt) =="TRUE")) == 1){tab1[2:4,13] <- tt[,which(colnames(tt) =="TRUE")]}
  tt <- table(reg$cat[reg$FareaM==1],reg$New_VMEs[reg$FareaM==1] %in% c(2,1))
  if(length(which(colnames(tt) =="TRUE")) == 1){tab1[6:8,13] <- tt[,which(colnames(tt) =="TRUE")]}
  tt <- table(reg$cat[reg$FareaM==1],reg$New_VMEs[reg$FareaM==1] %in% c(0))
  if(length(which(colnames(tt) =="TRUE")) == 1){tab1[10:12,13] <- tt[,which(colnames(tt) =="TRUE")]}
  # tab1[14:16,13]  <- reg %>% filter(FareaM==1) %>% select(seaMt, banks, corMd) %>% purrr::map_dfr(sum, na.rm = T) %>% t()
}
 tab1[14:16,13] <- 0 # no changes in geophysical elements

