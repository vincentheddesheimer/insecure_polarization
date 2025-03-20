# install packages from CRAN

# # install "HonestDID"
# if ("HonestDID" %in% rownames(installed.packages()) == FALSE) {
#   devtools:: install_github("asheshrambachan/HonestDiD")
# }  
# 
# # update "fect"
# devtools:: install_github("xuyiqing/fect", upgrade = "always")
# 
# 
# # install "paneltools"
# if ("paneltools" %in% rownames(installed.packages()) == FALSE) {
#   devtools:: install_github("xuyiqing/paneltools")
# }

pacman::p_load(dplyr, readstata13, fixest, did, fect, didimputation,
               panelView, PanelMatch, ggplot2, bacondecomp, HonestDiD, tidyverse,
               paneltools)


data <- fread("data/liss.csv")

# create ID & time
data <- data |>
  mutate(id = paste0(nomem_encr,wave),
         t = as.integer(wave - 7))

dupl <- data |>
  count(id) |>
  filter(n>1)

dupldf <- data |>
  filter(id %in% dupl$id)

data <- data |>
  filter(!(id %in% dupl$id) & !is.na(wave)) |>
  # create identifier
  arrange(nomem_encr,wave) |>
  group_by(nomem_encr) |>
  mutate(id = cur_group_id()) |>
  ungroup() |>
  data.frame()

# drop people with missing values for unemployed
data <- data |>
  filter(!is.na(unemployed))

rm(dupl, dupldf)


# Visualize treatment -----------------------------------------------------

y <- "distance"
d <- "unemployed"
unit <- "id"
time <- "t"
controls <- c("education_cat", "no_children_hh", "partner", "student","retired", "l1_net_monthly_income_cat", "house_owner")
index <- c("id", "t")

panelview(Y=y, D=d, X=controls, index = index, data = data, xlab = "Time Period", ylab = "Unit", 
          gridOff = TRUE, by.timing = TRUE, cex.legend=5, cex.axis= 5, cex.main = 10, cex.lab = 5,
          background = "white")



# TWFE --------------------------------------------------------------------

model.twfe <- feols(distance ~ unemployed + 
                      education_cat + no_children_hh + partner + student + retired + l1_net_monthly_income_cat + house_owner |
                      id + t,
                    data=data, cluster = "id") # use the clustered standard error
print(model.twfe)



# Event study plot --------------------------------------------------------

data_cohort <- get.cohort(data, index = index, D=d, start0 = TRUE)
# Generate a dummy variable treat
data_cohort$treat <- 0
data_cohort[which(data_cohort$Cohort!='Control'),'treat'] <- 1
data_cohort[which(is.na(data_cohort$Time_to_Treatment)), "treat"] <- 0

# remove observations that starts with treated status
remove <- intersect(which(is.na(data_cohort$Time_to_Treatment)), which(data_cohort[,d]==1)) 
if(length(remove)>0){data_cohort <- data_cohort[-remove,]}

# replace missingness in Time_to_Treatment with an arbitrary number
data_cohort[which(is.na(data_cohort$Time_to_Treatment)), "Time_to_Treatment"] <- 999 

twfe.est <- feols(distance ~ i(Time_to_Treatment, treat, ref = -1) + 
                    education_cat + no_children_hh + partner + student + retired + l1_net_monthly_income_cat + house_owner |
                    id + t,  
                  data = data_cohort, cluster = "id")
summary(twfe.est)
twfe.output <- as.matrix(twfe.est$coeftable[c(1:28),])
print(twfe.output)

twfe.output <- as.data.frame(twfe.output)
twfe.output$Time <- c(c(-14:-2),c(0:14))+1 
p.twfe <- esplot(twfe.output,Period = 'Time',Estimate = 'Estimate',
                 SE = 'Std. Error', xlim = c(-15,15))
p.twfe



# Imputation method -------------------------------------------------------

model.fect <- fect(Y = y, D = d, X= controls, data = data, 
                   method = "fe", 
                   index = index, se = TRUE, parallel = TRUE, seed = 1234, force = "two-way")
print(model.fect$est.avg)

fect.output <- as.matrix(model.fect$est.att)
print(fect.output)

fect.output <- as.data.frame(fect.output)
fect.output$Time <- c(-15:10)
p.fect <- esplot(fect.output,Period = 'Time',Estimate = 'ATT',
                 SE = 'S.E.',CI.lower = "CI.lower", 
                 CI.upper = 'CI.upper', xlim = c(-15,1))
p.fect

