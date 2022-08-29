# 1 pooled_coefs and pooled_vcovs -> rdata for pf_pv, ue_pv, pi

# 2 newdata hat 15 zeilen, country, age, sex, mit dummy coding eingabe

# 3 var.fit1 oder 2 sollte gleich sein

# 4 nv oder t-verteilgung mit crit wert

# 5 vergleichen mit app, sind ci slightly größer, identisch mittelwerte mit paper?

# 6. von result auf tabelle kommen


library(quantreg)
library(miceadds)
library(tictoc)
library(parallel)
library(pbapply)

# 1. Create pooled_coefs and pooled_vcovs as rdata for pf_pv, ue_pv, pi_pv
sets_with_plausible_values <- readRDS("data/sets_with_plausible_values.rds")
tau = c(.01, .05, seq(.1,.9,.1), .95,.99)

models = lapply(sets_with_plausible_values, function(x) rq(pf_pv ~ country + sex + age - 1, data = x, tau = tau))

tic("vcovs")
vcovs = lapply(1:length(tau), function(x) lapply(1:25, function(y) summary(models[[y]], cov=T)[[x]]$cov))
toc()

tic("params")
params = lapply(1:length(tau), function(x) lapply(1:25, function(y) summary(models[[y]])[[x]]$coef[,1]))
toc()

tic("pooled")
pooled = lapply(1:length(tau), function(x) pool_mi( qhat=params[[x]], u=vcovs[[x]]))
toc()

tic("pooled_coefs")
pooled_coefs = lapply(1:length(tau), function(x) coef(pooled[[x]]))
toc()

tic("pooled_vcovs")
pooled_vcovs = lapply(1:length(tau), function(x) vcov(pooled[[x]]))
toc()

newdata = models$`1`$x[sample(nrow(models[[1]]$x),15),]
newdata

lapply(1:length(tau), function(x){
  predicted_score = newdata %*% pooled_coefs[[x]]
  var.fit1 = diag(newdata %*% pooled_vcovs[[x]] %*% t(newdata))
  var.fit2 = rowSums((newdata %*% pooled_vcovs[[x]]) * newdata)
  se = sqrt(var.fit2)
  result = data.frame(newdata, fit = predicted_score, se = se)
  result$lo = result$fit - 1.96 * result$se
  result$hi = result$fit + 1.96 * result$se
  result
})

# og code Felix
#sets_with_plausible_values2 <- readRDS("~/ao5app/data/sets_with_plausible_values2.rds")
#tau = c(.01, .05, seq(.1,.9,.1), .95,.99)
#
#models = lapply(sets_with_plausible_values2, function(x) rq(pf_pv ~ country + sex + age - 1, data = x, tau = tau))
#
#vcovs = lapply(1:length(tau), function(x) lapply(1:25, function(y) summary(models[[y]], cov=T)[[x]]$cov))
#params = lapply(1:length(tau), function(x) lapply(1:25, function(y) summary(models[[y]])[[x]]$coef[,1]))
#
#pooled = lapply(1:length(tau), function(x) pool_mi( qhat=params[[x]], u=vcovs[[x]]))
#
#pooled_coefs = lapply(1:length(tau), function(x) coef(pooled[[x]]))
#pooled_vcovs = lapply(1:length(tau), function(x) vcov(pooled[[x]]))
#
#
#newdata = models$`1`$x[sample(nrow(models[[1]]$x),15),]
#newdata
#
#lapply(1:length(tau), function(x){
#  predicted_score = newdata %*% pooled_coefs[[x]]
#  var.fit1 = diag(newdata %*% pooled_vcovs[[x]] %*% t(newdata))
#  var.fit2 = rowSums((newdata %*% pooled_vcovs[[x]]) * newdata)
#  se = sqrt(var.fit2)   result = data.frame(newdata, fit = predicted_score, se = se)
#  result$lo = result$fit - 1.96 * result$se
#  result$hi = result$fit + 1.96 * result$se
#  result
#})

## parallelized for app?
cl = 9
system.time(res1pbcl <- pblapply(1:B, function(i) fun(bid[,i]), cl = cl))
tic()
vcovs_parallel = pblapply(1:length(tau), function(x) lapply(1:25, function(y) summary(models[[y]], cov=T)[[x]]$cov), cl = cl)
toc()
all.equal(vcovs, vcovs_parallel)


## Creating table

# Tables

## data prep code
plotdata_pf %>% 
  dplyr::filter(sex == 1 & age == 66 & country == 1) %>% 
  mutate(value = paste0(round(fit, 2), " [", round(lower, 2), "; ", round(higher, 2), "]")) %>%
  dplyr::select(tau, value) %>% 
  pivot_wider(names_from = tau, values_from = value)  

## shiny code

output$table_pf <- renderTable({
  result <- plotdata_pf %>% 
    dplyr::filter(sex == input$sex & age == input$age & country == ifelse(input$country == "country0", 0, 
                                                                          ifelse(input$country == "country1", 1, 
                                                                                 ifelse(input$country == "country2", 2)))) %>% 
    mutate(value = paste0(sprintf("%.1f",fit), " [", sprintf("%.1f",lower), "; ", sprintf("%.1f", higher), "]")) %>%
    dplyr::select(tau, value) %>% 
    pivot_wider(names_from = tau, values_from = value)  
  colnames(result) = paste0(sprintf("%.0f", as.numeric(colnames(result))*100), "%")
  result
  
})