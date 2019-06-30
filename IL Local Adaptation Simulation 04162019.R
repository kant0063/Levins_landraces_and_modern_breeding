#### Local Adaptation Simulation ####

libraries = c('lattice', 'car', 'magrittr', 'quantreg', 'foreach', 'doParallel')
for (i in libraries) library(i, character.only=TRUE)

functions_in = "/home/patrick/Cloud/Side Projects/Heritable Microbiomes/Scripts/IL Local Adaptation Permutations Functions 04152019.R"
resource = function() {
  source(functions_in)
}
resource()

#### Housekeeping ####

dir_in = "/home/patrick/Cloud/Side Projects/Heritable Microbiomes/Data"
tall_in = "IL_ALL_YEARS_TALL_03202019.csv"
factors = c('REGION', 'YEAR', 'COMPANY', 'HYBRID', 'LOCATION')

df = file.path(dir_in, tall_in) %>% 
  read.csv(stringsAsFactors=FALSE) %>% 
  subset(IS_CHECK)
df[, factors] %<>% lapply(as.factor)
df[, factors] %<>% lapply(tolower)  # semi-important!! A few names aren't consistent typecases.

df[, 'GENOTYPE'] = paste(df[, 'COMPANY'], df[, 'HYBRID'], sep="_")
df[, 'ENVI'] = paste(df[, 'LOCATION'], df[, 'YEAR'], sep="_")
df[, 'GENOYEAR'] = paste(df[, 'GENOTYPE'], df[, 'YEAR'], sep="_")

#### ID Home ####
df = scale_by_formula(df, YIELD ~ YEAR + LOCATION)
df = id_home_environment(df, STANDARDIZED ~ LOCATION, 'GENOTYPE')

#### Find Coefficients ####
mod = lm(YIELD ~ YEAR + GENOTYPE + LOCATION, data=df)

# qqPlot(residuals(mod))
# influenceIndexPlot(mod)
# ss = summary(mod)
# ss$adj.r.squared
# Anova(mod, type="II")

coefs = coef(mod)

coefs = list(
  YEAR = coefs_vector(df, 'YEAR', coefs),
  GENOTYPE = coefs_vector(df, 'GENOTYPE', coefs),
  LOCATION = coefs_vector(df, 'LOCATION', coefs),
  intercept = coefs['(Intercept)'],
  error = sd(residuals(mod))
)

#### Function ####
sim_LA = function(data, coefficients, ntimes) {
  require(foreach)
  require(magrittr)
  out = foreach(i=1:ntimes, .combine='rbind') %dopar% {
    sim = 
      simulate_data(data, coefficients) %>% 
    # tt = merge(df, sim, by=c('YEAR', 'LOCATION', 'GENOTYPE'))
    # xyplot(SIM ~ YIELD | YEAR, group=LOCATION, data=tt, type=c('r','p'))
      scale_by_formula(SIM ~ YEAR + LOCATION) %>% 
      id_home_environment(STANDARDIZED ~ LOCATION, 'GENOTYPE') %>% 
      coefficient_by_group(linear_model=SIM ~ GENOTYPE + LOCATION + IS_HOME, 
                           groups='YEAR', 
                           coefficient='IS_HOME')
    sim
  }
  return(out)
}

registerDoParallel(cores=4)
ntimes = 999
sims = sim_LA(df, coefs, ntimes)  # in parallel

tt = melt(sims)
mn = round(mean(tt[, 'value']), 1)
boxplot(value ~ as.factor(Var2), 
        data=tt,
        main=paste('Expected Home Compatibility\n', 
                   ntimes, ' Simulations, Mean = ', mn), sep="")
abline(h=mn, lty=2)
abline(h=13.3, lwd=2, col='blue')

write_ctrl = FALSE
# write_ctrl = TRUE
if (write_ctrl) {
  file_out = paste("SIMULATIONS Local Adaptation ", ntimes, "x ", Sys.Date(), ".csv", sep="")
  file_out = file.path(dir_in, file_out)
  write.csv(round(sims, 3), file_out, row.names=FALSE)
}