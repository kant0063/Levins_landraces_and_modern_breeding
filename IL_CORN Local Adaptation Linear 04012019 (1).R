#! /bin/R
#
# PME Updated April 1st, 2019

libraries = c('lme4', 'lattice', 'car', 'reshape2', 'magrittr', 'quantreg', 'ggplot2')
for (i in libraries) library(i, character.only=TRUE)

#### Housekeeping ####

dir_in = "/home/patrick/Cloud/Side Projects/Heritable Microbiomes/Data"
tall_in = "IL_ALL_YEARS_TALL_03202019.csv"

df = file.path(dir_in, tall_in) %>% 
  read.csv

df$YEAR %<>% as.factor

#### First Look ####
densityplot(~YIELD | YEAR, data=df)

s15 = subset(df, YEAR==2015 & !is.na(YIELD))

densityplot(~YIELD | LOCATION, data=s15, main='2015 Data')

#### Model ####
# The linear model is:
#   Yield ~ Hybrid (genotype) + Location + Is_Home + error - for each year.
#   Is_Home ~ Time
#
# To do this:
#   1. Subset checks
#   2. ID the home location. This is the location where the hybrid does the best relative to others.
#     a. Center/scale by year and location
#     b. Aggregate mean yield by hybrid and location
#     c. ID max location for each hybrid.
#     d. Flag home location in checks df
#   3. Run model for each year
#   4. Extract home_advantage coefficient
#   5. Regress Is_Home ~ Time

#### ID CHECKS ####
checks = subset(df, IS_CHECK)

# Center yields by location and year
checks[, 'LOCYR'] = paste(checks[, 'LOCATION'], checks[, 'YEAR'], sep="_") %>% 
  as.factor

centered = split(checks, checks[, 'LOCYR']) %>% 
  lapply(function(x) {
    x[, 'YIELD'] %<>% scale
    return(x) 
  }) %>% 
  do.call(rbind, .)

#### Find home location ####
# Find home location - the highest average relative yield
centered[, 'GENOTYPE'] = paste(centered[, 'COMPANY'], centered[, 'HYBRID'], sep="_") %>% 
  as.factor

centered = split(centered, centered[, 'GENOTYPE']) %>% 
  lapply(function(x) {
    # aggregate to mean (relative) yield within location across years
    aggd = aggregate(YIELD ~ LOCATION + GENOTYPE, data=x, function(y) mean(y, na.rm=T))
    names(aggd)[3] = 'YIELD'
    
    # find hightest mean yield
    tt = aggd[, 'YIELD'] == max(aggd[, 'YIELD'])
    aggd[, 'IS_HOME'] = tt
    aggd[, 'YIELD'] = NULL
    
    # Merge back to non-aggregated df
    x = merge(x, aggd, by=c('LOCATION', 'GENOTYPE'))
    return(x)
  }) %>% 
  do.call(rbind, .)
rownames(centered) = 1:nrow(centered)

# Send home info back to original df
merge_by = c('COMPANY', 'HYBRID', 'YEAR', 'LOCATION')
checks1 = centered[, c(merge_by, 'IS_HOME', 'GENOTYPE')] %>% 
  merge(checks, ., by=merge_by)

checks = checks1


#### Test G*E ####
# As a single model across years
# checks[, 'YEAR_NUMBER'] = as.numeric(checks[, 'YEAR']) - min(as.numeric(checks[, 'YEAR']))
# checks[, 'HOME_YEAR'] = checks[, 'YEAR_NUMBER'] * checks[, 'IS_HOME']
# checks[, 'GENO_YEAR'] = paste(checks[, 'GENOTYPE'], checks[, 'YEAR'], sep="_") %>%
#   as.factor
checks[, 'GENO_LOC'] = paste(checks[, 'GENOTYPE'], checks[, 'LOCATION'], sep="_") %>%
  as.factor
# checks[, 'GENO_LOC_YEAR'] = paste(checks[, 'GENO_LOC'], checks[, 'YEAR'], sep="_") %>% 
#   as.factor
# checks[, 'LOC_YEAR'] = paste(checks[, 'LOCATION'], checks[, 'YEAR'], sep="_") %>% 
#   as.factor

#### G*E, across years ####
check_sub = duplicated(checks$GENO_LOC) %>% 
  which %>% 
  c(duplicated(checks$GENO_LOC, fromLast=TRUE) %>% 
      which) %>% 
  unique %>% 
  checks[., ]

m = lm(YIELD ~ GENOTYPE + LOCATION + YEAR + YEAR:LOCATION + GENOTYPE:LOCATION + GENOTYPE:YEAR, data=check_sub)
a = Anova(m, type="II")
qqPlot(residuals(m))


#### Local Adaptation - Across Years ####
mod = lm(YIELD ~ IS_HOME + YEAR + GENOTYPE + LOCATION, data=checks)

qqPlot(residuals(mod))
ss = summary(mod)
ss$adj.r.squared
Anova(mod, type="II")

# Local Adaptation Effect Size
print("Local Adaptation Effect Size")
ss$coefficients["IS_HOMETRUE", ]

# Local Adaptation Effect Relative
print("Local Adaptation Effect Relative")
ss$coefficients["IS_HOMETRUE", 'Estimate']/ss$coefficients['(Intercept)', 'Estimate']

anv = Anova(mod, type="II")
anv['IS_HOME', 'Sum Sq'] / sum(anv[, 'Sum Sq'])


#### Local Adaptation - By Year ####
yy = split(checks, checks[, 'YEAR'])
yy['2012'] = NULL

local_adapt = lapply(yy, function(x) {
  mod = rq((YIELD) ~ GENOTYPE + LOCATION + IS_HOME, data=x) # robust regression (median quantile) each year
  vals = summary(mod, se='boot')                            # bootstrap standard errors
  vals = vals$coefficients[c('(Intercept)', 'IS_HOMETRUE'), c('Value', 'Std. Error')]  # make a table
  return(vals)
}) 

# Make a nice dataframe
local_adapt = lapply(names(local_adapt), function(x) {
  out = local_adapt[[x]] %>% 
    data.frame
  out$MEASURE = rownames(out)
  out$YEAR = x
  return(out)
}) %>% 
  do.call(rbind, .) %>% 
  data.frame %>% 
  split(.$MEASURE) %>% 
  lapply(function(x) {
    names(x) = c('VALUE', 'ERROR', 'MEASURE', 'YEAR')
    x$YEAR %<>% 
      as.character %>% 
      as.numeric
    x$YEAR_NUMBER = x$YEAR - min(x$YEAR)
    return(x)
  })

home = local_adapt[['IS_HOMETRUE']]
intercept = local_adapt[['(Intercept)']]

mod = lm(VALUE ~ YEAR_NUMBER, data=home)
summary(mod)

print("#####    Mean Local Adaptation Across Years    #####")
mean(home$VALUE)

#### Plot Local Adaptation By Year ####
overwrite=FALSE
if(overwrite) {
  write.csv(home, 
            file.path(dir_in, "Local Adaptation by Year 04012019.csv"),
            row.names=FALSE)
}

plt = ggplot(home) +
  ylim(c(0, 25)) +
  xlab("Year") +
  ylab("Home Field Advantage [bu/ac]") +
  geom_errorbar(aes(x=YEAR,
                    ymin=VALUE-ERROR,
                    ymax=VALUE+ERROR),
                width=0.2,
                color="#666666") +
  geom_point(aes(x=YEAR,
                 y=VALUE),
             color='steelblue3',
             size=4) +
  theme_light()
plt

#### Yield across years ####
yields = aggregate(YIELD ~ YEAR, data=checks, mean) %>% 
  data.frame(ERROR = aggregate(YIELD ~ YEAR, 
                               data=checks,
                               function(x) sd(x)/sqrt(length(x-1)))[, 2])
yields$YEAR %<>% as.character %>% as.numeric


ff = formula('YIELD ~ YEAR')
mod = lm(ff, data=yields)
summary(mod)
Anova(mod)

#### Plot Yield by Year ####
overwrite=FALSE
if(overwrite) {
  write.csv(yields, 
            file.path(dir_in, "Yield by Year 04012019.csv"),
            row.names=FALSE)
}

plt = ggplot(yields) +
  xlab("Year") +
  ylab("Yield [bu/ac]") +
  geom_errorbar(aes(x=YEAR,
                    ymin=YIELD-ERROR,
                    ymax=YIELD+ERROR),
                width=0.2,
                color="#666666") +
  geom_smooth(aes(x=YEAR,
                  y=YIELD),
              color='tomato4',
              method='lm',
              weight=0.1,
              linetype="dashed") +
  geom_point(aes(x=YEAR,
                 y=YIELD),
             color='steelblue3',
             size=4) +
  theme_light()
plt