# Bootstrapping local adaptation

#### Libraries ####
libraries = c('lattice','magrittr', 'quantreg', 'foreach', 'doParallel')
functions_in = "/home/patrick/Cloud/Side Projects/Heritable Microbiomes/Scripts/IL Local Adaptation Permutations Functions 04152019.R"
for (i in libraries) library(i, character.only=TRUE)

resource = function() {
  source(functions_in)
}
resource()

#### Data ####
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


#### Steps ####
# 1. Permute and Center
#   - Subset by year
#   - Reassign yield
#   - Center/scale
#   - Re-combine
# 2. Assign Home
#   - Subset by genotype
#   - calculate mean relative yield ~ environment | year
#   - Assign home as max aggregated relative yield
#   - Re-combine
# 3. Calculate local Adaptation
#   - subset by year
#   - model yield ~ genotype + environment + home_site
#   - pull home_site coefficient for each year
#   - save these coefficients
# Output: Iteration, Year, Home Advantage

#### Start ####
RESPONSE = 'YIELD'
SPLIT = 'YEAR'
PERMUTE_BY = 'ENVI'
HOME_ON = 'GENOTYPE'  # factor on which to define home environment
CENTER_FORMULA = '~ LOCATION + YEAR'
ADAPTATION_FORMULA = '~ GENOTYPE + LOCATION + IS_HOME' # for calculating local adaptation. Response is response.
HOME_FORMULA = '~ LOCATION' # for IDing local site. Response is flagged by 'center_for_home'
data = df
ntimes = 99


registerDoParallel(cores=4)
permute_loop = function(PERMUTE_BY, ntimes) {
  out = foreach(i=1:ntimes, .combine='rbind') %dopar% {
    # require(quantreg)
    #### Permute ####
    dd = permute(data, PERMUTE_BY, RESPONSE)#, center=CENTER_FOR_HOME) # scale by default
    # Verify permutation
    # tt = merge(data, dd, by=factors)
    # xyplot(YIELD.y ~ YIELD.x | LOCATION, group=YEAR, data=tt, type=c('r', 'p'), alpha=0.1, pch=20)
    
    tt = is.character(CENTER_FORMULA)
    if (tt) {
      ff = paste0(RESPONSE, 
                  CENTER_FORMULA)
      dd = scale_by_formula(dd, 
                            as.formula(ff))
    }
    
    #### ID new home environment ####
    resp = ifelse(tt, 
                  'STANDARDIZED', 
                  RESPONSE) # identifies fitness column from permute_center
    ff = paste0(resp, HOME_FORMULA)
    dd = id_home_environment(dd, 
                             as.formula(ff),           # name of fitness column
                             group_variable=HOME_ON)     # name of genotype column
    
    #### Calculate yearly home advantage ####
    ff = paste0(RESPONSE, ADAPTATION_FORMULA)
    dd = coefficient_by_group(data=dd, 
                              linear_model=as.formula(ff), 
                              groups=SPLIT, 
                              coefficient='IS_HOME')
    # mat_out[i, ] = dd
  }
  return(out)
}

NN=999
year_mat = permute_loop(PERMUTE_BY='YEAR',
                        ntimes=NN)
envi_mat = permute_loop(PERMUTE_BY='ENVI',
                        ntimes=NN)
gy_mat = permute_loop(PERMUTE_BY='GENOYEAR',
                        ntimes=NN)

# Quick plot
tt = read.csv('/home/patrick/Cloud/Side Projects/Heritable Microbiomes/Data/Permutations Local Adaptation 99x on YEAR 2019-04-16.csv')
NN = nrow(tt)
tt = melt(tt)
tt[, 'variable'] = gsub("X", "", tt[, 'variable'])
mn = round(mean(tt[, 'value']), 1)
boxplot(value ~ as.factor(variable), 
        data=tt,
        main=paste('Expected Local Adaptation\n', 
                   NN, ' Permutations on YEAR, Mean = ', mn), sep="")
abline(h=mn, lty=2)
abline(h=13.3, lwd=2, col='blue')

tt = read.csv('/home/patrick/Cloud/Side Projects/Heritable Microbiomes/Data/Permutations Local Adaptation 99x on SITE-YEAR 2019-04-16.csv')
NN = nrow(tt)
tt = melt(tt)
tt[, 'variable'] = gsub("X", "", tt[, 'variable'])
mn = round(mean(tt[, 'value']), 1)
boxplot(value ~ as.factor(variable), 
        data=tt,
        main=paste('Expected Local Adaptation\n', 
                   NN, ' Permutations on SITE-YEAR, Mean = ', mn, sep=""))
abline(h=mn, lty=2)
abline(h=13.3, lwd=2, col='blue')

tt = melt(gy_mat)
mn = round(mean(tt[, 'value']), 1)
boxplot(value ~ as.factor(Var2), 
        data=tt,
        main=paste('Expected Local Adaptation\n', 
                   NN, ' Permutations on GENOTYPE-YEAR, Mean = ', mn, sep=""))
abline(h=mn, lty=2)
abline(h=13.3, lwd=2, col='blue')


# write
write_control = FALSE
# write_control = TRUE
if (write_control) {
  mat = year_mat
  var = 'YEAR'
  
  file_out = paste("Permutations Local Adaptation ", ntimes, "x on ", var, " ", Sys.Date(), ".csv", sep="")
  file_out = file.path(dir_in, file_out)
  write.csv(mat, file_out, row.names=FALSE)
  print(file_out)
}

if (write_control) {
  mat = envi_mat
  var = 'SITE-YEAR'
  
  file_out = paste("Permutations Local Adaptation ", ntimes, "x on ", var, " ", Sys.Date(), ".csv", sep="")
  file_out = file.path(dir_in, file_out)
  write.csv(mat, file_out, row.names=FALSE)
  print(file_out)
}