#!/bin/R
# PME 03192019
# 
# Merge and stack regional Illinois yield data into tall and wide dataframes, flagging check types

#### Housekeeping #####
libs = c('magrittr', 'lme4', 'car', 'lattice', 'reshape2')
for (i in libs) library(i, character.only=TRUE)


# specify and load directories, file names
dir_in = "/home/patrick/Cloud/Side Projects/Heritable Microbiomes/Data/Regional"
dir_out = "/home/patrick/Cloud/Side Projects/Heritable Microbiomes/Data/"

files_in = list.files(dir_in) %>% 
  extract(grepl(".csv", .)) 

# name regions
regions = sapply(files_in, function(x) {
  strsplit(x, "_")[[1]][1]
})

##### load files and format dataframes ####
dd = lapply(files_in,  # load files
            function(x){
              file.path(dir_in, x) %>% 
                read.csv(stringsAsFactors=FALSE)
            }) %>% 
  lapply(function(x) {  # uppercase names
    names(x) %<>% toupper
    return(x)
  }) %>% 
  lapply(function(x) {  # remove "_YIELD" tag
    names(x) %<>% gsub('_YIELD', '', .)
    return(x)
  })

dd %<>% # select factors and numerics
  lapply(function(x) {  
    l = x[, 1:3]  # year, company, hybrid
    r = x[, 4:ncol(x)] %>% 
      lapply(as.numeric)
    return(data.frame(l, r))
  }) %>% 
  lapply(function(x) {
    x[, 'YEAR'] %<>% as.factor
    return(x)
  })

#### find duplicates in original data, and drop them ####
dups = lapply(dd, function(x) {
  t = duplicated(x[, merge_by])
  t1 = duplicated(x[, merge_by], fromLast=TRUE)
  t = c(which(t), which(t1))
  return(x[t, ])
})
# Two hybrids were duplicated in 2004: Horizon 7409 CB in the east, and Kruger 5315 in the north. Best to remove them. 

drop_rownames = lapply(dups, rownames)

dd = lapply(c(1:4), function(x) {
  d = dd[[x]]
  r = sapply(rownames(d), function(y) y %in% drop_rownames[[x]])
  out = d[!r, ]
  return(out)
})

##### Merge into a wide dataframe of only checks, which should be in all regions for a given year - therefore, an inner join ####
merge_by = c('YEAR', 'COMPANY', 'HYBRID')
checks = dd[[1]][, merge_by]
for (i in 2:length(dd)) {
  checks %<>% merge(., dd[[i]][, merge_by], # inner joins
                    by=merge_by)
}

##### Merge into a wide dataframe complete, with a flag for is_check ####
df = dd[[1]]
for (i in 2:length(dd)) {
  df %<>% merge(., dd[[i]], # full outer joins
                by=merge_by, 
                all=TRUE)
}

tt = list()
for (i in merge_by) {
  tt[[i]] = sapply(df[, i], function(x) x %in% checks[, i])
}
tt %<>% 
  as.data.frame %>% 
  apply(1, function(x) sum(x) == 3)

df[, 'IS_CHECK'] = tt


##### Stack into a tall dataframe with region info ####
# add region name
for (i in 1:length(regions)) {
  dd[[i]] %<>% data.frame(REGION=regions[i],
                          .)
}

# melt and stack
tall = lapply(dd, function(x) {
    melt(x, value.name='YIELD')
  }) %>% 
  do.call(rbind, .) %>% 
  as.data.frame

names(tall) %<>% gsub('variable', 'LOCATION', .)

# add flag for checks
tt = list()
for (i in merge_by) {
  tt[[i]] = sapply(tall[, i], function(x) x %in% checks[, i])
}
tt %<>% 
  as.data.frame %>% 
  apply(1, function(x) sum(x) == 3)

tall[, 'IS_CHECK'] = tt


# Check: Kept Values
count_values = function(x) sum(!(is.na(x)))

tt = sapply(dd, function(x) sapply(x[, -c(1:4)], count_values)) %>% 
  sapply(sum) %>% 
  sum

t1 = sapply(df[, 4:22], count_values) %>% 
  sum

tt - t1  # 0

# Check: Total Yields
sum_na = function(x) sum(x, na.rm=TRUE)

tt = sapply(dd, function(x) sapply(x[, -c(1:4)], sum_na)) %>% 
  sapply(sum) %>% 
  sum

t1 = sapply(df[, 4:22], sum_na) %>% 
  sum

tt - t1

# Check: Unjoin and compare
site_names = sapply(dd, function(x) colnames(x)[-c(1:4)])
base_names = colnames(dd[[1]])[2:4]

tt = lapply(1:4, function(x) {
  cnames = c(base_names, site_names[[x]])
  return(df[, cnames])
})

rm_na = function(x) {
  dat = x[, -c(1:3)]
  t = rowSums(is.na(dat))
  t = t == max(t)
  return(x[!t, ])
}
tt = lapply(tt, rm_na)

is_unequal = function(a, b) {
  a = a[order(1:3), ]
  b = b[order(1:3), ]
  
  asub = a[, -c(1:3)]
  bsub = b[, -c(1:3)]
  
  asub = apply(asub, c(1,2), function(x) ifelse(is.na(x), -1, x))
  bsub = apply(bsub, c(1,2), function(x) ifelse(is.na(x), -1, x))
  
  out = asub != bsub %>% 
    sum

  return(out)
}

equals = rep(0, length.out=4)
for (i in 1:4) {
  equals[i] = is_unequal(dd[[i]][, -c(1)], tt[[i]])
}
equals

missings = function(a, b) {
  a = a[order(1:3), ]
  b = b[order(1:3), ]
  
  asub = a[, -c(1:3)] %>% 
    apply(c(1,2), function(x) ifelse(is.na(x), -1, x))
  bsub = b[, -c(1:3)] %>% 
    apply(c(1,2), function(x) ifelse(is.na(x), -1, x))

  temp = asub != bsub
  x = rowSums(temp) > 0
  a_out = a[x, ]
  b_out = b[x, ]
  
  out = list(a_out, b_out)
  return(out)
}

m = list()
for (i in 1:4) {
  m[[i]] = missings(dd[[i]][, -c(1)], tt[[i]])
}
m

# compare to tall version
tall_yield = sum(tall$YIELD, na.rm=TRUE)
join_yield = sapply(df[, 4:22], sum_na) %>% 
  sum
orig_yield = sapply(dd, function(x) sapply(x[, -c(1:4)], sum_na)) %>% 
  sapply(sum) %>% 
  sum

tall_yield
join_yield
orig_yield

# Clean up tall
tall = subset(tall, !(is.na(YIELD)))

densityplot(~YIELD | YEAR, group=REGION, data=tall, auto.key=TRUE)


#### Export tables ####
w = FALSE
# w = TRUE
if (w) {
  tt = 'IL_ALL_YEARS_TALL_03202019.csv' %>% 
    file.path(dir_out, .)
  write.csv(tall,
            tt, 
            row.names=FALSE)
  
  tt = 'IL_ALL_YEARS_WIDE_03202019.csv' %>% 
    file.path(dir_out, .)
  write.csv(df,
            tt,
            row.names=FALSE)
}
