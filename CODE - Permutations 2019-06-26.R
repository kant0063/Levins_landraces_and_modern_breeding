library(ggplot2)
library(reshape2)
# library(boot)

dir_in = "/home/patrick/Cloud/Side Projects/Heritable Microbiomes/Data/Figures"
expected_in = "PLOT DATA - Permutations Local Adaptation 999x on SITE-YEAR 2019-06-26.csv"
observed_in = "PLOT DATA - Local Adaptation by Year 2019-06-27.csv"
exp = read.csv(file.path(dir_in, expected_in), header=TRUE, stringsAsFactors=FALSE)


obs = read.csv(file.path(dir_in, observed_in), header=TRUE, stringsAsFactors=FALSE)

# Calculate difference observed - expected
exp = t(exp)
diff = apply(exp, 2, function(x) {
  obs[, 'VALUE'] - x
})

pltdf = melt(t(diff))
pltdf$YEAR = gsub("X", "", pltdf$Var2)

plt = ggplot(pltdf) +
  geom_hline(aes(yintercept=0),
             color="#555555") +
  geom_violin(aes(x=YEAR,         # minor intervals
                   y=value),
              fill='steelblue2',
              color='#666666',
              size=0.25,
              draw_quantiles=c(0.05, 0.25, 0.75, 0.95)) +
  geom_violin(aes(x=YEAR,        # major interval - the median
                  y=value),
              alpha=0,           # transparent fill
              color='#444444',
              size=0.5,
              draw_quantiles=c(0.5)) +
  ylab(expression(atop("Observed - Expected", 
                       paste("Home Field Advantage [Mg ", "ha"^"-1", "]")))) +
  xlab("Year") +
  theme_light() +
  theme(axis.text.x=element_text(angle=60,
                                 vjust=0.7))
plt


#### Write to PDF ####
ww = FALSE
# ww = TRUE
if (ww) {
  fout = paste("Fig 5 - Permutation Comparison ",
               Sys.Date(),
               ".pdf",
               sep="")
  fout = file.path(dir_in, fout)
  pdf(file=fout, height=3, width=4)#, res=300)
  plt
  dev.off()
}
