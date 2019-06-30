library(ggplot2)

dir_in = "/home/patrick/Cloud/Side Projects/Heritable Microbiomes/Data/Figures"
file_in = "PLOT DATA - Local Adaptation by Year 2019-06-25.csv"
pltdf = read.csv(file.path(dir_in, file_in), header=TRUE, stringsAsFactors=FALSE)

mn = mean(pltdf$VALUE)
se = sd(pltdf$VALUE)/sqrt(nrow(pltdf)-1)
# pltdf$YEAR = as.factor(pltdf$YEAR)
  
  
plt = ggplot(pltdf) +
  # scale_fill_manual(values=c('steelblue3', 'tomato2')) +
  ylim(c(0, 1.5)) +
  xlab("Year") +
  ylab(expression(atop("Observed Home Field", 
                       paste("Advantage [Mg ", "ha"^"-1", "]")))) +
  # geom_ribbon(aes(x=YEAR,
  #                 ymin=mn-se,
  #                 ymax=mn+se)) +
  geom_hline(aes(yintercept=mn+se),
             color='#999999',
             linetype='dashed') +
  geom_hline(aes(yintercept=mn-se),
             color="#999999",
             linetype='dashed') +
  geom_errorbar(aes(x=YEAR,
                    ymin=VALUE-ERROR,
                    ymax=VALUE+ERROR),
                width=0.0,
                color="#666666") +
  geom_point(aes(x=YEAR,
                 y=VALUE),
             color='steelblue3',
             size=3) +
  theme_light() #+
  # theme(axis.text.x=element_text(angle=60,
  #                                vjust=0.5))
plt


#### Write to PDF ####
ww = FALSE
# ww = TRUE
if (ww) {
  fout = paste("Fig 4 - Local Adaptation by Year ",
               Sys.Date(),
               ".pdf",
               sep="")
  fout = file.path(dir_in, fout)
  pdf(file=fout, height=2.5, width=4)#, res=300)
  plt
  dev.off()
}
