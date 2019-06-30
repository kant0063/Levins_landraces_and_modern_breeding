library(ggplot2)

dir_in = #path_to_file
dir_in = "/home/patrick/Cloud/Side Projects/Heritable Microbiomes/Data"

df = file.path(dir_in, "Local Adaptation by Year 04012019.csv")
df = read.csv(df, stringsAsFactors=FALSE)

plt = ggplot(df) +
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

write=FALSE
if (write) {
  file_out = paste("Local Adaptation by Year ",
                   Sys.Date(),
                   ".pdf",
                   sep="")
  file_out = file.path(dir_in, file_out)
  
  pdf(file_out, height=4, width=5)
  plt
  dev.off()
}