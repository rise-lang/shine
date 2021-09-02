# libPath <- "~/.rlibs"
# .libPaths(libPath)

usePackage <- function(p) {
   if (!is.element(p, installed.packages()[,1]))
       install.packages(p, repos = "https://cloud.r-project.org/")
   library(p, character.only = TRUE)
}

usePackage("tidyverse")
usePackage("extrafont")
# font_import(prompt=FALSE)
loadfonts()

input <- "bench2.csv"
output <- "bench.pdf"
data <- read_delim(input, " ")

hardwareSeq <- c("Titan Xp", "Tesla K40c")
sizeSeq <- c("small", "large", "mean")
benchSeq <- c("N-Body, NVIDIA", "N-Body, AMD", "MD", "K-Means", "NN", "MRI-Q", "Convolution", "ATAX", "GEMV", "GESUMMV", "MM, NVIDIA", "MM, AMD", "Mean")
data <- data %>%
  select(-minTime, -maxTime) %>%
  group_by(hardware, size) %>%
  separate(kernel, c("generator", "version"), " ", fill = "right")
  
print(data)

nbody <- data %>%
  filter(benchmark == "N-Body") %>%
  mutate(benchmark = paste0(benchmark, ", ", version)) %>%
  select(-version)
  
mriq <- data %>%
  filter(benchmark == "MRI-Q") %>%
  pivot_wider(id_cols=c("hardware", "benchmark", "size", "generator"), names_from=version, values_from=medianTime) %>%
  mutate(medianTime=PhiMag+Q) %>%
  select(-PhiMag, -Q)
  
convolution <- data %>%
  filter(benchmark == "Convolution") %>%
  pivot_wider(id_cols=c("hardware", "benchmark", "size", "generator"), names_from=version, values_from=medianTime) %>%
  mutate(medianTime=X+Y) %>%
  select(-X, -Y)

atax <- data %>%
  filter(benchmark == "GEMV") %>%
  pivot_wider(id_cols=c("hardware", "benchmark", "size", "generator"), names_from=version, values_from=medianTime) %>%
  mutate(benchmark = "ATAX", medianTime =N+T) %>%
  select(-N, -T)

gemv <- data %>%
  filter(benchmark == "GEMV", version == "T") %>%
  select(-version)
  
gesummv <- data %>%
  filter(benchmark == "GEMV", version == "N") %>%
  mutate(benchmark = "GESUMMV", medianTime = 2*medianTime) %>%
  select(-version)

mm <- data %>%
  filter(benchmark == "MM") %>%
  mutate(benchmark = paste0(benchmark, ", ", version)) %>%
  select(-version)
  
rest <- data %>%
  filter(benchmark %in% c("MD", "K-Means", "NN")) %>%
  select(-version)

results <- bind_rows(nbody, mriq, convolution, atax, gemv, gesummv, mm, rest) %>%
  pivot_wider(id_cols=c("hardware", "benchmark", "size"), names_from=generator, values_from=medianTime) %>%
  mutate(relativeTime = original / dpia) %>%
  filter(benchmark != "MM, NVIDIA")

results <- results %>% ungroup() %>%
  add_row(hardware = hardwareSeq, benchmark = "Mean", size = "mean", relativeTime=exp(mean(log(pull(results, relativeTime))))) %>%
  transform(hardware = factor(hardware, hardwareSeq),
            benchmark = factor(benchmark, benchSeq),
            size = factor(size, sizeSeq))
  
print(results)

g <- ggplot(results, aes(x=size, y=relativeTime, fill=size)) +
  geom_bar(colour = "black", position="dodge", stat="identity", show.legend = FALSE) +
  facet_grid(hardware ~ benchmark, scales="free_x", space="free") +
  geom_hline(yintercept = 1) +
  xlab("input size") +
  ylab("relative runtime performance") +
  theme_bw() + theme(
    axis.text.x = element_text(angle = 45, hjust=1),
    #axis.title.x = element_blank(),
    #axis.title.y = element_text(margin = margin(t = 0, r = 15, b = 0, l = 0)),
    #text = element_text(size=14, family="DejaVu Sans")
  ) +
  scale_fill_manual(values = c("#006ddb", "#490092", "#505050"))
ggsave(output, plot = g, width = 36, height = 8, units = "cm")