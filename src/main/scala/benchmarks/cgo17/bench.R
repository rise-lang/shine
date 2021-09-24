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
# "MM NVIDIA", "MM AMD"
benchSeq <- c("N-Body NVIDIA", "N-Body AMD", "MD", "K-Means", "NN", "MRI-Q", "Conv", "ATAX", "GEMV", "GESUMMV", "MM", "Mean")
data <- data %>%
  select(-minTime, -maxTime) %>%
  group_by(hardware, size) %>%
  separate(kernel, c("generator", "version"), " ", fill = "right")
  
print(data)

nbody <- data %>%
  filter(benchmark == "N-Body") %>%
  mutate(benchmark = paste0(benchmark, " ", version)) %>%
  select(-version)
  
mriq <- data %>%
  filter(benchmark == "MRI-Q") %>%
  pivot_wider(id_cols=c("hardware", "benchmark", "size", "generator"), names_from=version, values_from=medianTime) %>%
  mutate(medianTime=PhiMag+Q) %>%
  select(-PhiMag, -Q)
  
convolution <- data %>%
  filter(benchmark == "Convolution") %>%
  pivot_wider(id_cols=c("hardware", "benchmark", "size", "generator"), names_from=version, values_from=medianTime) %>%
  mutate(benchmark = "Conv", medianTime=X+Y) %>%
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
  filter(benchmark == "MM", version == "AMD") %>%
  # mutate(benchmark = paste0(benchmark, " ", version)) %>%
  select(-version)
  
rest <- data %>%
  filter(benchmark %in% c("MD", "K-Means", "NN")) %>%
  select(-version)

results <- bind_rows(nbody, mriq, convolution, atax, gemv, gesummv, mm, rest) %>%
  pivot_wider(id_cols=c("hardware", "benchmark", "size"), names_from=generator, values_from=medianTime) %>%
  mutate(relativeTime = original / dpia)

results <- results %>% ungroup() %>%
  add_row(hardware = hardwareSeq, benchmark = "Mean", size = "mean", relativeTime=exp(mean(log(pull(results, relativeTime))))) %>%
  add_column(generator="Rise") %>%
  bind_rows(crossing(tibble(hardware = hardwareSeq), tibble(benchmark = head(benchSeq, -1)), tibble(size = head(sizeSeq, -1)), tibble(relativeTime=1, generator="Lift"))) %>%
  add_row(hardware = hardwareSeq, benchmark = "Mean", size = "mean", relativeTime=1, generator="Lift") %>%
  transform(hardware = factor(hardware, hardwareSeq),
            benchmark = factor(benchmark, benchSeq),
            size = factor(size, sizeSeq))
  
print(results)

shift <- 1
t_shift <- scales::trans_new("shift",
                             transform = function(x) { log2(x) + shift },
                             # y - shift = log2(x)
                             inverse = function(y) { (2^(y - shift)) })
                             
g <- ggplot(results, aes(x=size, y=relativeTime, fill=generator)) +
  geom_bar(colour = "black", position="dodge", stat="identity", size=0.2) +
  facet_grid(hardware ~ benchmark, scales="free_x", space="free", labeller = labeller(benchmark = label_wrap_gen(width=10))) +
  geom_hline(yintercept = 1) +
  xlab("input size") +
  scale_y_continuous(name = "relative runtime performance", trans=t_shift, breaks = c(0.5, 1, 2.0), limits = c(0.5, 3)) +
  scale_fill_manual(name = "", values = c("#ffdf4d", "#db6d00")) +
  # scale_fill_manual(name = "", values = c("#006ddb", "#490092", "#505050")) +
  theme_bw() + theme(
    legend.position="top",
    strip.text = element_text(angle = -90, size=10, face="bold"),
    axis.text.x = element_text(angle = -90, size=10, vjust=0.5),
    axis.title.y = element_blank()
  )
ggsave(output, plot = g, width = 16, height = 12, units = "cm")