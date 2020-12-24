library(xml2)
library(ggplot2)

extract_coords <- function(point) {
  x <- as.numeric(attr(point, "cx"))
  y <- as.numeric(attr(point, "cy"))
  size <- as.numeric(attr(point, "r"))
  c(x, y, size)
}

# Lindemann logo
lindemann <- read_xml("Lindemann_logo.svg")

lindemann_points <- as_list(xml_find_first(lindemann, "/*/*[4]"))

lindemann_df <- as.data.frame(t(sapply(lindemann_points, extract_coords))) 

rownames(lindemann_df) <- 1:nrow(lindemann_df)
colnames(lindemann_df) <- c("x", "y", "size")

lindemann_df$y <- 480 - lindemann_df$y

ggplot(lindemann_df, aes(x, y)) +
  geom_point() +
  coord_fixed(1)

# Lordi logo
lordi <- read_xml("Lordi_logo.svg")

lordi_points <- as_list(xml_find_first(lordi, "/*/*[4]"))

lordi_df <- as.data.frame(t(sapply(lordi_points, extract_coords))) 

rownames(lordi_df) <- 1:nrow(lordi_df)
colnames(lordi_df) <- c("x", "y", "size")

lordi_df$y <-  360 - lordi_df$y

ggplot(lordi_df, aes(x, y, size = size / 5)) +
  geom_point() +
  coord_fixed(1)

# Imagine Dragons logo
idragons <- read_xml("Imagine_Dragons_logo.svg")

idragons_points <- as_list(xml_find_first(idragons, "/*/*[4]"))

idragons_df <- as.data.frame(t(sapply(idragons_points, extract_coords))) 

rownames(idragons_df) <- 1:nrow(idragons_df)
colnames(idragons_df) <- c("x", "y", "size")

idragons_df$y <-  97 - idragons_df$y

ggplot(idragons_df, aes(x, y)) +
  geom_point(size = 1) +
  coord_fixed(1)

# Sampsa Astala logo
sampsa_astala <- read_xml("Sampsa_Astala_logo.svg")

sampsa_astala_points <- as_list(xml_find_first(sampsa_astala, "/*/*[4]"))

sampsa_astala_df <- as.data.frame(t(sapply(sampsa_astala_points, extract_coords))) 

rownames(sampsa_astala_df) <- 1:nrow(sampsa_astala_df)
colnames(sampsa_astala_df) <- c("x", "y", "size")

sampsa_astala_df$y <-  480 - sampsa_astala_df$y

ggplot(sampsa_astala_df, aes(x, y)) +
  geom_point(aes(size = factor(size))) +
  coord_fixed(1)

# Rammstein logo
rammstein <- read_xml("Rammstein_logo.svg")

rammstein_points <- as_list(xml_find_first(rammstein, "/*/*[4]"))

rammstein_df <- as.data.frame(t(sapply(rammstein_points, extract_coords))) 

rownames(rammstein_df) <- 1:nrow(rammstein_df)
colnames(rammstein_df) <- c("x", "y", "size")

rammstein_df$y <-  400 - rammstein_df$y

ggplot(rammstein_df, aes(x, -y)) +
  geom_point(aes(size = factor(size))) +
  coord_fixed(1)

# Offsets
lindemann_df$x <- lindemann_df$x - 100
lindemann_df$y <- lindemann_df$y - 30

lordi_df$x <- lordi_df$x + 350
lordi_df$y <- lordi_df$y + 50

idragons_df$x <- idragons_df$x + 300
idragons_df$y <- idragons_df$y + 0

sampsa_astala_df$x <- sampsa_astala_df$x + 950
sampsa_astala_df$y <- sampsa_astala_df$y -50

rammstein_df$x <- rammstein_df$x + 1400
rammstein_df$y <- rammstein_df$y + 20

# Labels for colors
lindemann_df$group <- "lindemann"
lordi_df$group <- "lordi"
idragons_df$group <- "idragons"
sampsa_astala_df$group <- "sampsa_astala"
rammstein_df$group <- "rammstein"

logo_points <- rbind(lindemann_df, lordi_df, idragons_df, sampsa_astala_df, rammstein_df)

logo_points$size <- factor(logo_points$size, levels = c(5, 10, 20), labels = c("small", "medium", "large"))

plots_theme <- theme_bw(base_size = 16, base_family = "PT Sans") +
  theme(line = element_line(size = 1, color ="#333333"),
        axis.ticks = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank())
theme_set(plots_theme)
plots_color <- "#800094"

cover_picture <- ggplot(logo_points, aes(x = x, y = y, size = size)) +
  geom_point(color = plots_color) +
  scale_size_manual(values = c(1, 2, 4), guide = NULL) +
  #scale_color_manual(values = c("#350794", "#948C0F", "#800094", "#0F9422", "#94072A"), guide = NULL) +
  coord_fixed(1) +
  labs(x = "", y = "") +
  # facet_wrap(~group, scales = "fixed") +
  theme(panel.grid = element_blank(),
        strip.background = element_blank(),
        strip.text = element_blank(),
        axis.line = element_line(arrow = arrow(angle = 10, type = "closed")),
        axis.text = element_blank())

ggsave("cover_picture.png", cover_picture, width = unit(12, "cm"), height = unit(6, "cm"))

ggsave()