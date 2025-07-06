library(unvotes)
library(dplyr)
library(tidyr)
library(igraph)
library(ggraph)
library(tidygraph)
library(scales)

#Define major economies with bloc grouping
eastern <- c("China", "India", "Russia", "Japan", "South Korea")
western <- c("United States", "Canada", "United Kingdom", "France", "Germany")
targets <- c(eastern, western)

#Load UNGA voting data
data("un_votes")

#Calculate voting similarity
vote_similarity <- function(country1, country2) {
  votes1 <- un_votes %>% filter(country == country1) %>% select(rcid, vote)
  votes2 <- un_votes %>% filter(country == country2) %>% select(rcid, vote)
  
  combined <- inner_join(votes1, votes2, by = "rcid") %>%
    filter(!is.na(vote.x) & !is.na(vote.y)) %>%
    mutate(same_vote = vote.x == vote.y)
  
  if (nrow(combined) < 10) return(NA)
  mean(combined$same_vote)
}

#Createlist with only significant connections
edge_list <- expand.grid(from = targets, to = targets, stringsAsFactors = FALSE) %>%
  filter(from != to) %>%
  rowwise() %>%
  mutate(similarity = vote_similarity(from, to)) %>%
  ungroup() %>%
  filter(!is.na(similarity) & similarity > 0.5)

#Create bloc grouping
nodes <- tibble(
  name = targets,
  bloc = ifelse(name %in% eastern, "Eastern", "Western")
)

g <- graph_from_data_frame(edge_list, directed = FALSE, vertices = nodes)
tg <- as_tbl_graph(g)

node_order <- c(eastern, western)

#Create  layot with spacing
layout <- create_layout(
  tg,
  layout = "linear",
  circular = TRUE,
  sort.by = factor(V(tg)$name, levels = node_order)
)

#Plot with color gradient
p <- ggraph(layout) +
  geom_edge_arc(
    aes(color = similarity),
    width = 1.5,
    strength = 0.15,
    show.legend = TRUE
  ) +
  geom_node_point(
    aes(fill = bloc),
    shape = 21,
    size = 14,
    color = "white",
    stroke = 1.5
  ) +
  geom_node_text(
    aes(label = name),
    size = 4,
    fontface = "bold",
    hjust = "center",
    vjust = "middle"
  ) +
  scale_edge_color_gradientn(
    "Agreement %",
    colors = c("#FF0000", "#FFFF00", "#00FF00"),
    values = rescale(c(0.55, 0.7, 0.85)),
    labels = scales::percent
  ) +
  # Node colors
  scale_fill_manual(
    values = c(Eastern = "#E41A1C", Western = "#377EB8"),
    guide = "none"
  ) +
  labs(
    title = "UNGA Voting Alignment among Major Economies",
    subtitle = "Eastern bloc in red, Western bloc in blue | Agreement >50% shown",
    caption = "Based on UN General Assembly voting records"
  ) +
  theme_void() +
  theme(
    plot.margin = margin(30, 30, 30, 30),
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
    plot.subtitle = element_text(hjust = 0.5, size = 12, color = "gray", 
                                 margin = margin(b = 15)),
    plot.caption = element_text(hjust = 0.5, size = 10, color = "gray"),
    legend.position = "bottom",
    legend.title = element_text(face = "bold", size = 10),
    legend.text = element_text(size = 9),
    legend.key.width = unit(1.5, "cm")
  ) +
  expand_limits(x = c(-1.3, 1.3), y = c(-1.3, 1.3)) +
  coord_cartesian(clip = "off")

# Display the plot
print(p)