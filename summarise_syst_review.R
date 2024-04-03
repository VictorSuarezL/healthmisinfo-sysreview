library(googlesheets4)
library(tidyverse)

# googlesheets::gs_auth(new_user = T)

vs_screening <- sel_process <- googlesheets4::read_sheet("1OGuX_llmmwpTstLm4d7SaLx-D6XVeTvfpAIjP5LVbQ8", sheet = 2)

# vs_screening <- sel_process <- googlesheets::gs_key("1OGuX_llmmwpTstLm4d7SaLx-D6XVeTvfpAIjP5LVbQ8") %>%
#   googlesheets::gs_read(ws = 2)

vs_screening %>%
  mutate(REASON = str_replace_all(REASON, pattern = "No focused in social media", "It is internet based")) %>%
  filter(Include == "No") %>%
  count(REASON, sort = T)


# Included Literature -----------------------------------------------------

sel_process <- googlesheets4::read_sheet("1OGuX_llmmwpTstLm4d7SaLx-D6XVeTvfpAIjP5LVbQ8",
                                         sheet = 4, skip = 1) %>%
  mutate(post_misinformative = as.integer(post_misinformative)/100) %>%
  janitor::clean_names() %>%
  mutate(year = as.character(year))

sel_process %>%
  count(coded_topic)
  count(languages)

sel_process <- googlesheets4::read_sheet("1OGuX_llmmwpTstLm4d7SaLx-D6XVeTvfpAIjP5LVbQ8",
                                           sheet = 4,
                                           skip = 1) %>%
    mutate(post_misinformative = as.character(post_misinformative)) %>%
    mutate(post_misinformative = as.double(post_misinformative)/100) %>%
    janitor::clean_names() %>%
    mutate(year = as.character(year))

sel_process %>%
  count(coded_topic)

# Graph -------------------------------------------------------------------

sel_process <- googlesheets4::read_sheet("1OGuX_llmmwpTstLm4d7SaLx-D6XVeTvfpAIjP5LVbQ8", sheet = 4,
                                         skip = 1) %>%
  mutate(post_misinformative = as.character(post_misinformative)) %>%
  mutate(post_misinformative = as.double(post_misinformative)/100) %>%
  janitor::clean_names() %>%
  mutate(year = as.character(year)) %>%
  filter(!is.na(post_misinformative))


du <- sel_process %>%
  filter(social_media == "Twitter and Tumblr") %>%
  mutate(post_misinformative = 13.8/100,
         social_media = "Tumblr",
         year = paste0(year, " (Tumblr)"))

sel_process <- sel_process %>%
  mutate(social_media = if_else(social_media == "Twitter and Tumblr", "Twitter", social_media)) %>%
  bind_rows(du) %>%
  mutate(sm = case_when(social_media %in% c("Twitter",
                                            "Tumblr",
                                            "MySpace") ~ "Microblogging",
                        social_media %in% c("Facebook",
                                            "VK", "WhatsApp") ~ "Social\nNetworks",
                        social_media %in% c("Youtube",
                                            "Pinterest", "Instagram") ~ "Media\nSharing")) %>%
  mutate_at(vars(sq, eq, qual_s, quan_s), ~str_replace_all(., ",", ".")) %>%
  mutate_at(vars(sq, eq, qual_s, quan_s), ~if_else(is.na(.), NA_character_, .))

sel_process %>%
  filter(!is.na(post_misinformative)) %>%
  mutate(sm = as_factor(sm),
         post_misinformative = post_misinformative * 100,
         coded_topic = str_to_title(coded_topic),
         coded_topic = str_replace_all(coded_topic, "No Communicable", "NCD")) %>%
  mutate(coded_topic = case_when(coded_topic == "Drug" ~ "Drugs",
                                 coded_topic == "Eating Disorder" ~ "Eating Disorders",
                                 coded_topic == "NCD" ~ "Noncommunicable Diseases",
                                 coded_topic == "Pandemic" ~ "Pandemics",
                                 coded_topic == "Treatment" ~ "Treatments",
                                 TRUE ~ coded_topic)) %>%
  mutate(authors = paste(cite, year),
         authors = if_else(authors == "Branley 2017", "Branley 2017 (Twitter)", authors)) %>%
  select(authors, post_misinformative, sm, coded_topic) %>%
  as.data.frame() -> data

data %>%
  filter(is.na(sm))

empty_bar <- 2
to_add <- data.frame( matrix(NA, empty_bar*nlevels(data$sm), ncol(data)) )
colnames(to_add) <- colnames(data)
to_add$sm <- rep(levels(data$sm), each=empty_bar)
data <- rbind(data, to_add)
data <- data %>% arrange(sm, post_misinformative)
data$id <- seq(1, nrow(data))

label_data <- data
number_of_bar <- nrow(label_data)
angle <- 90 - 360 * (label_data$id-0.5) /number_of_bar     # I substract 0.5 because the letter must have the angle of the center of the bars. Not extreme right(1) or extreme left (0)
label_data$hjust <- ifelse( angle < -90, 1, 0)
label_data$angle <- ifelse(angle < -90, angle+180, angle)

base_data <- data %>%
  group_by(sm) %>%
  summarize(start=min(id), end=max(id) - empty_bar) %>%
  rowwise() %>%
  mutate(title=mean(c(start, end)))


# prepare a data frame for grid (scales)
grid_data <- base_data
grid_data$end <- grid_data$end[ c( nrow(grid_data), 1:nrow(grid_data)-1)] + 1
grid_data$start <- grid_data$start - 1
grid_data <- grid_data[-1,]

# Make the plot
ggplot(data, aes(x=as.factor(id), y=post_misinformative, fill=coded_topic)) +       # Note that id is a factor. If x is numeric, there is some space between the first bar

  geom_bar(aes(x=as.factor(id), y=post_misinformative, fill=coded_topic), stat="identity", alpha=0.5, colour = "black") +

  # Add a val=100/75/50/25 lines. I do it at the beginning to make sur barplots are OVER it.
  geom_segment(data=grid_data, aes(x = end, y = 80, xend = start, yend = 80), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
  geom_segment(data=grid_data, aes(x = end, y = 60, xend = start, yend = 60), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
  geom_segment(data=grid_data, aes(x = end, y = 40, xend = start, yend = 40), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
  geom_segment(data=grid_data, aes(x = end, y = 20, xend = start, yend = 20), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +

  # Add text showing the value of each 100/75/50/25 lines
  annotate("text", x = rep(max(data$id),4), y = c(20, 40, 60, 80), label = c("20", "40", "60", "80") , color="grey", size=3 , angle=0, fontface="bold", hjust=1) +

  geom_bar(aes(x=as.factor(id), y=post_misinformative, fill=coded_topic), stat="identity", alpha=0.5) +
  ylim(-100,120) +
  theme_minimal() +
  theme(
    legend.position = "right",
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    plot.margin = unit(rep(-1,4), "cm")
  ) +
  labs(fill = "Topic") +
  coord_polar() +
  geom_text(data=label_data, aes(x=id, y=post_misinformative+10, label=authors, hjust=hjust), color="black", fontface="bold",alpha=0.6, size=2.5, angle= label_data$angle, inherit.aes = FALSE ) +

  # Add base line information
  geom_segment(data=base_data, aes(x = start, y = -5, xend = end, yend = -5), colour = "black", alpha=0.8, size=0.6 , inherit.aes = FALSE )  +
  geom_text(data=base_data, aes(x = title, y = -18, label=sm), hjust=c(1,0,0), colour = "black", alpha=0.8, size=4, fontface="bold", inherit.aes = FALSE)+
  scale_fill_brewer(palette = "Set2") -> p


p +
  ggsave()


# Quality Table -----------------------------------------------------------

sel_process <- googlesheets::gs_key("1OGuX_llmmwpTstLm4d7SaLx-D6XVeTvfpAIjP5LVbQ8") %>%
  googlesheets::gs_read(ws = 4, skip = 1) %>%
  mutate(post_misinformative = as.integer(post_misinformative)/100) %>%
  janitor::clean_names()

sel_process %>%
  select(cite, year, coded_topic, social_media, post_misinformative, eq, sq, quan_s, qual_s) %>%
  mutate(quan_qual_s = case_when(quan_s == "0,00" ~ qual_s,
                                 TRUE ~ quan_s)) %>%
  mutate(cite = paste0(cite, " et al."),
         post_misinformative = post_misinformative,
         coded_topic = str_to_title(coded_topic),
         coded_topic = str_replace_all(coded_topic, "No Communicable", "NCD")) %>%
  mutate_at(vars(eq, sq), ~if_else(. == "#DIV/0!", NA_character_, .)) %>%
  mutate_at(vars(eq, sq, quan_qual_s, quan_s), ~str_replace_all(., ",", ".")) %>%
  mutate_at(vars(sq, eq, quan_qual_s), as.double) %>%
  select(-quan_s, -qual_s) %>%
  gt::gt() %>%
  gt::fmt_percent(vars(post_misinformative, eq, sq, quan_qual_s), decimals = 0) %>%
  gt::cols_label(
    cite = "Authors",
    year = "Year",
    # objectives = "Objectives",
    coded_topic = "Topic",
    social_media = "Social Media Platform",
    post_misinformative = "% of misinformative post",
    # authors_conclusions_about_misinformation_prevalence_in_social_media = "Author's conclusions",
    eq = "EQ Score",
    sq = "SQ Score",
    quan_qual_s = "GQ Score"
  ) %>%
  gt::as_rtf() -> rtf

fileConn<-file("output_table_a.rtf")
writeLines(rtf, fileConn)
close(fileConn)


sel_process %>%
  select(cite, year, res_objectives, coded_topic, social_media, post_misinformative, res_conclusion, eq, sq, quan_s, qual_s) %>%
  mutate(quan_qual_s = if_else(is.na(quan_s), qual_s, quan_s)) %>%
  mutate(cite = paste0(cite, " et al."),
         post_misinformative = post_misinformative,
         coded_topic = str_to_title(coded_topic),
         coded_topic = str_replace_all(coded_topic, "No Communicable", "NCD")) %>%
  mutate_at(vars(eq, sq), ~if_else(. == "#DIV/0!", NA_character_, .)) %>%
  mutate_at(vars(eq, sq, quan_qual_s, quan_s), ~str_replace_all(., ",", ".")) %>%
  mutate_at(vars(sq, eq, quan_qual_s), as.double) %>%
  select(-quan_s, -qual_s, -eq, -sq, -quan_qual_s) %>%
  gt::gt() %>%
  gt::fmt_percent(vars(post_misinformative), decimals = 0) %>%
  gt::cols_label(
    cite = "Authors",
    year = "Year",
    res_objectives = "Objectives",
    coded_topic = "Topic",
    social_media = "Social Media Platform",
    post_misinformative = "% of misinformative post",
    res_conclusion = "Author's conclusions",
    # eq = "EQ Score",
    # sq = "SQ Score",
    # quan_qual_s = "GQ Score"
  ) %>%
  gt::as_rtf() -> rtf

fileConn<-file("output.rtf")
writeLines(rtf, fileConn)
close(fileConn)

str_to_sentence("this is. a test.")


# New chart ---------------------------------------------------------------


nchart <- googlesheets4::read_sheet("1OGuX_llmmwpTstLm4d7SaLx-D6XVeTvfpAIjP5LVbQ8", sheet = 4,
                                    skip = 1) %>%
  mutate(post_misinformative = as.character(post_misinformative)) %>%
  mutate(post_misinformative = as.double(post_misinformative)/100) %>%
  janitor::clean_names() %>%
  mutate(year = as.character(year)) %>%
  filter(!is.na(post_misinformative))


nchart %>%
  select(coded_methods, post_misinformative) %>%
  filter(is.na(post_misinformative)) %>%
  view()

# nchart %>%
#   select(Year, post_misinformative)
#
# nchart %>%
#   group_by(CODED_TOPIC) %>%
#   count(social_media, sort = T) %>%
#   gt::gt()

du <- nchart %>%
  filter(social_media == "Twitter and Tumblr") %>%
  mutate(post_misinformative = 13.8/100,
         social_media = "Tumblr",
         year = paste0(year, " (Tumblr)"))

sel_process <- nchart %>%
  mutate(social_media = if_else(social_media == "Twitter and Tumblr", "Twitter", social_media)) %>%
  bind_rows(du) %>%
  mutate(sm = case_when(social_media %in% c("Twitter",
                                            "Tumblr",
                                            "MySpace") ~ "Microblogging",
                        social_media %in% c("Facebook",
                                            "VK", "WhatsApp") ~ "Social\nNetworks",
                        social_media %in% c("Youtube",
                                            "Pinterest", "Instagram") ~ "Media\nSharing")) %>%
  mutate_at(vars(sq, eq, qual_s, quan_s), ~str_replace_all(., ",", ".")) %>%
  mutate_at(vars(sq, eq, qual_s, quan_s), ~if_else(is.na(.), NA_character_, .))

sel_process %>%
  filter(!is.na(post_misinformative)) %>%
  # filter(!is.na(authors))
  mutate(sm = as_factor(sm),
         post_misinformative = post_misinformative * 100,
         coded_topic = str_to_title(coded_methods),
         coded_topic = str_replace_all(coded_topic, "No Communicable", "NCD")) %>%
  mutate(coded_topic = case_when(coded_topic == "Drug" ~ "Drugs",
                                 coded_topic == "Eating Disorder" ~ "Eating Disorders",
                                 coded_topic == "NCD" ~ "Noncommunicable Diseases",
                                 coded_topic == "Pandemic" ~ "Pandemics",
                                 coded_topic == "Treatment" ~ "Treatments",
                                 TRUE ~ coded_topic)) %>%
  mutate(authors = paste(cite, year),
         authors = if_else(authors == "Branley 2017", "Branley 2017 (Twitter)", authors)) %>%
  select(authors, post_misinformative, sm, coded_topic) %>%
  as.data.frame() -> data

data %>%
  filter(is.na(authors))

empty_bar <- 2
to_add <- data.frame( matrix(NA, empty_bar*nlevels(data$sm), ncol(data)) )
colnames(to_add) <- colnames(data)
to_add$sm <- rep(levels(data$sm), each=empty_bar)
data <- rbind(data, to_add)
data <- data %>% arrange(sm, post_misinformative)
data$id <- seq(1, nrow(data))

label_data <- data
number_of_bar <- nrow(label_data)
angle <- 90 - 360 * (label_data$id-0.5) /number_of_bar     # I substract 0.5 because the letter must have the angle of the center of the bars. Not extreme right(1) or extreme left (0)
label_data$hjust <- ifelse( angle < -90, 1, 0)
label_data$angle <- ifelse(angle < -90, angle+180, angle)

base_data <- data %>%
  group_by(sm) %>%
  summarize(start=min(id), end=max(id) - empty_bar) %>%
  rowwise() %>%
  mutate(title=mean(c(start, end)))


# prepare a data frame for grid (scales)
grid_data <- base_data
grid_data$end <- grid_data$end[ c( nrow(grid_data), 1:nrow(grid_data)-1)] + 1
grid_data$start <- grid_data$start - 1
grid_data <- grid_data[-1,]

# Make the plot
ggplot(data, aes(x=as.factor(id), y=post_misinformative, fill=coded_topic)) +       # Note that id is a factor. If x is numeric, there is some space between the first bar

  geom_bar(aes(x=as.factor(id), y=post_misinformative, fill=coded_topic), stat="identity", alpha=0.5, colour = "black") +

  # Add a val=100/75/50/25 lines. I do it at the beginning to make sur barplots are OVER it.
  geom_segment(data=grid_data, aes(x = end, y = 80, xend = start, yend = 80), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
  geom_segment(data=grid_data, aes(x = end, y = 60, xend = start, yend = 60), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
  geom_segment(data=grid_data, aes(x = end, y = 40, xend = start, yend = 40), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
  geom_segment(data=grid_data, aes(x = end, y = 20, xend = start, yend = 20), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +

  # Add text showing the value of each 100/75/50/25 lines
  annotate("text", x = rep(max(data$id),4), y = c(20, 40, 60, 80), label = c("20", "40", "60", "80") , color="grey", size=3 , angle=0, fontface="bold", hjust=1) +

  geom_bar(aes(x=as.factor(id), y=post_misinformative, fill=coded_topic), stat="identity", alpha=0.5) +
  ylim(-100,120) +
  theme_minimal() +
  theme(
    legend.position = "right",
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    plot.margin = unit(rep(-1,4), "cm")
  ) +
  labs(fill = "Methods") +
  coord_polar() +
  geom_text(data=label_data, aes(x=id, y=post_misinformative+10, label=authors, hjust=hjust), color="black", fontface="bold",alpha=0.6, size=2.5, angle= label_data$angle, inherit.aes = FALSE ) +

  # Add base line information
  geom_segment(data=base_data, aes(x = start, y = -5, xend = end, yend = -5), colour = "black", alpha=0.8, size=0.6 , inherit.aes = FALSE )  +
  geom_text(data=base_data, aes(x = title, y = -18, label=sm), hjust=c(1,0,0), colour = "black", alpha=0.8, size=4, fontface="bold", inherit.aes = FALSE)+
  scale_fill_brewer(palette = "Set2") -> p


p +
  ggsave()
