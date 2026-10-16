#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#         INTERACTIONS BETWEEN PIKA AND INVERTEBRATE HERBIVORY
#                   Isabel C Barrio (isabel@lbhi.is)
#               based on old R scripts from Kristen Peck
#                        last update 16.06.2026
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# libraries ---- 
library(readxl)          # to import data directly from Excel
library(writexl)         # to write out Excel files
library(openxlsx)        # to write out Excel files and format tables
library(tidyverse)
library(nlme)
library(lme4)
library(lmerTest)
library(emmeans)         # calculate estimated marginal means
library(performance)     # calculate intraclass correlation and R2
library(patchwork)       # combine plots into multipanel figure


# load and tidy up data ---- 
# data on haypile contents (= used by pikas)
haypiles <- read_excel("data/pika_invert_herbivory.xlsx", sheet = "haypiles_ORIGINAL") %>% 
              # fix haypile names
              mutate(haypileID = case_when(Location == "E BB 17 W3 N25" ~ "E BB17 W3 N25",
                                           Location == "ZSAME AS CC13 _____E CC14 S35 W1" ~ "E CC14 S35 W1",
                                           TRUE ~ Location)) %>% 
              select(- `Location`, -`Talus area`, -`Name in database? 0=no, 1=yes`, -`Date taken`) %>% 
              # data are stored with separate rows for damaged/undamaged plant material
              pivot_longer(-c(haypileID, Category), names_to = "species.plant", values_to = "weight", 
                           values_drop_na = TRUE) %>% 
              mutate(damage = recode(Category, `Undamaged weight (g)` = "undamaged", `Damaged weight (g)` = "damaged"),
                     species.plant = tolower(gsub(" ", "", species.plant)),
                     # manually fix species names
                     species = case_when(species.plant == "salsp." ~ "salsp",
                                         species.plant == "lich/moss" ~ "lich_moss",
                                         species.plant %in% c("assortedforbs", "un-id'dforbs") ~ "forbs",
                                         species.plant == "vacculi" ~ "vaculi",
                                         species.plant == "potbiflo" ~ "potbif",
                                         species.plant == "potvillosa" ~ "potvil",
                                         species.plant %in% c("dryseedhead", "dryas") ~ "dryas",
                                         species.plant == "vaccvit-id" ~ "vacvit",
                                         species.plant == "saxbronch" ~ "saxbro",
                                           TRUE ~ species.plant),
                     index = "use") %>% # indicate that these values are for use
              select(haypileID, index, species, damage, weight) %>%
              # make sure we have one observation per species in each haypile
                group_by(haypileID, index, species, damage) %>% 
                  summarise(weight = sum(weight), .groups = "drop_last") %>% ungroup() %>% 
                group_by(haypileID) %>% 
                  mutate(total_weight = sum(weight)) %>% ungroup() %>% 
              # calculate the proportion of each species in each haypile sample
              mutate(value = round(weight/total_weight, 6)) %>% 
              select(haypileID, index, species, damage, value, total_weight)

haypiles %>% distinct(species) # 25 plant species/groups


# data on point intercept surveys of meadow (= available to pikas)
point_intercept_long <- read_excel("data/pika_invert_herbivory.xlsx", sheet = "point_intercept_ORIGINAL") %>% 
  # remove summary rows (sum of total hits added at the end of each haypileID)
  filter(!is.na(date)) %>% 
  # create long string unique to each point intercept frame
  mutate(quadratID = paste0(haypileID, "_", dist_from_haypile, "_", 
                            angle_from_haypile, "_", distance_from_talus)) %>% 
  select(-dist_from_haypile, -angle_from_haypile, -distance_from_talus, -date) %>% 
  # calculate total number of intercepts per quadrat before cleaning up
  group_by(haypileID) %>%
    mutate(n_intercepts = n_distinct(paste(quadratID, point))) %>% ungroup() %>% 
  # make data longer to clean up
  pivot_longer(-c(haypileID, quadratID, point, n_intercepts), names_to = "species.plant", values_to = "value", 
               values_drop_na = TRUE) %>% 
      mutate(species.plant = tolower(gsub(" ", "", species.plant)), 
             damage = ifelse(grepl("\\.dam$", species.plant), "damaged", "undamaged"), 
             species = sub("\\.dam$", "", species.plant),
             # manually fix species names
             species = case_when(species == "gentprost" ~ "genpro",
                                 species == "castelleja" ~ "castilleja",
                                 species %in% c("asteraceaesp.", "asteraceae") ~ "asteraceae",
                                 species == "saxbronch" ~ "saxbro",
                                 species %in% c("oxydig", "ocydig") ~ "oxydig",
                                           TRUE ~ species)) %>% 
             select(haypileID, quadratID, species, damage, point, value, n_intercepts) 

point_intercept <- point_intercept_long %>%
             # calculate number of hits per haypile and cover
             group_by(haypileID, species, damage, n_intercepts) %>%
                summarise(hits = sum(value, na.rm = TRUE), .groups = "drop") %>% ungroup() %>% 
             mutate(value = round(hits/n_intercepts, 6),
                    index = "avail") %>% # indicate that these values are for availability
             select(haypileID, index, species, damage, value)

point_intercept %>% distinct(species) # 54 plant species/groups

# location of point intercept quadrats - how far from talus edge?
read_excel("data/pika_invert_herbivory.xlsx", sheet = "point_intercept_ORIGINAL") %>%
  filter(!is.na(date)) %>% 
  # create long string unique to each point intercept frame
  mutate(quadratID = paste0(haypileID, "_", dist_from_haypile, "_", 
                            angle_from_haypile, "_", distance_from_talus)) %>%
  group_by(quadratID) %>% 
  summarise(dist = first(distance_from_talus), .groups = "drop") %>% 
  filter(!dist == "?") %>% 
  summarise(n = n_distinct(quadratID),
            mean = mean(as.numeric(dist)),
            se = sd(as.numeric(dist))/sqrt(n),
            min = min(as.numeric(dist)),
            max = max(as.numeric(dist)))
  

# check how many haypiles are included in each dataset
point_intercept %>% distinct(haypileID) %>% print(n=30) # 30 haypiles
haypiles %>% distinct(haypileID) %>% print(n=35)        # 31 haypiles; drop E CC13 N10 E15


# combine use and availability datasets
use_avail <- haypiles %>% rename(use = value) %>%
  select(haypileID, species, damage, use) %>%
  full_join(point_intercept %>%
              rename(avail = value) %>%
                select(haypileID, species, damage, avail),
            by = c("haypileID", "species", "damage")) %>% 
  # check that information is available in both datasets (haypiles and point_intercept)
  # e.g. there was one more haypile in haypiles than in point intercept
  group_by(haypileID) %>% 
    mutate(total_use = sum(use, na.rm = T),
           total_avail = sum(avail, na.rm = T),
           completeness = case_when(total_use == 0 | total_avail == 0 ~ "incomplete",
                                    TRUE ~ "complete")) %>% ungroup() %>% 
  filter(!completeness == "incomplete") %>%  # remove haypiles with incomplete information
    select(-total_use, -total_avail, -completeness) 

# keep only species that are used by at least 10 pikas and for which there are observations of damage
keep_species <- use_avail %>%
  group_by(species) %>%
    summarise(n_pikas = n_distinct(haypileID[use > 0]), # how many pikas used each species (irrespective of damage)?
              n_damaged = sum(damage == "damaged" & use > 0, na.rm = T), .groups = "drop") %>%
  filter(n_pikas >= 10, n_damaged >= 5) %>%
    pull(species)

use_avail_subset <- use_avail %>% 
    filter(species %in% keep_species) %>% 
    # correct use values: NAs should be zero (no use)
        mutate(use = case_when(is.na(use) ~ 0, TRUE ~ use)) 

# what to do with the NAs in availability is a bit trickier
# check how many cases: %>% filter(is.na(avail) & use > 0) # 88 observations out of 261 (34%)
# check the distribution of NAs in availability across species (this info will go to Table S3)
TableS3 <- use_avail_subset %>%
  group_by(species) %>%
  summarise(n = n(),
            n_NA = sum(is.na(avail)),
            prop_NA = n_NA / n) 
write_xlsx(TableS3, "data/TableS3_NAs.xlsx", col_names = TRUE, format_headers = TRUE)


# interesting patterns: Dryas and Salix spp have only 8% NAs
# suggesting that they are available in the vegetation and commonly used by pikas
# in turn, Polygonum bistorta, Salix reticulata and Vaccinium uliginosum
# are frequently used but not captured by the point intercept. This could
# indicate that they are patchily distributed and poorly sampled by point intercept

# sensitivity analyses ----
# in any case, we could assign a very low availability to those cases 
# (pikas actually used the species, so it has to be available)
# since the sampling resolution is 100 or 200 intercepts, the minimum detectable cover 
# is 0.01 or 0.005, but we can check the influence of this choice in the analyses
# doing a sensitivity analysis

# sensitivity analysis for the non-detected availability
fit_model_avail_fix <- function(avail_c){
  dat <- use_avail_subset %>%
            mutate(avail2 = case_when(is.na(avail) ~ avail_c, 
                                      avail == 0 ~ avail_c,
                                      TRUE ~ avail),
           select.ratio = use / avail2,
           Lselect = log(select.ratio + 0.01))
  lme(Lselect ~ damage * species, random = ~1|haypileID, data = dat)
}

models <- list(pc_0.001 = fit_model_avail_fix(0.001),
               pc_0.005 = fit_model_avail_fix(0.005),
               pc_0.01  = fit_model_avail_fix(0.01))

    # compare fixed effects
    lapply(models, anova)
    
    # compare p-value of the interaction
    sapply(models, function(m)
      anova(m)["damage:species","p-value"])
    
    # compare estimated effects (emmeans)
    lapply(models, function(m)
      emmeans(m, ~ damage | species))


# one last check: 
# run the model without avail = NA
use_avail_noNAs <- use_avail_subset %>% 
  filter(!is.na(avail) & !avail == 0) %>% 
  mutate(Lselect = log((use/avail) + 0.01))

model_interaction_noNAs <- lme(Lselect ~ damage * species, 
                               random = ~1|haypileID, data = use_avail_noNAs)
anova(model_interaction_noNAs)


# the other decision we have to make is about the constant to be added to the log
# we can again run sensitivity analysis to see how that choice affects the results
fit_model_log <- function(k){
  dat <- use_avail_subset %>%
    mutate(avail2 = case_when(is.na(avail) ~ 0.001, 
                                      avail == 0 ~ 0.001,
                                      TRUE ~ avail),
           select.ratio = use / avail2,
           Lselect = log(select.ratio + k))
  nlme::lme(Lselect ~ damage * species, random = ~1|haypileID,
    method = "REML", data = dat)
}

models <- list(k0.1   = fit_model_log(0.1),
                k0.01  = fit_model_log(0.01),
                k0.001 = fit_model_log(0.001))

    # compare fixed effects
    lapply(models, anova)
    
    # compare p-value of the interaction
    sapply(models, function(m)
      anova(m)["damage:species","p-value"])
    
    # compare estimated effects (emmeans)
    lapply(models, function(m)
      emmeans(m, ~ damage | species))


    
# results are robust to these choices, so we're good - we will use 0.001 for avail
# and add 0.01 to the selection ratio
use_avail_data <- use_avail_subset %>%
    mutate(avail2 = case_when(is.na(avail) ~ 0.001, 
                              avail == 0 ~ 0.001,
                                TRUE ~ avail),
           select.ratio = use / avail2,
           Lselect = log(select.ratio + 0.01))




# models ----
model_interaction = lme(Lselect ~ damage*species, random = ~1|haypileID, 
                        method = "REML", data = use_avail_data)
  summary(model_interaction)
  anova(model_interaction) # species*damage (0.312) 
    plot(model_interaction) 

emmeans(model_interaction, pairwise ~ damage | species) 
  # since the interaction is not significant, we need to treat these results carefully


model_additive = lme(Lselect ~ damage + species, random = ~1|haypileID, 
                     method = "REML", data = use_avail_data)
  summary(model_additive)
  anova(model_additive)

emmeans(model_additive, pairwise ~ species) # identify which species differ in selection
emmeans(model_additive, ~ damage) # report estimated marginal means for damage


# check model assumptions
ER2 = residuals(model_additive, type="normalized"); par(mfrow = c(1,2)); plot(model_additive)
  boxplot(ER2 ~ use_avail_subset$species); abline(0, 0, col =2 )
  boxplot(ER2 ~ use_avail_subset$damage); abline(0, 0, col = 2)

# other residual checks: distribution of random intercepts
  qqnorm(ranef(model_additive)[[1]]); qqline(ranef(model_additive)[[1]])
  # with only 30 haypiles normality may not be perfect, but gross departures can be informative.

# inspect residuals separately for each species × damage combination:
ggplot(data.frame(resid = resid(model_additive),
                  species = use_avail_subset$species,
                  damage = use_avail_subset$damage),
      aes(species, resid)) +
  geom_boxplot() +
  facet_wrap(~damage)
  # species seem to have similar variances, so we're good :)


## intraclass correlation (ICC) - how correlated are observations from the same pika
# ICC = variance among pikas / variance among pikas + residual variance
VarCorr(model_interaction)

vc <- VarCorr(model_interaction)
  var_haypile <- as.numeric (vc[1, "Variance"])
  var_resid <- as.numeric(vc[nrow(vc), "Variance"])
  (ICC <- var_haypile / (var_haypile + var_resid))

# ICC shows the variance attributable to differences among haypiles 
# (the rest up to 1 is the variance within haypiles)
  
# alternatively we can use the function icc in the performance package
icc(model_interaction)  


# we can also calculate marginal and conditional R2 (Nakagawa & Schielzeth, 2013)
# also with the performance package
r2_nakagawa(model_interaction)

# marginal R2 is the variance explained by the fixed effects only
# conditional R2 is the variance explained by fixed and random effects

# in our case marginal R2 0.242 --> 24% explained by species*damage
# conditional R2 0.298 --> about 5% explained by the random effect (haypileID)


# Figure 1 ----
# plot raw values and estimated marginal means
emm <- as.data.frame(emmeans(model_interaction, ~ species*damage))          

n <- use_avail_data %>%
  group_by(species, damage) %>%
  summarise(n = n_distinct(haypileID), .groups = "drop")

emm <- left_join(emm, n, by = c("species", "damage"))


# plot
Fig1 <- ggplot(use_avail_data, aes(x = species, y = Lselect, colour = damage)) +
  # horizontal reference line
  geom_hline(yintercept = 0, linetype = "solid") +
  # raw data points
  geom_jitter(aes(group = damage), colour = "grey70", alpha = 0.5,
              size = 2, position = position_dodge(width = 0.5)) +
  # estimated marginal means
  geom_point(data = emm, aes(y = emmean), size = 4,
    position = position_dodge(width = 0.5)) +
  # EMM confidence intervals
  geom_errorbar(data = emm,
    aes(y = emmean, ymin = lower.CL, ymax = upper.CL),
    width = 0, linewidth = 0.8, position = position_dodge(width = 0.5)) +
  # sample sizes
  geom_text(data = emm,
    aes(y = min(use_avail_data$Lselect) - 0.5, label = paste0("n=", n), group = damage), colour = "black",
    position = position_dodge(width = 0.5), 
    size = 3, show.legend = FALSE) +
  scale_colour_manual(values = c("damaged"   = "#482878",
                                 "undamaged" = "#21908CFF")) +
  scale_x_discrete(labels = c("dryas" = "Dryas\noctopetala",
                             "polbis" = "Polygonum\nbistorta",
                             "polviv" = "Polygonum\nviviparum",
                             "salret" = "Salix\nreticulata",
                             "salsp" = "Salix sp.",
                             "vaculi" = "Vaccinium\nuliginosum")) +
  labs(x = NULL,
       y = "Log selection ratio",
       colour = "Leaf damage") +
  theme_bw() +
    theme(axis.text.x = element_text(face = "italic"),
          legend.position = "bottom")
  
ggsave("figures/Figure1.png", plot = Fig1, width = 8, height = 4, dpi = 300)


# alternatively we could have violin plots
ggplot(use_avail_data, aes(x = species, y = Lselect, fill = damage)) +
  # horizontal reference line
  geom_hline(yintercept = 0, linetype = "solid") +
  geom_violin(position = position_dodge(width = 0.9),
              trim = FALSE, alpha = 0.7) +
  geom_boxplot(width = 0.15, position = position_dodge(width = 0.9),
               outlier.shape = NA) +
  scale_fill_manual(values = c("damaged"   = "#482878",
                                 "undamaged" = "#21908CFF")) +
  labs(y = "Log selection ratio",
       x = "Species",
       fill = "Damage") +
  theme_bw()



# Figure S1 ----
# species effect on Lselect
emm_species <- as.data.frame(emmeans(model_additive, ~ species))          

n_species <- use_avail_data %>%
  group_by(species) %>%
  summarise(n = n_distinct(haypileID), .groups = "drop")

emm_species <- left_join(emm_species, n_species, by = c("species"))


# plot
FigS1_species <- ggplot(use_avail_data, aes(x = species, y = Lselect)) +
  # horizontal reference line
  geom_hline(yintercept = 0, linetype = "solid") +
  # raw data points
  geom_jitter(colour = "grey70", alpha = 0.5,
              size = 2, width = 0.15) +
  # estimated marginal means
  geom_point(data = emm_species, aes(y = emmean), size = 4, colour = "black") +
  # EMM confidence intervals
  geom_errorbar(data = emm_species,
    aes(y = emmean, ymin = lower.CL, ymax = upper.CL),
    width = 0, linewidth = 0.8) +
  # sample sizes
  geom_text(data = emm_species,
    aes(y = min(use_avail_data$Lselect) - 0.5, label = paste0("n=", n)), colour = "black",
    size = 3) +
  scale_x_discrete(labels = c("dryas" = "Dryas\noctopetala",
                             "polbis" = "Polygonum\nbistorta",
                             "polviv" = "Polygonum\nviviparum",
                             "salret" = "Salix\nreticulata",
                             "salsp" = "Salix sp.",
                             "vaculi" = "Vaccinium\nuliginosum")) +
  labs(x = NULL,
       y = "Log selection ratio",
       title = "a) Species effect") +
  theme_bw() +
    theme(axis.text.x = element_text(face = "italic"),
          legend.position = "bottom")

# damage effect on Lselect
emm_damage <- as.data.frame(emmeans(model_additive, ~ damage))          

n_damage <- use_avail_data %>%
  group_by(damage) %>%
  summarise(n = n_distinct(haypileID), .groups = "drop")

emm_damage <- left_join(emm_damage, n_damage, by = c("damage"))

FigS1_damage <- ggplot(use_avail_data, aes(x = damage, y = Lselect)) +
  # horizontal reference line
  geom_hline(yintercept = 0, linetype = "solid") +
  # raw data points
  geom_jitter(colour = "grey70", alpha = 0.5,
              size = 2, width = 0.15) +
  # estimated marginal means
  geom_point(data = emm_damage, aes(y = emmean), size = 4, colour = "black") +
  # EMM confidence intervals
  geom_errorbar(data = emm_damage,
    aes(y = emmean, ymin = lower.CL, ymax = upper.CL),
    width = 0, linewidth = 0.8) +
  # sample sizes
  geom_text(data = emm_damage,
    aes(y = min(use_avail_data$Lselect) - 0.5, label = paste0("n=", n)), colour = "black",
    size = 3) +
  labs(x = NULL,
       y = "Log selection ratio",
       title = "b) Damage effect") +
  theme_bw() +
    theme(legend.position = "none")


FigS1 <- FigS1_species + FigS1_damage +
          plot_layout(widths = c(3, 1)) +
          plot_annotation(tag_levels = "a")


ggsave("figures/FigureS1.png", plot = FigS1, width = 10, height = 5, dpi = 300)



# Table S1 ----
# overview of haypile data
# drop extra haypile and rename haypiles to shorter names
haypiles_30 <- haypiles %>% 
  filter(!haypileID == "E CC13 N10 E15")
haypile_lookup <- tibble(haypileID = unique(haypiles_30$haypileID),
  HP = paste0("HP", seq_along(unique(haypiles_30$haypileID))))

haypile_summary <- haypiles_30 %>%
  # add shorter haypile names
  left_join(haypile_lookup, by = "haypileID") %>% 
  # make sure order is correct
    mutate(HP = factor(HP, levels = haypile_lookup$HP)) %>% 
  # pool species into broader groups
  mutate(species_group = case_when(species == "dryas"  ~ "Dryas octopetala",
                                   species == "polbis" ~ "Polygonum bistorta",
                                   species == "polviv" ~ "Polygonum viviparum",
                                   species == "salret" ~ "Salix reticulata",
                                   species == "salsp"  ~ "Salix spp.",
                                   species == "vaculi" ~ "Vaccinium uliginosum",
                                   species %in% c("aneparv","pedic","potbif","potvil",
                                        "oxynig","saxbro","antenn","parrya",
                                        "stellon","camp","oxydyg","artarc",
                                        "astragalus", "forbs") ~ "Other forbs",
                                   species == "graminoids" ~ "Graminoids",
                                   species == "lich_moss"  ~ "Lichens and mosses",
                                   species == "vacvit"     ~ "Vaccinium vitis-idaea",
                                   species == "petfri"     ~ "Petasites frigidus",
                                   species == "castet" ~ "Cassiope tetragona",
                                    TRUE ~ "Other"))  # there should be no "Other" ;)

haypile_summary %>% distinct(species_group) # 12 groups

haypile_summary_table <- haypile_summary %>%
  # calculate proportional biomass in haypile (irrespective of damage)
  group_by(HP, species_group) %>%
    summarise(prop_weight = sum(value), .groups = "drop") %>% 
  # make wider table
  pivot_wider(names_from = HP, values_from = prop_weight, values_fill = 0) %>% 
  # order rows
  mutate(species_group = factor(species_group,
                         levels = c("Dryas octopetala", "Polygonum bistorta", 
                                    "Polygonum viviparum", "Salix reticulata", 
                                    "Salix spp.", "Vaccinium uliginosum", 
                                    "Vaccinium vitis-idaea", "Cassiope tetragona", 
                                    "Petasites frigidus", "Other forbs", 
                                    "Graminoids", "Lichens and mosses"))) %>%
  arrange(species_group)

# calculate the proportion damaged and extract total weight
damage_row <- haypile_summary %>% 
  group_by(HP) %>%
  summarise(prop_damaged = sum(value[damage == "damaged"]), .groups = "drop") %>%
    #summarise(mean = mean(prop_damaged), se = sd(prop_damaged)/sqrt(n()), min = min(prop_damaged), max = max(prop_damaged))
  pivot_wider(names_from = HP,
              values_from = prop_damaged) %>%
  mutate(species_group = "Proportion damaged") %>%
  relocate(species_group)

biomass_row <- haypile_summary %>% 
  group_by(HP) %>%
  summarise(total_biomass = first(total_weight), .groups = "drop") %>%
    #summarise(mean = mean(total_biomass), se = sd(total_biomass)/sqrt(n()), min = min(total_biomass), max = max(total_biomass))
  pivot_wider(names_from = HP,
              values_from = total_biomass) %>%
  mutate(species_group = "Total biomass (g)") %>%
  relocate(species_group)

# combine all
final_table <- bind_rows(haypile_summary_table, damage_row, biomass_row) %>%
  # round to three decimals
  mutate(across(-species_group, ~ round(.x, 3)))

# create a damage-indicator matrix for grey shading
damage_present <- haypile_summary %>% 
  group_by(HP, species_group) %>%
    summarise(damage_present = any(damage == "damaged" & value > 0), .groups = "drop") %>% 
  # make wider table
  pivot_wider(names_from = HP, values_from = damage_present, values_fill = 0) %>% 
  # order rows
  mutate(species_group = factor(species_group,
                         levels = c("Dryas octopetala", "Polygonum bistorta", 
                                    "Polygonum viviparum", "Salix reticulata", 
                                    "Salix spp.", "Vaccinium uliginosum", 
                                    "Vaccinium vitis-idaea", "Cassiope tetragona", 
                                    "Petasites frigidus", "Other forbs", 
                                    "Graminoids", "Lichens and mosses"))) %>%
  arrange(species_group)


# export to Excel applying the conditional formatting :)
wb <- createWorkbook()
  addWorksheet(wb, "Summary")
  addWorksheet(wb, "Damage")

writeData(wb, "Summary", final_table)
writeData(wb, "Damage", damage_present)

# create grey fill style
grey_style <- createStyle(fgFill = "#D9D9D9")

# apply cell shading
for(i in seq_len(nrow(damage_present))) {
  for(j in 2:ncol(damage_present)) {
    if(damage_present[[j]][i]) {
      addStyle(wb, sheet = "Summary",
        style = grey_style, rows = i + 1, cols = j,
        gridExpand = FALSE, stack = TRUE)
    }
  }
}

# make headers bold
header_style <- createStyle(textDecoration = "bold")

addStyle(wb, "Summary",
  header_style, rows = 1,
  cols = 1:ncol(final_table),
  gridExpand = TRUE)

# save workbook
saveWorkbook(wb, "data/TableS1_haypile_summary.xlsx", overwrite = TRUE)
    

# Table S2 ----
# overview of point intercept data

# calculate damage per point - for each point, was at least one damaged plant recorded?
point_damage <- point_intercept_long %>%
             # calculate the proportion of damage per haypile (irrespective of species)
             group_by(haypileID, quadratID, point, n_intercepts) %>%
              summarise(damaged_point = any(damage == "damaged" & value > 0),
                .groups = "drop") %>% 
             # calculate proportion of damaged intercepts
             group_by(haypileID) %>%
                summarise(n_intercepts = first(n_intercepts),
                          n_damaged = sum(damaged_point),
                          prop_damaged = n_damaged / n_intercepts,
                          .groups = "drop")  
              #summarise(mean = mean(prop_damaged), se = sd(prop_damaged)/sqrt(n()), min = min(prop_damaged), max = max(prop_damaged))
  
# create shorter haypile names as above
point_lookup <- tibble(haypileID = unique(point_intercept_long$haypileID),
  HP = paste0("HP", seq_along(unique(point_intercept_long$haypileID))))

# create conversion table for species names
species_lookup <- tibble::tribble(
  ~species, ~species_name,
  "dryas", "Dryas octopetala",
  "polbis", "Polygonum bistorta",
  "polviv", "Polygonum viviparum",
  "salret", "Salix reticulata",
  "salsp", "Salix spp.",
  "vaculi", "Vaccinium uliginosum",

  "aneparv", "Anemone parviflora",
  "antmon", "Antennaria sp.",
  "arctagrostis", "Arctagrostis latifolia",
  "artarc", "Artemisia sp.",
  "camplas", "Campanula lasiocarpa",
  "carex", "Carex spp.",
  "festsp.", "Festuca spp.",
  "hiealp", "Hierochloe alpina",
  "kobmyo", "Kobresia myosuroides",
  "luzula", "Luzula spp.",
  "pedicul.", "Pedicularis sp.",
  "petfri", "Petasites frigidus",
  "poasp.", "Poa alpina",
  "sauvis", "Saussurea viscida",
  "senecio", "Senecio sp.",
  "stellon", "Stellaria longipes",
  "genpro", "Gentiana prostrata",
  "mertensia", "Mertensia paniculata",
  "oxynig", "Oxytropis nigrescens",
  "saxlya", "Saxifraga lyallii",
  "trispi", "Trisetum spicatum",
  "vacvit", "Vaccinium vitis-idaea",
  "castet", "Cassiope tetragona",
  "castilleja", "Castilleja hyperborea",
  "asteraceae", "Other Asteraceae",
  "gental", "Gentiana algida",
  "potent", "Potentilla spp.",
  "astragalus", "Astragalus sp.",
  "aconitum", "Aconitum delphinifolius",
  "arnica", "Arnica lessingii",
  "pyrola", "Pyrola grandiflora",
  "silaca", "Silene acaulis",
  "grasssp.", "Other graminoids",
  "parrya", "Parrya nudicaulis",
  "saxcer", "Saxifraga cernua",
  "cardamine", "Cardamine purpurea",
  "saxbro", "Saxifraga bronchialis",
  "ranunculus", "Ranunculus sp.",
  "oxydig", "Oxyria digyna",
  "cerast", "Cerastium beeringianum",
  "saxhie", "Saxifraga hieraciifolia",
  "dodec", "Dodecatheon frigidum",
  "monsar", "Montia sarmentosa")

point_summary <- point_intercept_long %>%
  # add shorter haypile names
  left_join(point_lookup, by = "haypileID") %>% 
  # make sure order is correct
    mutate(HP = factor(HP, levels = point_lookup$HP)) %>% 
  # calculate cover of each species (irrespective of damage)
  group_by(HP, species) %>%
    summarise(hits = sum(value), 
              n_intercepts = first(n_intercepts),
              .groups = "drop") %>% 
  mutate(cover = hits/ n_intercepts) %>% 
  # make wider table
  select(-hits, -n_intercepts) %>% 
  pivot_wider(names_from = HP, values_from = cover, values_fill = 0) %>% 
  # fix species names for table, keeping only vascular plants
  filter(!species %in% c("rock", "mushroom", "equisetum", "moss", "lich")) %>% 
  left_join(species_lookup, by = "species") %>%
    mutate(species = species_name) %>% select(-species_name)
  
# define order of species for the rows
priority_species <- c("Dryas octopetala", "Polygonum bistorta", "Polygonum viviparum",
                      "Salix reticulata", "Salix spp.", "Vaccinium uliginosum")
other_species <- sort(setdiff(unique(point_summary$species), priority_species))
species_order <- c(priority_species, other_species)

# apply this order
point_summary <- point_summary %>% 
  mutate(species = factor(species, levels = species_order)) %>%
  arrange(species)

# calculate the proportion damaged and extract total weight
damage_row_PI <- point_damage %>%
  # add shorter haypile names
  left_join(point_lookup, by = "haypileID") %>% 
  # make sure order is correct
    mutate(HP = factor(HP, levels = point_lookup$HP)) %>% 
  # make wider
  select(HP, prop_damaged) %>% 
  pivot_wider(names_from = HP,
              values_from = prop_damaged) %>%
  mutate(species = "Proportion damaged") %>%
  relocate(species)

intercepts_row <- point_damage %>%
  # add shorter haypile names
  left_join(point_lookup, by = "haypileID") %>% 
  # make sure order is correct
    mutate(HP = factor(HP, levels = point_lookup$HP)) %>% 
  # make wider
  select(HP, n_intercepts) %>% 
  pivot_wider(names_from = HP,
              values_from = n_intercepts) %>%
  mutate(species = "Total nr intercepts") %>%
  relocate(species)

# combine all
final_table_PI <- bind_rows(point_summary, damage_row_PI, intercepts_row) %>%
  # round to three decimals
  mutate(across(-species, ~ round(.x, 3)))


# create a damage-indicator matrix for grey shading
damage_points <- point_intercept_long %>% 
  # add shorter haypile names
  left_join(point_lookup, by = "haypileID") %>% 
  # make sure order is correct
    mutate(HP = factor(HP, levels = point_lookup$HP)) %>% 
  # calculate cover of each species (irrespective of damage)
  group_by(HP, species) %>%
    summarise(damage_present = any(damage == "damaged" & value > 0), .groups = "drop") %>% 
  # make wider table
  pivot_wider(names_from = HP, values_from = damage_present, values_fill = FALSE) %>% 
  # fix species names for table, keeping only vascular plants
  filter(!species %in% c("rock", "mushroom", "equisetum", "moss", "lich")) %>% 
  left_join(species_lookup, by = "species") %>%
    mutate(species = species_name) %>% select(-species_name)

# apply row order
damage_points <- damage_points %>% 
  mutate(species = factor(species, levels = species_order)) %>%
  arrange(species)


# export to Excel applying the conditional formatting :)
wb2 <- createWorkbook()
  addWorksheet(wb2, "Summary")
  addWorksheet(wb2, "Damage")

writeData(wb2, "Summary", final_table_PI)
writeData(wb2, "Damage", damage_points)

# create grey fill style
grey_style <- createStyle(fgFill = "#D9D9D9")

# apply cell shading
for(i in seq_len(nrow(damage_points))) {
  for(j in 2:ncol(damage_points)) {
    if(damage_points[[j]][i]) {
      addStyle(wb2, sheet = "Summary",
        style = grey_style, rows = i + 1, cols = j,
        gridExpand = FALSE, stack = TRUE)
    }
  }
}

# make headers bold
header_style <- createStyle(textDecoration = "bold")

addStyle(wb2, "Summary",
  header_style, rows = 1,
  cols = 1:ncol(point_summary),
  gridExpand = TRUE)

# save workbook
saveWorkbook(wb2, "data/TableS2_point_intercept_summary.xlsx", overwrite = TRUE)
    


# alternative analyses: functional groups ----
## models with functional groups ----
# first create a column with functional group id
use_avail_data <- use_avail_data |> 
  mutate(fct_group = case_when(species == "dryas" ~ "evergreen",
                               species %in% c("polbis", "polviv") ~ "forb",
                               species %in% c("salret", "salsp", "vaculi") ~ "deciduous"))

model_fct <- lmer(Lselect ~ damage * fct_group + (1 | haypileID) + (1 | species),
  data = use_avail_data, REML = TRUE)
  summary(model_fct)
  anova(model_fct) # species*damage (0.092) 
    plot(model_fct) 

emmeans(model_fct, pairwise ~ damage | fct_group) 
  # since the interaction is not significant, we need to treat these results carefully


model_fct_additive <- lme(Lselect ~ damage + fct_group, random = ~1|haypileID, 
                     method = "REML", data = use_avail_data)
  summary(model_fct_additive)
  anova(model_fct_additive)

emmeans(model_fct_additive, pairwise ~ fct_group) # identify which functional groups differ in selection
emmeans(model_fct_additive, ~ damage) # report estimated marginal means for damage


# check model assumptions
ER2 = residuals(model_fct_additive, type="normalized"); par(mfrow = c(1,2)); plot(model_fct_additive)
  boxplot(ER2 ~ use_avail_data$fct_group); abline(0, 0, col =2 )
  boxplot(ER2 ~ use_avail_data$damage); abline(0, 0, col = 2)

# other residual checks: distribution of random intercepts
  qqnorm(ranef(model_fct_additive)[[1]]); qqline(ranef(model_fct_additive)[[1]])
  # with only 30 haypiles normality may not be perfect, but gross departures can be informative.

# inspect residuals separately for each species × damage combination:
ggplot(data.frame(resid = resid(model_fct_additive),
                  fct_group = use_avail_data$fct_group,
                  damage = use_avail_data$damage),
      aes(fct_group, resid)) +
  geom_boxplot() +
  facet_wrap(~damage)
  # functional groups seem to have (roughly) similar variances, so we're good :)



## Figure S1_fct ----
# functional group effect on Lselect
emm_fct <- as.data.frame(emmeans(model_fct_additive, ~ fct_group))          

n_fct <- use_avail_data %>%
  group_by(fct_group) %>%
  summarise(n = n_distinct(haypileID), .groups = "drop")

emm_fct <- left_join(emm_fct, n_fct, by = c("fct_group"))


# plot
FigS1_fct_groups <- ggplot(use_avail_data, aes(x = fct_group, y = Lselect)) +
  # horizontal reference line
  geom_hline(yintercept = 0, linetype = "solid") +
  # raw data points
  geom_jitter(colour = "grey70", alpha = 0.5,
              size = 2, width = 0.15) +
  # estimated marginal means
  geom_point(data = emm_fct, aes(y = emmean), size = 4, colour = "black") +
  # EMM confidence intervals
  geom_errorbar(data = emm_fct,
    aes(y = emmean, ymin = lower.CL, ymax = upper.CL),
    width = 0, linewidth = 0.8) +
  # sample sizes
  geom_text(data = emm_fct,
    aes(y = min(use_avail_data$Lselect) - 0.5, label = paste0("n=", n)), colour = "black",
    size = 3) +
  scale_x_discrete(labels = c("deciduous" = "deciduous\nshrubs",
                             "evergreen" = "evergreen\nshrubs",
                             "forb" = "forbs")) +
  labs(x = NULL,
       y = "Log selection ratio",
       title = "a) Functional group effect") +
  theme_bw() +
    theme(axis.text.x = element_text(face = "italic"),
          legend.position = "bottom")

# damage effect on Lselect
emm_damage <- as.data.frame(emmeans(model_fct_additive, ~ damage))          

n_damage <- use_avail_data %>%
  group_by(damage) %>%
  summarise(n = n_distinct(haypileID), .groups = "drop")

emm_damage <- left_join(emm_damage, n_damage, by = c("damage"))

FigS1_fct_damage <- ggplot(use_avail_data, aes(x = damage, y = Lselect)) +
  # horizontal reference line
  geom_hline(yintercept = 0, linetype = "solid") +
  # raw data points
  geom_jitter(colour = "grey70", alpha = 0.5,
              size = 2, width = 0.15) +
  # estimated marginal means
  geom_point(data = emm_damage, aes(y = emmean), size = 4, colour = "black") +
  # EMM confidence intervals
  geom_errorbar(data = emm_damage,
    aes(y = emmean, ymin = lower.CL, ymax = upper.CL),
    width = 0, linewidth = 0.8) +
  # sample sizes
  geom_text(data = emm_damage,
    aes(y = min(use_avail_data$Lselect) - 0.5, label = paste0("n=", n)), colour = "black",
    size = 3) +
  labs(x = NULL,
       y = "Log selection ratio",
       title = "b) Damage effect") +
  theme_bw() +
    theme(legend.position = "none")


FigS1_fct <- FigS1_fct_groups + FigS1_fct_damage +
          plot_layout(widths = c(3, 1)) +
          plot_annotation(tag_levels = "a")


