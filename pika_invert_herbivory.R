#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#         INTERACTIONS BETWEEN PIKA AND INVERTEBRATE HERBIVORY
#                   Isabel C Barrio (isabel@lbhi.is)
#               based on old R scripts from Kristen Peck
#                        last update 24.05.2026
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# libraries ---- 
library(readxl)          # to import data directly from Excel
library(openxlsx)        # to write out Excel files and format tables
library(tidyverse)
library(nlme)
library(emmeans)         # calculate estimated marginal means
library(performance)     # calculate intraclass correlation


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
             # calculate number of hits per haypile, and calculate cover
             group_by(haypileID, species, damage, n_intercepts) %>%
                summarise(hits = sum(value, na.rm = TRUE), .groups = "drop") %>% ungroup() %>% 
             mutate(value = round(hits/n_intercepts, 6),
                    index = "avail") %>% # indicate that these values are for availability
             select(haypileID, index, species, damage, value)

# check how many haypiles are included in each dataset
point_intercept %>% distinct(haypileID) %>% print(n=30) # 30 haypiles
haypiles %>% distinct(haypileID) %>% print(n=35)        # 31 haypiles; drop E CC13 N10 E15


# combine use and availability datasets
use_avail <- haypiles %>% bind_rows(point_intercept) %>% 
  pivot_wider(names_from = index, values_from = value) %>% 
  # chech that information is available in both datasets (haypiles and point_intercept)
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
    # correct availability values (when use > 0)
    mutate(avail = case_when(is.na(avail) & use > 0 ~ 0.001, # CHECK
                             avail == 0 & use > 0 ~ 0.001, # CHECK
                             TRUE ~ avail),
           use = case_when(is.na(use) ~ 0, TRUE ~ use)) %>% 
    # calculate selection ratios
    mutate(select.ratio = (use/avail),
           Lselect = log(select.ratio + 0.0001)) # CHECK

# check to decide on which constant to use for the log transformation
sort(round(use_avail_subset$select.ratio, 4))


# models
model0_interaction = lme(Lselect ~ damage*species, random = ~1|haypileID, method="ML", data = use_avail_subset)
model0_additive = lme(Lselect ~ damage + species, random = ~1|haypileID, method="ML", data = use_avail_subset)      
  anova(model0_interaction, model0_additive)         # species*damage (0.29) 
    summary(model0_interaction)
      plot(model0_interaction) #doesn't look too bad ;)
       
model_interaction = lme(Lselect ~ damage*species, random = ~1|haypileID, method = "REML", data = use_avail_subset)
  summary(model_interaction)

emmeans(model_interaction, pairwise ~ damage | species) 
  # since the interaction is not significant, we need to treat these results carefully


model_additive = lme(Lselect ~ damage + species, random = ~1|haypileID, method = "REML", data = use_avail_subset)
  summary(model_additive)

emmeans(model_interaction, pairwise ~ species) # identify which species differ in selection
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

VarCorr(model_additive)

vc <- VarCorr(model_additive)
  var_haypile <- as.numeric (vc[1, "Variance"])
  var_resid <- as.numeric(vc[nrow(vc), "Variance"])
  (ICC <- var_haypile / (var_haypile + var_resid))

# ICC shows the variance attributable to differences among haypiles 
# (the rest up to 1 is the variance within haypiles)
  
# alternatively we can use the function icc in the performance package
icc(modelAb.final)  


# we can also calculate marginal and conditional R2 (Nakagawa & Schielzeth, 2013)
# also with the performance package
r2_nakagawa(model_additive)

# marginal R2 is the variance explained by the fixed effects only
# conditional R2 is the variance explained by fixed and random effects

# in our case marginal R2 0.206 --> 21% explained by species*damage
# conditional R2 0.324 --> about 12% explained by the random effect (haypileID)



# Figure 1 ----
# plot raw values and estimated marginal means
emm <- as.data.frame(emmeans(model_interaction, ~ species*damage))          

n <- use_avail_subset %>%
  group_by(species, damage) %>%
  summarise(n = n_distinct(haypileID), .groups = "drop")

emm <- left_join(emm, n, by = c("species", "damage"))


# plot
ggplot(use_avail_subset, aes(x = species, y = Lselect, colour = damage)) +
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
    aes(y = min(use_avail_subset$Lselect) - 0.5, label = paste0("n=", n), group = damage), colour = "black",
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
  

# alternatively we could have violin plots
ggplot(use_avail_subset, aes(x = species, y = Lselect, fill = damage)) +
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

n_species <- use_avail_subset %>%
  group_by(species) %>%
  summarise(n = n_distinct(haypileID), .groups = "drop")

emm_species <- left_join(emm_species, n_species, by = c("species"))


# plot
ggplot(use_avail_subset, aes(x = species, y = Lselect)) +
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
    aes(y = min(use_avail_subset$Lselect) - 0.5, label = paste0("n=", n)), colour = "black",
    size = 3) +
  scale_x_discrete(labels = c("dryas" = "Dryas\noctopetala",
                             "polbis" = "Polygonum\nbistorta",
                             "polviv" = "Polygonum\nviviparum",
                             "salret" = "Salix\nreticulata",
                             "salsp" = "Salix sp.",
                             "vaculi" = "Vaccinium\nuliginosum")) +
  labs(x = NULL,
       y = "Log selection ratio") +
  theme_bw() +
    theme(axis.text.x = element_text(face = "italic"),
          legend.position = "bottom")




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
                                    TRUE ~ "Other")) %>%  # there should be no "Other" ;)
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
damage_row <- haypiles_30 %>% 
  # add shorter haypile names
  left_join(haypile_lookup, by = "haypileID") %>% 
  # make sure order is correct
    mutate(HP = factor(HP, levels = haypile_lookup$HP)) %>% 
  group_by(HP) %>%
  summarise(prop_damaged = sum(value[damage == "damaged"]), .groups = "drop") %>%
  pivot_wider(names_from = HP,
              values_from = prop_damaged) %>%
  mutate(species_group = "Proportion damaged") %>%
  relocate(species_group)

biomass_row <- haypiles_30 %>% 
  # add shorter haypile names
  left_join(haypile_lookup, by = "haypileID") %>% 
  # make sure order is correct
    mutate(HP = factor(HP, levels = haypile_lookup$HP)) %>% 
  group_by(HP) %>%
  summarise(total_biomass = first(total_weight)) %>%
  pivot_wider(names_from = HP,
              values_from = total_biomass) %>%
  mutate(species_group = "Total biomass (g)") %>%
  relocate(species_group)

# combine all
final_table <- bind_rows(haypile_summary, damage_row, biomass_row) %>%
  # round to three decimals
  mutate(across(-species_group, ~ round(.x, 3)))

# create a damage-indicator matrix for grey shading
damage_present <- haypiles_30 %>% 
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
                                    TRUE ~ "Other")) %>%  # there should be no "Other" ;)
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

writeData(wb2, "Summary", point_summary)
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
    











####Figure 2 -- species selection
par(mfrow=c(1,1))
par(mai=c(2,3,1,2))

model.all.lmm1=lme(Lselect~fSPP-1, random=~1|fPIKA, method="REML", data=datos)
summary(model.all.lmm1)

#get means and SEs from there :)
means.logselect <- c(-1.137,-1.562,0.232)
ci.logselect <- (1.96*(c(0.292,0.305,0.291)))

var.names <- c("Dryas octopetala", "Polygonum viviparum", "Salix spp.")

library(arm)
coefplot(means.logselect, sds=ci.logselect, CI=1,varnames=var.names,cex.var=1.2,
         main="Log selection ratios",cex.pts=2)

library(nlme)
dryas <- subset(datos, datos$fSPP=="dryas")
  model.dryas.lmm1=lme(Lselect~fDAM, random=~1|fPIKA, method="REML", data=dryas)
  summary(model.dryas.lmm1)

polviv <- subset(datos, datos$fSPP=="polviv")
  model.polviv.lmm1=lme(Lselect~fDAM, random=~1|fPIKA, method="REML", data=polviv)
  summary(model.polviv.lmm1)

salsp <- subset(datos, datos$fSPP=="salsp")
  model.salsp.lmm1=lme(Lselect~fDAM, random=~1|fPIKA, method="REML", data=salsp)
  summary(model.salsp.lmm1)



    
    
    
