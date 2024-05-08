####INDEX ANALYSIS####

####Win - Loss####
#Plot correlation for AQ_score vs Rho_Index for each BlockType
for (block in c("social", "nonsocial")) {
    plot <- ggplot(merged_df %>% filter(BlockType == block), aes(x = AQ_score, y = Rho_Index)) +
        geom_point() +
        geom_smooth(method = "lm") +
        ggtitle(paste("Scatter plot for AQ_score vs Rho_Index in", block, "condition"))
    #save plot in plot_dir
    ggsave(file.path(plot_dir, paste0("Rho_Index_", block, "_scatter.png")), plot)
}

#Fit lmer model for Rho_Index with AQ_score and BlockType
model <- lmer(Rho_Index ~ AQ_score + BlockType + (1 | participant_x), data = merged_df)
print(summary(model))

#QQplot
qqplot <- create_qqplot(model)
ggsave(file.path(plot_dir, "Rho_Index_qqplot.png"), qqplot)


#Fit lm model for Rho_Index, but only in social condition
model <- lm(Rho_Index ~ AQ_score, data = merged_df %>% filter(BlockType == "social"))
print(summary(model))
#QQplot
create_qqplot(model)

#Fit lm model for Rho_Index, but only in non-social condition
model <- lm(Rho_Index ~ AQ_score, data = merged_df %>% filter(BlockType == "nonsocial"))
print(summary(model))

#QQplot
create_qqplot(model)


####Ambi_Index####
#Filter data so that each participant is unique, then plot correlation for AQ_score vs Ambi_Index_Win
unique_participants <- merged_df %>% distinct(participant_x, .keep_all = TRUE)

#index columns
index_cols <- c("Ambi_Index_Alpha_Win", "Ambi_Index_Alpha_Loss", "Ambi_Index_Rho_Win", "Ambi_Index_Rho_Loss")

#Plot correlation for AQ_score vs each Ambi_Index
for (index in index_cols) {
    plot <- ggplot(unique_participants, aes(x = AQ_score, y = !!sym(index))) +
        geom_point() +
        geom_smooth(method = "lm") +
        ggtitle(paste("Scatter plot for AQ_score vs", index))
    #save plot in plot_dir
    ggsave(file.path(plot_dir, paste0(index, "_scatter.png")), plot)
}

#Fit Linear model for AQ_score vs each Ambi_Index
for (index in index_cols) {
    formula <- as.formula(paste(index, "~ AQ_score"))
    model <- lm(formula, data = unique_participants)
    print(index)
    print(summary(model))
}