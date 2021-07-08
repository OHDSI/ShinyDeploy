plotIRv3 <- function(outcomeIds=outcomeCohortDefinitionId, event_type,data,base_size=12, metaAnalysis = TRUE) {
  outcomeIds <- outcome_ref %>%
    filter(
      outcome_group == event_type & outcome_include == 1 &
        !outcome_main_name %in% c('Seizures', 'Kawasaki disease')
    ) %>% .$outcomeId
  
  p1 <-
    data %>% filter(outcomeCohortDefinitionId %in% outcomeIds &
                      cell_ls5 == 0) %>%
    group_by(outcomeCohortDefinitionId) %>%
    ggplot(aes(x = as.numeric(
      interaction(age_group, sex_group, lex.order = T)
    ), y = IR_P_100000py))
  
  p1 <- p1 + theme_minimal() +
    #  theme(legend.position = "bottom")  +
    geom_point(
      aes(
        y = ir.rand,
        shape = sex_group,
        color = sex_group,
        x = as.numeric(interaction(age_group, sex_group, lex.order = T)) + 0.5
      ),
      size = 2.5 ,
      stroke = 1.3,
      show.legend = F
    )
  
  if (metaAnalysis) {
    p1 <- p1 +
      geom_segment(
        aes(
          y = ir.predict.lower,
          yend = ir.predict.upper,
          x = as.numeric(interaction(age_group, sex_group, lex.order = T)) + 0.5,
          xend = as.numeric(interaction(age_group, sex_group, lex.order = T)) + 0.5,
          color = sex_group
        ),
        size = 1,
        alpha = 0.8
      )
  }
  
  p1 <- p1 + #plot confint
    geom_point(
      aes(
        color = db_name,
        alpha = 1,
        shape = sex_group,
        group = db_name,
        x = as.numeric(interaction(age_group, sex_group, lex.order = T))
      ),
      stroke = 1.4,
      size = 2
    ) +
    scale_shape_manual(name = 'Sex',
                       values = c(16, 17) ,
                       guide = guide_legend(ncol = 1)) +   #values=c(16,17)
    scale_color_manual(
      values = db_color_sex,
      drop = FALSE,
      name = 'Database',
      breaks = db_names
    ) + #,  aesthetics = c("colour", "fill")
    theme(axis.text.x = element_text(angle = 50, hjust = 1)) +
    facet_wrap( ~ outcome_main_name  , nrow = 1, labeller = label_wrap_gen(20)) +
    guides(alpha = F, linetype = F) +
    ylab("Incidence rate per 100,000 person-years")  + xlab(" ") +
    scale_x_continuous(
      name = 'Age group',
      breaks = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16),
      labels = c(
        "1 - 5   F",
        " M",
        "6 - 17   F" ,
        "M",
        "18 - 34   F" ,
        "M",
        "35 - 54   F" ,
        "M",
        "55 - 64   F" ,
        "M",
        "65 - 74   F" ,
        "M",
        "75 - 84   F",
        "M",
        "85 +   F",
        "M"
      )
    )
  return(p1)
}


camelCaseToTitleCase <- function(string) {
  string <- gsub("([A-Z])", " \\1", string)
  string <- gsub("([a-z])([0-9])", "\\1 \\2", string)
  substr(string, 1, 1) <- toupper(substr(string, 1, 1))
  return(string)
}
