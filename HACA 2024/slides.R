library(DEPOfunctions)
library(ggplot2)
library(dplyr)
library(officer)
library(ggpubr)
library(flextable)
library(scales)


table_data <- tibble::tribble(
  ~key_performance_indicator,    ~rag, ~sort_no,   ~progress,
               "Programme 1", "Green",       1L, 58,
           "doughnut supply", "Amber",       2L, 49,
        "staff satisfaction", "Green",       2L, 68,
                   "R usage", "Green",       2L, 82,
               "Programme 2", "Amber",       1L, 10,
          "chocolate supply",   "Red",       2L, 52,
        "staff satisfaction",   "Red",       2L, 45,
              "Excel useage", "Amber",       2L, 43,
               "Programme 3", "Amber",       1L, 77,
            "useful reports", "Green",       2L, 75,
               "Programme 4", "Green",       1L, 56,
        "time freed up by automation", "Green",       2L, 63,
  "additional requests prompted by fabulous reports", "Green",       2L, 68,
                "R useage", "Green",       2L, 90,    
  )


RAG_data <- data.frame(
  stringsAsFactors = FALSE,
               RAG = c("Red","Red","Red","Amber/Red","Amber/Red","Amber/Red",
                       "Amber/Red","Amber/Red","Amber/Red","Amber/Red",
                       "Amber/Red","Red","Amber/Red","Red","Amber/Red","Red",
                       "Red","Amber/Red","Red","Amber/Red","Amber/Red","Red",
                       "Amber/Red"),
          Category = c("IG, Data Sharing and Governance",
                       "IG, Data Sharing and Governance","Workforce and resourcing",
                       "Programme Delivery", "Financial",
                       "Operational resilience", "Financial",
                       "Operational resilience","Operational resilience",
                       "Financial","Financial","Reputational",
                       "Operational resilience","Workforce and resourcing","Operational resilience",
                       "IG, Data Sharing and Governance",
                       "IG, Data Sharing and Governance","Workforce and resourcing",
                       "Programme Delivery","Workforce and resourcing",
                       "Programme Delivery","Workforce and resourcing","Financial"),
            Source = c("Risk",
                       "Risk","Risk","Risk","Risk","Risk","Risk","Risk",
                       "Risk","Risk","Risk","Risk","Risk","Risk","Risk",
                       "Issue","Issue","Issue","Issue","Issue","Issue",
                       "Issue","Issue")
)

tree_data <- data.frame(
  stringsAsFactors = FALSE,
                           Band = c("3",
                                    "3","4","4","5","5","6","7","7","8A",
                                    "8A","8B","8B","8C","8C","8D","8D"),
                    post_filled = c("filled","vacant","vacant","filled","filled",
                                    "vacant","filled","filled","vacant",
                                    "filled","vacant","filled","vacant",
                                    "filled","vacant","filled","vacant"),
                          count = c(36L,
                                    -6L,-9L,29L,25L,-7L,6L,13L,-1L,14L,
                                    -4L,26L,-6L,21L,-3L,10L,-3L)
             )

RAG_colours <- c("Amber/Red" = "#ED8B00", "Red" = "#DA291C")

RAG_plot  <- ggplot(RAG_data, aes(x=Category, fill = RAG)) + geom_bar(aes(y = (..count..))) + geom_text(stat='count', size = 10, aes(label=..count..),position = position_stack(vjust = 0.5)) + theme_minimal(base_size = 16) + scale_fill_manual(values = RAG_colours) + coord_flip() + facet_wrap(~ forcats::fct_rev(Source), ncol = 1) +labs(title = "Areas of Concern", align = "left") +
  theme(legend.title=element_blank(), legend.position = "top")+
  theme(plot.title = element_text(hjust = 0, vjust = -12, size = 20), 
        axis.title.x=element_blank(),
        axis.text.x= element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_text(size = 16, face = "italic", color = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.text = element_text(size = 16, face = "bold" ),
        text = element_text(size = 16)
  ) 

ggsave("RAG_plot.jpg", RAG_plot, width = 16, height = 10, dpi = 150)
RAG_plot2 <- "C:/Users/LyndaHoward/OneDrive - NHS England/Documents/R projects/presentations/RAG_plot.jpg"


# tree plot
gg1 <- tree_data  |> 
  mutate(count = if_else(post_filled == "vacant", count, 0)) |> 
  
  ggplot(aes(Band, count, fill = post_filled)) +
  
  geom_col(width = 0.6) +
  geom_text(data = subset(tree_data, count<0), nudge_y = -0.8, size = 12, aes(label = round(-count, digits =  0))) +
  coord_flip() +
  
  theme_void() +
  
  scale_fill_manual(values = c("filled" = "#006747", "vacant" = "#8A1538")) +
  
  theme(plot.title = element_text(hjust = 1),
        
        axis.title.y=element_blank(),
        
        axis.text.y=element_blank(),
        
        axis.ticks.y=element_blank(),
        text = element_text(size = 14))


gg2 <- tree_data |> 
  ggplot(aes(Band, 0, label = Band)) +
  geom_text(size =12) +
  coord_flip() +
  theme_void() 


gg3 <- tree_data |> 
  mutate(count = if_else(post_filled == "filled", count, 0)) |> 
  ggplot(aes(Band, count, fill = post_filled)) +
  
  geom_col(width = 0.6) +
  geom_text(data = subset(tree_data, count>0), nudge_y = 1.2, size = 12, aes(label = round(count, digits =  0))) +
  coord_flip() + theme_void() +
  scale_fill_manual(values = c("filled" = "#006747", "vacant" = "#8A1538")) +
  theme(
    axis.title.y=element_blank(),
    axis.text.y=element_blank(),
    axis.ticks.y=element_blank(),
    text = element_text(size = 14))



title_data <-tree_data |> group_by(post_filled) |> dplyr::summarise( count = sum(count))
filled <- filter(title_data, post_filled == "filled") 
vacant <- filter(title_data, post_filled == "vacant") 
title_text1 <- text_grob(paste0(" There are ", -vacant$count , " vacant posts "), color = "#8A1538", face = "bold", size = 22)
title_text2 <- text_grob(paste0(filled$count, " filled posts across all teams."), color =  "#006747", face = "bold", size = 22)


plot_0 <- as_ggplot(title_text1) + theme(plot.margin = margin(0,0,0,0, "cm")) 
plot_and <- as_ggplot(text_grob(" and ", color = "black", size = 18))
plot_1 <- as_ggplot(title_text2) + theme(plot.margin = margin(0,0,0,0, "cm"))

plot_2 <- ggarrange(gg1, gg2, gg3, ncol = 3, widths = c(2,1,6), legend = "none", align = "h")

title_plot <- ggarrange( plot_0, plot_and, plot_1,NULL, ncol = 5, widths = c(4,1,5,2))

tree_plot <- ggarrange(title_plot, plot_2, nrow = 2, heights = c(2, 10))
ggsave("tree_plot.jpg", tree_plot, width = 16, height = 10, dpi = 150)
tree_plot2 <- "C:/Users/LyndaHoward/OneDrive - NHS England/Documents/R projects/presentations/tree_plot.jpg"

colourer <- col_numeric(
  palette = c("transparent", "#005eb8"),
  domain = c(0, 100))

flex_progress <- table_data |>  flextable() |> 
  bg(i = ~ rag == "Red", j = "rag", bg = "#DA291C") |> 
  bg(i = ~ rag == "Amber", j = "rag", bg = "orange") |>
  bg(i = ~ rag == "Green", j = "rag", bg = "#238823") |> 
  bg(bg = colourer, j = ~ progress, part = "body") |>
  colformat_int(j = "progress", suffix = "%") |> 
  bold(i = ~ sort_no == 1) |> delete_columns(j = "sort_no")  |>
  delete_part(part = "header") |> 
  width(j = "key_performance_indicator", width = 3) |> 
  width(j = "rag", width = 1) 

 
##### Initiate the slide deck #####

# Read in the template file
doc <- read_pptx(file.path(Sys.getenv("DEPO_PATH"),"HLR_template.pptx"))

#### create slides ##########
# Add a slide
doc <- add_slide(doc, layout = "2_column_1st_slide", master = "NHSE-Theme-NOV1120B-withR")

# Add the slide contents
slide_title <- "Sample Summary Slide"

doc <- ph_with(doc, slide_title, location = ph_location_label(ph_label = "Slide Heading"))
doc <- ph_with(doc, flex_progress, location = ph_location_label(ph_label = "Content Placeholder 1"))

doc <- ph_with(doc,external_img(RAG_plot2), location = ph_location_label(ph_label = "Content Placeholder 5"))

doc <- ph_with(doc, external_img(tree_plot2), location = ph_location_label(ph_label ="Content Placeholder 6"))

###### Save the output PowerPoint file #####################
print(doc, target = "summary_demo_slide.pptx" )
