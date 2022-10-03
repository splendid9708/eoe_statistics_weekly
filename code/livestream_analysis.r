# Databricks notebook source
library(treemapify)
library(tidyverse)
library(showtext)
library(readxl)
library(ggplot2)
library(viridis)
library(hrbrthemes)
showtext_auto()

# COMMAND ----------

df <- read_excel("livestream.xlsx")

# COMMAND ----------

df <- df %>% 
  mutate(pct_rev = paste(100*round(revenue/sum(revenue),3),"%", sep="")) %>%
  mutate(str_rev = paste("¥", revenue, sep=""))

# COMMAND ----------

ggplot(df, aes(area = revenue, fill = livestream, label = paste(livestream, str_rev, pct_rev, idol, sep="\n"))) +
  geom_treemap(show.legend = FALSE) +
  geom_treemap_text(colour = "white", size = 18) +
  labs(title="直播营收（2022年9月19日-9月25日）")

# COMMAND ----------

# MAGIC %md 

# COMMAND ----------

ggplot(df, aes(x=livestream, y=peak_renqi, fill=livestream)) +
  geom_bar(stat="identity",show.legend = FALSE, size=10)+theme_minimal()+
       geom_text(aes(label= paste(livestream, peak_renqi, idol, sep="\n")),alpha = 1, size=5)+
  coord_polar()+
    theme(
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank())+
  ggtitle("直播最高人气（2022年9月19日-9月25日）")+
  theme(plot.title = element_text(size=20))

# COMMAND ----------

hsize <- 1

df <- df %>% 
  mutate(x = hsize)

ggplot(df, aes(x = hsize, y = duration, fill = livestream)) +
  geom_col() +
   theme_void() +
  coord_polar(theta = "y") +
  geom_text(aes(label = paste(round(duration,2),"小时")),
            position = position_stack(vjust = 0.5),col = "white") +
  xlim(c(0.2, hsize + 0.5))+
    labs(title="直播时长（2022年9月19日-9月25日）", x = "", y= "付费量（¥）")

# COMMAND ----------

ggplot(df, aes(x=livestream, y=pay_per_person, fill=livestream)) +
  geom_bar(stat="identity",show.legend = FALSE)+theme_minimal()+
       geom_text(aes(label=round(pay_per_person,2)), position=position_dodge(width=0.9), vjust=-0.25)+
  labs(title="付费观众的人均付费量（2022年9月19日-9月25日）", x = "", y= "付费量（¥）")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0, hjust=0.3))

# COMMAND ----------

ggplot(df, aes(x=livestream, y=interacted_users, fill=livestream)) +
  geom_bar(stat="identity",show.legend = FALSE)+theme_minimal()+
       geom_text(aes(label=interacted_users), position=position_dodge(width=0.9), vjust=-0.25)+
  labs(title="互动人数（2022年9月12日-9月17日）", x = "", y= "互动人数")+
    theme(axis.text.x = element_text(angle = 90, vjust = 0, hjust=0.3))

# COMMAND ----------

ggplot(df, aes(x=livestream, y=total_danmu, fill=livestream)) +
  geom_bar(stat="identity",show.legend = FALSE)+theme_minimal()+
       geom_text(aes(label= paste(livestream, interacted_users, idol, sep="\n")),alpha = 1)+
  coord_polar()+
    theme(
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank())+
  labs(title="弹幕数（2022年9月12日-9月17日）", x = "", y= "弹幕数")+
    ggtitle("直播弹幕数（2022年9月19日-9月25日）")+
  theme(plot.title = element_text(size=20))

# COMMAND ----------

ggplot(df, aes(x = interacted_users, y = paid_users, label=livestream, color = livestream))+
  geom_point(size=5)+
    labs(title="付费与互动人数（2022年9月第三周）", size="直播时长", colour="直播", x= "互动人数" ,y="付费人数")+
    scale_x_continuous(expand=c(0,1000))+
    geom_text(
    label=df$livestream, 
    nudge_x = 200, nudge_y = 10, 
    check_overlap = F
  )

# COMMAND ----------

ggplot(df, aes(x = total_danmu, y = revenue, label=livestream, color = livestream)) + 
  geom_point(size=5) +
  scale_size_continuous(range = c(3, 7)) + 
  labs(title="弹幕数与营收（2022年9月第三周）", size="直播时长", colour="直播", x= "总弹幕数" ,y="营收（¥）") + 
  scale_x_continuous(expand=c(0,5000))+
    geom_text(
    label=df$livestream, 
    nudge_x = -1000, nudge_y = 1500, 
    check_overlap = T
  )
