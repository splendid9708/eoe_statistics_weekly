# Databricks notebook source
knitr::opts_chunk$set(echo = TRUE)
library(ggplot2)
library(tidyverse)
library(showtext)
library(RColorBrewer)
library(gplots)
library(gt)
library(forecast)
library(ggtext)
library(emo)
library(scales)
library(fmsb)
library("xts")
library(prismatic)
showtext_auto()

# COMMAND ----------

df_user <- read.csv("User_stat_22919.csv")
df_vid  <- read.csv("Vid_stat_22919.csv")
df_cap  <- read.csv("Cap_lists_22919.csv")

# COMMAND ----------

colScale <- scale_color_manual(values = c("露早"="#B6DE6A", "柚恩" = "#F97E2B", "虞莫" = "#D5B2FF", "莞儿" = "#7DADDA", "米诺" = "#B6190F"))

# COMMAND ----------

df_user2 <- df_user %>% 
  mutate(dhh_per_follower = dahanghai/follower) %>%
  mutate(date=as.Date(filedate,format='%Y-%m-%d')) %>%
  mutate(follower_k = follower/1000)
df_user2

# COMMAND ----------

ggplot(df_user2, aes(x=date,y=follower_k, color=name, group=name)) + 
  geom_line(lwd=1) +
  facet_wrap(.~name,scales = "free_y") +
  colScale +
  labs(title="个人账号关注数(历史数据)",
        x ="时间", y = "关注人数（千人）", color = "成员")

# COMMAND ----------

ggplot(df_user2%>% filter(date>="2022-09-12"), aes(x=date,y=follower_k, color=name, group=name)) + 
  geom_line(lwd=1) +
  facet_wrap(.~name,scales = "free_y") +
  colScale +
  labs(title="个人账号关注数(9月12日-25日)",
        x ="时间", y = "关注人数（千人）", color = "成员")+
  scale_x_date(breaks = scales::breaks_pretty(7))+
theme(axis.text.x = element_text(angle = 45, hjust = 1))

# COMMAND ----------

ggplot(df_user2,aes(x=date,y=follower_k, color=name, group=name)) + 
  geom_line(lwd=1) +
  scale_x_date(breaks = scales::breaks_pretty(7))+
  colScale +
  labs(title="个人账号关注数(历史数据)",
        x ="时间", y = "关注人数（千人）", color = "成员")

# COMMAND ----------

ggplot(df_user2,aes(x=date,y=dahanghai, color=name, group=name)) + 
  geom_line(lwd=2) +
  colScale +
  labs(title="大航海人数（历史数据）",
        x ="时间", y = "大航海人数（舰长+提督+总督）", color = "成员")

# COMMAND ----------

ggplot(df_user2 %>% filter(date>="2022-09-12"),aes(x=date,y=dahanghai, color=name, group=name)) + 
  geom_line(lwd=2) +
  geom_point(lwd=2.5) +
  scale_x_date(breaks = scales::breaks_pretty(6))+
  colScale +
  labs(title="大航海人数(9月12日-25日)",
        x ="时间", y = "大航海人数（舰长+提督+总督）", color = "成员")

# COMMAND ----------

ggplot(df_user2,aes(x=date,y=dhh_per_follower, color=name, group=name)) + 
  geom_line(lwd=2) +
  colScale +
  labs(title="大航海人数-关注数比（累计数据）",
        x ="时间", y = "大航海人数/关注数", color = "成员")

# COMMAND ----------

df_vid2 = df_vid %>%
  filter(filedate=="2022-09-25")

# COMMAND ----------

df_vid2 %>% 
  arrange(desc(like)) %>% 
  select(like, title) %>%
  top_n(10, like) %>%
  ggplot(aes(x=like, y=reorder(title, like, size=10), fill=title)) +
  geom_bar(stat="identity") +
  theme(text = element_text(size=13))+
  geom_text(aes(label=like), vjust=0.5, hjust=1,size=6) +
  theme(legend.position="none")+
  labs(y = NULL, x="点赞数", title="点赞数Top10（9月25日晚)")

# COMMAND ----------

df_vid2 %>% 
  select(view, title)%>% group_by(title) %>% summarize_all(max)  %>% 
   arrange(desc(view))%>%
  top_n(10, view) %>%
  ggplot(aes(x=view, y=reorder(title, view, size=10), fill=title)) +
  geom_bar(stat="identity") +
  theme(text = element_text(size=13))+
  geom_text(aes(label=view), vjust=0.5, hjust=1,size=4) +
  theme(legend.position="none")+
  labs(y = NULL, x="播放量", title="播放量Top10（9月25日晚)")

# COMMAND ----------

df_vid2 %>% 
  arrange(desc(danmaku)) %>% 
  select(danmaku, title) %>%group_by(title) %>% summarize_all(max)  %>% 
  top_n(10, danmaku) %>%
  ggplot(aes(x=danmaku, y=reorder(title, danmaku), fill=title)) +
  geom_bar(stat="identity") +
  geom_text(aes(label=danmaku), vjust=0.5, hjust=1, size=6) +
    theme(text = element_text(size=13))+
  theme(legend.position="none") +
  labs(y = NULL, x="弹幕数", title="弹幕数Top10（9月25日晚)")

# COMMAND ----------

# MAGIC %md 

# COMMAND ----------

df_ts = df_user2 %>% select(date, name, follower_k)
luzao_ts = df_ts %>% filter(name == "露早") %>% select(date, follower_k)
youen_ts = df_ts %>% filter(name == "柚恩")%>% select(date, follower_k)
waner_ts = df_ts %>% filter(name == "莞儿")%>% select(date, follower_k)
minuo_ts = df_ts %>% filter(name == "米诺")%>% select(date, follower_k)
yumo_ts = df_ts %>% filter(name == "虞莫")%>% select(date, follower_k)

# COMMAND ----------

luzao_ts

# COMMAND ----------

luzao_ts = xts(luzao_ts$follower_k, luzao_ts$date)
youen_ts = xts(youen_ts$follower_k, youen_ts$date)
waner_ts = xts(waner_ts$follower_k, waner_ts$date)
minuo_ts = xts(minuo_ts$follower_k, minuo_ts$date)
yumo_ts = xts(yumo_ts$follower_k, yumo_ts$date)

# COMMAND ----------

arima_luzao = auto.arima(luzao_ts, seasonal=T)
arima_youen = auto.arima(youen_ts, seasonal=T)
arima_waner = auto.arima(waner_ts, seasonal=T)
arima_minuo = auto.arima(minuo_ts, seasonal=T)
arima_yumo = auto.arima(yumo_ts, seasonal=T)

# COMMAND ----------

f_luzao = forecast(arima_luzao,level=c(95), h = 7)
f_youen = forecast(arima_youen,level=c(95),  h = 7)
f_waner = forecast(arima_waner, level=c(95), h = 7)
f_minuo = forecast(arima_minuo,level=c(95),  h = 7)
f_yumo  = forecast(arima_yumo, level=c(95), h = 7)

# COMMAND ----------

 length(seq(as.Date("2022/8/18"), as.Date("2022/10/4"), "day"))

# COMMAND ----------

autoplot(f_luzao) +
  scale_x_continuous(breaks = 1:48, labels = seq(as.Date("2022/8/18"), as.Date("2022/10/4"), "day"))+
theme(axis.text.x=element_text(angle=60, hjust=1))+
  labs(title="个人账号关注数预测:露早 (SARIMA, 95%CI)",
        x ="时间", y = "关注人数（千人）")+
  scale_y_continuous(breaks= pretty_breaks())

# COMMAND ----------

autoplot(f_youen) +
  scale_x_continuous(breaks = 1:48, labels = seq(as.Date("2022/8/18"), as.Date("2022/10/4"), "day"))+
theme(axis.text.x=element_text(angle=60, hjust=1))+
  labs(title="个人账号关注数预测:柚恩 (SARIMA, 95%CI)",
        x ="时间", y = "关注人数（千人）")+
  scale_y_continuous(breaks= pretty_breaks())

# COMMAND ----------

autoplot(f_minuo) +
scale_x_continuous(breaks = 1:48, labels = seq(as.Date("2022/8/18"), as.Date("2022/10/4"), "day"))+
theme(axis.text.x=element_text(angle=60, hjust=1))+
  labs(title="个人账号关注数预测:米诺 (SARIMA, 95%CI)",
        x ="时间", y = "关注人数（千人）")+
   scale_y_continuous(breaks= pretty_breaks())

# COMMAND ----------

autoplot(f_waner) +
 scale_x_continuous(breaks = 1:48, labels = seq(as.Date("2022/8/18"), as.Date("2022/10/4"), "day"))+
theme(axis.text.x=element_text(angle=60, hjust=1))+
  labs(title="个人账号关注数预测:莞儿 (SARIMA, 95%CI)",
        x ="时间", y = "关注人数（千人）")+
   scale_y_continuous(breaks= pretty_breaks())

# COMMAND ----------

autoplot(f_yumo) +
scale_x_continuous(breaks = 1:48, labels = seq(as.Date("2022/8/18"), as.Date("2022/10/4"), "day"))+
theme(axis.text.x=element_text(angle=60, hjust=1))+
  labs(title="个人账号关注数预测:虞莫 (SARIMA, 95%CI)",
        x ="时间", y = "关注人数（千人）")+
   scale_y_continuous(breaks= pretty_breaks())
