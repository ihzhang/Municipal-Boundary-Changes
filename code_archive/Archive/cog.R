# COG 

# 2000 ---- 
p1 <- read_csv("COG/2000/IndFin00a.Txt")

p1 %<>%
  filter(`Type Code`==2)
