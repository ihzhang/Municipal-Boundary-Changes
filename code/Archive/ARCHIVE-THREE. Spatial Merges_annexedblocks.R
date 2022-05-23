# archived spatial merge code ####
end_list <- split(blocks2017, f = blocks2017$plid)
start_list <- split(blocks2014, f = blocks2014$plid)

get_annex <- function(end_df, start_df, homeless_df) {
  x <- anti_join(end_df, start_df, by = "blkid") %>%
    filter(blkid %in% homeless_df$blkid)
  return(x)
  print(end_df$plid[1])
}

test <- get_annex(end_list[[10500]], start_list[[10500]], blocks2014_na)

annexedblocks1417 <- pmap_dfr(list(end_list, start_list, list(blocks2014_na)), 
                              get_annex)

# OG for-loop method 
for (i in 1:length(blocks)) {
  block14 <- blocks2014 %>% filter(plid %in% blocks[[i]]$plid)
  blocks[[i]] %<>%
    filter((!(blkid %in% block14$blkid) & (blkid %in% blocks2014_na$blkid))) 
  print(i)
}
annexedblocks <- rbindlist(blocks)