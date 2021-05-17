setwd('../../realitycommons')
time.zone = 'Etc/GMT+6'
dir.output = 'tmp'

anchor = read_csv('data/badge_assignment.csv',
                  col_types=cols(BID=col_integer(), role=col_character())) %>%
  filter(role != 'RSSI') %>% drop_na()
base.station = anchor[anchor$role == 'Base station',]
office = anchor[anchor$role != 'Base station',]
office = office %>% mutate(x=ifelse(BID==99, 1036, x))

badge.loc = read_csv('data/Badge/LocationTrackingEvery1Minute.csv', locale=locale(tz=time.zone),
                     col_types=cols(time=col_datetime())) %>%
  rename(date.time=time)

role = read_csv('data/badge_assignment.csv',
                col_types=cols(.default=col_skip(), BID=col_integer(), role=col_character())) %>%
  filter(role != 'RSSI', role != 'Base station') %>% drop_na()

transaction = read_csv('data/Badge/Transactions.csv', locale=locale(tz=time.zone),
                       col_types=cols(assigned.to=col_integer(), closed.by=col_integer(), duration=col_integer(),
                                      complexity=col_character(), role=col_character(),
                                      assign.date=col_datetime(), close.date=col_datetime())) %>% drop_na()

zigbee = read_csv('data/Badge/Zigbee.csv', locale=locale(tz=time.zone),
                  col_types=cols(sender.id=col_integer(), local.id=col_integer(), RSSI=col_integer(), date.time=col_datetime()))

zigbee.worker = zigbee %>%
  filter(sender.id %in% office$BID & local.id %in% office$BID, sender.id != 0 & local.id != 0)

ir = read_csv('data/Badge/IR.csv', locale=locale(tz=time.zone),
              col_types=cols(sender.id=col_integer(), local.id=col_integer(), date.time=col_datetime())) %>%
  filter((sender.id %in% office$BID) & (local.id %in% office$BID))

ir.filt = ir %>% filter(hms::as_hms(date.time) >= hms::hms(h=9) & hms::as_hms(date.time) <= hms::hms(h=18))

library(RColorBrewer)
color.role = c(brewer.pal(3, "Pastel1"), 'white', 'white')
color.type = c("#5c9986", "#6643bc", "#b98582", "#7aa23e", "#c553bd", "#514e2b", "#c34368",
               "#7885bd", "#c86532", "#4c2c4c")[1:3]
color.badge = c("#2f2234","#58b434","#623ace","#a0a533","#bc46de","#5cb065","#d24fc3",
                "#436f2e","#49258b","#e08635","#7e6bdc","#e3472b","#7690da","#af4528",
                "#62a2c2","#de445e","#55ab95","#df4c97","#203a2b","#ca83c4","#bb8b45",
                "#485398","#74662e","#893787","#9fa17d","#371a50","#d78281","#477468",
                "#9e2f56","#485e79","#853e27","#a793ad","#591d24","#8d6861","#45371e",
                "#7b4467")
names(color.role) = c(unique(role$role), 'NA')
names(color.badge) = levels(role$BID)