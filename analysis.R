library(broom)
library(crayon)
library(plyr)
library(MASS)
library(tidyverse)
library(lubridate)
library(ggpubr)
library(grid)
library(gridExtra)
library(scales)
library(igraph)
library(ggraph)
library(tidygraph)
library(dendextend)
library(DiagrammeR)

library(sandwich)
library(lmtest)

theme_set(theme_bw(base_size=8) + theme(panel.grid=element_blank()))

source('rddat.R')

dir.output.sensor = file.path(dir.output, 'sensor')
dir.output.plot = file.path(dir.output.sensor, 'plot')
dir.output.data = file.path(dir.output.sensor, 'data')

dir.outputs = c(dir.output, dir.output.sensor, dir.output.plot, dir.output.data)
for (od in dir.outputs) dir.create(od, showWarnings=T)

plot.width.small = 2.25
plot.width.small.full = 3.25
plot.width.large = 7.125

scale_color_complexity = function(...) {
  t.col = hue_pal()(3)
  scale_color_manual('Task Difficulty', values=c('Basic'=t.col[1], 'Advanced'=t.col[2], 'Complex'=t.col[3]))
}

scale_shape_complexity = function(...) {
  scale_shape_manual('Task Difficulty', values=c('Basic'=16, 'Advanced'=15, 'Complex'=17))
}

save.plot = function(filename, width=NA, height=NA) {
  ggsave(paste0(dir.output.plot, '/', filename, '.eps'), device=cairo_ps, width=width, height=height)
  ggsave(paste0(dir.output.plot, '/', filename, '.png'), width=width, height=height)
}

###
# Summary
###

ggplot(ir %>%
         mutate(date=floor_date(date.time, unit='day')) %>%
         pivot_longer(c(sender.id, local.id), values_to='id') %>%
         count(id, date),
       mapping=aes(x=factor(id, levels=unique(id)), y=date, fill=n)) +
  geom_tile() +
  labs(x="Employee Badge ID", y="Date") +
  scale_fill_distiller("Encounters", trans='log10', palette='Spectral') +
  scale_y_datetime(date_labels='%m-%d', date_breaks='1 day') +
  guides(fill=guide_colourbar(title.vjust=1)) +
  theme(axis.text.x=element_text(angle=90, vjust=0.25, hjust=0.25),
        legend.position='top', legend.key.height=unit(0.25, 'cm'))
save.plot('ir-count', width=plot.width.small, height=plot.width.small)

###
# Task filtering: Focus on tasks that have data
###

tmp.bloc.simple = badge.loc %>% count(id, date.time)
fn.hasloc = function(bid, start, end) {
  nrow(tmp.bloc.simple %>% filter(id==bid, date.time %within% interval(start, end))) > 0
}
fn.hasir = function(bid, start, end) {
  nrow(ir %>% filter(sender.id==bid | local.id==bid, date.time %within% interval(start, end))) > 0
}
fn.haszigbee = function(bid, start, end) {
  nrow(zigbee %>% filter(sender.id==bid | local.id==bid, date.time %within% interval(start, end))) > 0
}

transaction.filt = transaction %>%
  mutate(tid=row_number(), same.id=(assigned.to == closed.by)) %>%
  rowwise() %>%
  mutate(has.ir=fn.hasir(assigned.to, floor_date(assign.date), ceiling_date(close.date)),
         has.zigbee=fn.haszigbee(assigned.to, floor_date(assign.date), ceiling_date(close.date)),
         has.loc=fn.hasloc(assigned.to, floor_date(assign.date), ceiling_date(close.date))) %>%
  ungroup() %>%
  filter(same.id & (has.ir | has.zigbee | has.loc) & duration > 0) %>%
  select(-same.id, -has.ir, -has.zigbee, -has.loc)

fn.duration.day <- function(start, end) {
  if (as_date(start)==as_date(end)) {
    return(as.integer(difftime(end, start, units='mins')))
  }
  my_dates = seq.Date(as.Date(start), as.Date(end), by="day")
  my_dates = my_dates[!(weekdays(my_dates) %in% c('Saturday', 'Sunday'))]
  my_intervals <- interval(ymd_hm(paste(my_dates, "09:00"), tz=time.zone), ymd_hm(paste(my_dates, "18:00"), tz=time.zone))
  int_start(my_intervals[1]) <- pmax(pmin(start, int_end(my_intervals[1])), int_start(my_intervals[1]))
  int_end(my_intervals[length(my_intervals)]) <- pmax(pmin(end, int_end(my_intervals[length(my_intervals)])), int_start(my_intervals[length(my_intervals)]))
  sum(time_length(my_intervals, "min"))
}

transaction.filt = transaction.filt %>%
  rowwise() %>%
  mutate(duration.day=fn.duration.day(assign.date, close.date)) %>%
  ungroup() %>%
  filter(duration.day > 0)

par(mfrow=c(1,2)); boxplot(transaction.filt$duration, ylim=c(0,3500)); boxplot(transaction.filt$duration.day, ylim=c(0,2000)); par(mfrow=c(1,1))

###
# Set data
###

dat.interact = ir

###
# Interactions and productivity
###

fn.build.network = function(start=NULL, end=NULL) {
  intv = NULL
  if (!is.null(start) & !is.null(end)) {
    intv = interval(start, end)
  }
  
  if (is.null(intv)) {
    dat = dat.interact
  } else {
    dat = dat.interact %>% filter(date.time %within% intv)
  }

  if (nrow(dat)==0) return(NULL)  # Null if there is no data within the interval
    
  edge.list.duration = dat %>%
    # mutate(from=as.character(sender.id), to=as.character(local.id)) %>%
    # group_by(from, to) %>%
    mutate(key.fromto=paste(pmin(sender.id, local.id), pmax(sender.id, local.id), sep=';')) %>%
    group_by(key.fromto) %>%
    summarize(ts=n()) %>%
    separate(key.fromto, into=c('from', 'to'), sep=';') %>%
    ungroup() %>%
    mutate(length.kk=log(1/ts+3), length.fr=exp(ts+4)) %>%
    select(from, to, ts, length.kk, length.fr)  # add duration?
  print(edge.list.duration)
  
  graph.inter = tbl_graph(edges=edge.list.duration) %N>%
    left_join(role %>% mutate(BID=as.character(BID)), by=c('name'='BID')) %N>%
    mutate(n.interact=centrality_degree())
  centrality = degree(graph.inter %E>% distinct(from, to), mode='all')
  
  print(intv)
  print(centrality)
  
  graph.inter = graph.inter %N>% left_join(tibble(name=names(centrality), centrality=centrality), by='name')
  graph.inter
}
fn.build.network()

uniq.intv = lapply(sort(unique(as_date(dat.interact$date.time))),
                   function(x) { interval(x, x+duration(1, 'day')) })
graph.inter.all = lapply( uniq.intv, function(intv) fn.build.network(int_start(intv), int_end(intv)) )

### IR Network

graph.inter = fn.build.network()
ggraph(graph.inter, layout='kk', weights=length.kk) +
  geom_edge_link(aes(edge_width=ts)) +
  geom_node_point(aes(fill=role, size=centrality), shape=21) +
  geom_node_text(mapping=aes(label=name), size=1.5) +
  scale_edge_width(range=c(0.25,1.8)) +
  scale_size_continuous(range=c(3,6)) +
  scale_fill_discrete('Role') +
  guides(edge_width=F, size=F) +
  theme_graph() +
  theme(legend.position=c(0.15,0.1), legend.text=element_text(size=4, lineheight=0), legend.title=element_text(size=6),
        legend.key.size=unit(4, 'mm'), legend.key.width=unit(0.25,'cm'), legend.key.height=unit(0.25,'cm'),
        plot.margin=margin())
save.plot('ir-network', width=plot.width.small, height=plot.width.small)

### centrality

transaction.durcent = transaction.filt %>%
  left_join(graph.inter %>% as_tibble() %>% mutate(name=as.integer(name)) %>% select(name, centrality), by=c('assigned.to'='name'))
ggplot(data=transaction.durcent %>%
         filter(!(duration %in% boxplot.stats(duration)$out)) %>%
         group_by(assigned.to, complexity, role, centrality) %>%
         summarize(duration=median(duration)),
       mapping=aes(x=centrality, y=duration, color=complexity)) +
  facet_wrap(complexity ~ role, scales='free', ncol=2) +
  geom_point() +
  # geom_smooth(method='lm', formula=y ~ x) +
  scale_x_continuous(breaks=function(x) unique(floor(pretty(seq(0, (max(x) + 1) * 1.1))))) +
  labs(x='Centrality', y='Avg. Duration', title='Duration of tasks by centrality and complexity')

ggplot(data=transaction.durcent %>%
         filter(!(duration.day %in% boxplot.stats(duration.day)$out)), # %>%
         # group_by(assigned.to, complexity, role, centrality) %>%
         # summarize(duration=median(duration.day)),
       mapping=aes(x=centrality, y=duration, color=complexity)) +
  facet_wrap(complexity ~ role, scales='free', ncol=2) +
  geom_point() +
  # geom_smooth(method='lm', formula=y ~ x) +
  scale_x_continuous(breaks=function(x) unique(floor(pretty(seq(0, (max(x) + 1) * 1.1))))) +
  labs(x='Centrality', y='Avg. Duration', title='Duration of tasks by centrality and complexity')

### Daily Centrality and productivity

fn.gen.durcent = function(start=NULL, end=NULL) {
  g = fn.build.network(start, end)
  
  intv = NULL
  if (!is.null(start) & !is.null(end)) {
    intv = interval(start, end, tzone=time.zone)
  }
  
  g = g %>% as_tibble()
  if (nrow(g) == 0) return(NULL)
  
  transaction.durcent = transaction.filt %>%
    filter(close.date %within% intv) %>%
    # filter(as_date(assign.date) == as_date(close.date)) %>%
    group_by(assigned.to, complexity, role) %>%
    summarize(avg.duration=mean(duration.day), med.duration=median(duration.day), var.duration=var(duration.day), n.task=n()) %>%
    left_join(g %>% mutate(name=as.integer(name)) %>% select(name, centrality, n.interact), by=c('assigned.to'='name'))
}

durcent = lapply( uniq.intv, function(intv) fn.gen.durcent(int_start(intv), int_end(intv)) )
durcent = bind_rows(durcent) %>%
  mutate(centrality=replace_na(centrality, 0),
         n.interact=replace_na(n.interact, 0),
         complexity=factor(complexity, levels=c('Basic', 'Advanced', 'Complex'))) %>%
  ungroup()
durcent %>% group_by(role, complexity) %>% summarize(r=cor(centrality, avg.duration))

fn.lmer.durcent.expl = function(f, df.durcent, omit.bid=NULL, plot.ylin=F) {
  model.durcent.lm <<- lm(formula=f, data=df.durcent)
  print(summary(model.durcent.lm)); print(coeftest(model.durcent.lm, vcov=sandwich)); cat(red(strrep('-', 25)), '\n')
  
  res = abs(resid(model.durcent.lm)); fit = fitted(model.durcent.lm)
  model.durcent.resid.lm = lm(res ~ fit)
  res.hat = fitted(model.durcent.resid.lm)
  
  df.durcent = df.durcent %>%
    mutate(weight=abs(resid(model.durcent.lm))^2, weight.plot=(1/weight-min(1/weight))/(max(1/weight)-min(1/weight))) %>%
    mutate(weight=ifelse(res.hat^2 == 0, 1, res.hat^2), weight.plot=(1/weight-min(1/weight))/(max(1/weight)-min(1/weight)))
  print(res)
  print(res.hat)
  
  tmp.durcent <<- df.durcent  # workaround for influence.ME::influence
  model.durcent <<- lm(formula=f, weights=1/weight, data=tmp.durcent)
  model.durcent.smmy = summary(model.durcent)
  print(model.durcent.smmy); print(coeftest(model.durcent, vcov=sandwich)); cat(red(strrep('-', 25)), '\n')
  model.durcent.cd = cooks.distance(model.durcent)

  par(mfrow=c(2,2))
  plot(model.durcent)
  par(mfrow=c(1,1))
  
  p = ggplot(mapping=aes(x=factor(df.durcent$assigned.to), y=model.durcent.cd)) +
    geom_point() +
    geom_hline(yintercept=3*mean(model.durcent.cd), color='red') +
    geom_hline(yintercept=4/length(model.durcent.cd), color='blue') +
    geom_text(aes(label=ifelse(model.durcent.cd > 3*mean(model.durcent.cd), paste(1:length(model.durcent.cd), format(df.durcent$avg.duration, digits=6), sep=';'), '')), hjust=-.1, vjust=-.1)
  print(p)
  
  new.durcent = df.durcent
  df.durcent %>% print(n=Inf)
  print(model.durcent.cd); print(df.durcent[as.vector(model.durcent.cd > 3*mean(model.durcent.cd)),]$assigned.to); cat(red(strrep('-', 25)), '\n')
  
  if (!is.null(omit.bid)) {
    tmp.new.durcent <<- new.durcent %>% filter(!(assigned.to %in% omit.bid))
    model.durcent <<- lm(formula=f, weights=1/weight, data=tmp.new.durcent)
    model.durcent.smmy = summary(model.durcent)
    print(model.durcent.smmy); print(coeftest(model.durcent, vcov=sandwich)); cat(red(strrep('-', 25)), '\n')
  }
  
  newdata = data.frame(centrality=min(new.durcent$centrality):max(new.durcent$centrality))
  y.hat.lm = predict(model.durcent.lm, newdata=newdata)
  y.hat = predict(model.durcent, newdata=newdata, interval='confidence')
  p.out = ggplot() +
    # geom_ribbon(data=newdata, mapping=aes(x=centrality, ymin=y.hat[,'lwr'], ymax=y.hat[,'upr']), alpha=0.25) +
    geom_point(data=new.durcent, mapping=aes(x=centrality, y=avg.duration, shape=complexity), color='black', size=1) +
    geom_point(data=new.durcent, mapping=aes(x=centrality, y=avg.duration, shape=complexity,
    color=complexity, alpha=weight.plot
    ), size=0.75) +
    # geom_line(data=newdata, mapping=aes(x=centrality, y=y.hat.lm), linetype=2) +
    geom_line(data=newdata, mapping=aes(x=centrality, y=y.hat[,'fit'])) +
    scale_color_complexity() +
    scale_shape_complexity() +
    labs(x=NULL, y=NULL) +
    # scale_x_continuous(breaks=0:max(new.durcent$centrality)) +
    # scale_y_continuous(labels=math_format(10^.x, format=scales::number_format(accuracy=0.1))) +
    theme(legend.position='none', text=element_text(size=8), panel.grid=element_blank())
  if (!plot.ylin)
    #   p.out = p.out + scale_y_log10()
    # else
    p.out = p.out + scale_y_continuous(labels=math_format(10^.x, format=scales::number_format(accuracy=0.1)))
  p.out
}

leg = get_legend(ggplot(durcent %>% mutate(complexity=factor(complexity, levels=c('Basic', 'Advanced', 'Complex'))), mapping=aes(x=centrality, y=avg.duration, group=complexity)) +
                   geom_point(aes(color=complexity, shape=complexity)) +
                   geom_point(aes(shape=complexity)) +
                   scale_color_complexity() +
                   scale_shape_complexity() +
                   theme(legend.key.size=unit(4, 'mm')))
col_lab1 = ggplot() + annotate('text', label='Configuration', x=1, y=1, size=3) + coord_cartesian(clip='off') + theme_void()
col_lab2 = ggplot() + annotate('text', label='Pricing', x=1, y=1, size=3) + coord_cartesian(clip='off') + theme_void()

durcent.xlab = 'Centrality'; durcent.ylab = 'Avg. Task Processing Time (min.)'
p1 = fn.lmer.durcent.expl(formula(avg.duration ~ centrality + I(centrality^2)),
                          durcent %>% filter(complexity=='Basic' & role=='Configuration') %>% filter(avg.duration < 1000),
                          plot.ylin=T,
                          omit.bid=c(256))
save.plot('duration-centrality-lin-p1', width=plot.width.small.full, height=plot.width.small.full)
p2 = fn.lmer.durcent.expl(formula(avg.duration ~ centrality + I(centrality^2)),
                          durcent %>% filter(complexity=='Advanced' & role=='Configuration') %>% filter(avg.duration< 2000),
                          plot.ylin=T,
                          omit.bid=c(256))
save.plot('duration-centrality-lin-p2', width=plot.width.small.full, height=plot.width.small.full)
p3 = fn.lmer.durcent.expl(formula(avg.duration ~ centrality),
                          durcent %>% filter(complexity=='Complex' & role=='Configuration'), plot.ylin=T,
                          omit.bid=c(256))
p4 = fn.lmer.durcent.expl(formula(avg.duration ~ centrality + I(centrality^2)),
                          durcent %>% filter(complexity=='Basic' & role=='Pricing'), plot.ylin=T,)
p5 = fn.lmer.durcent.expl(formula(avg.duration ~ centrality),
                          durcent %>% filter(complexity=='Complex' & role=='Pricing'), plot.ylin=T,)
fig = ggarrange(col_lab1, col_lab2, p1, p4, p2, leg, p3, p5, ncol=2, nrow=4, heights=c(0.15,1,1,1))
fig = ggarrange(col_lab1, p1, p2, p3, col_lab2, p4, leg, p5, ncol=4, nrow=2, widths=c(0.1,1,1,1))
annotate_figure(ggplotGrob(fig),
                left=text_grob(durcent.ylab, rot=90, size=8, lineheight=1),
                bottom=text_grob(durcent.xlab, size=8, lineheight=1))
save.plot('duration-centrality-lin', width=plot.width.small.full, height=4.25)

p1 = fn.lmer.durcent.expl(formula(avg.duration ~ centrality),
                          durcent %>% filter(complexity=='Basic' & role=='Configuration') %>% mutate(avg.duration=log10(avg.duration)),
                          omit.bid=NULL)
p1 + labs(x='Centrality', y='AVg. Task Processing Time (min.)')
save.plot('duration-centrality-p1', width=plot.width.small.full, height=plot.width.small.full)
p2 = fn.lmer.durcent.expl(formula(avg.duration ~ centrality + I(centrality^2)),
                          durcent %>% filter(complexity=='Advanced' & role=='Configuration') %>% mutate(avg.duration=log10(avg.duration)),
                          omit.bid=NULL)
p2 + labs(x='Centrality', y='AVg. Task Processing Time')
save.plot('duration-centrality-p2', width=plot.width.small.full, height=plot.width.small.full)
p3 = fn.lmer.durcent.expl(formula(avg.duration ~ centrality + I(centrality^2)),
                          durcent %>% filter(complexity=='Complex' & role=='Configuration') %>% mutate(avg.duration=log10(avg.duration)),
                          omit.bid=NULL)
p3 + labs(x='Centrality', y='AVg. Task Processing Time')
save.plot('duration-centrality-p3', width=plot.width.small.full, height=plot.width.small.full)
p4 = fn.lmer.durcent.expl(formula(avg.duration ~ centrality + I(centrality^2)),
                     durcent %>% filter(complexity=='Basic' & role=='Pricing') %>% mutate(avg.duration=log10(avg.duration)))
p4 + labs(x='Centrality', y='AVg. Task Processing Time')
save.plot('duration-centrality-p4', width=plot.width.small.full, height=plot.width.small.full)
p5 = fn.lmer.durcent.expl(formula(avg.duration ~ centrality),
                     durcent %>% filter(complexity=='Complex' & role=='Pricing') %>% mutate(avg.duration=log10(avg.duration)))

fig = ggarrange(col_lab1, col_lab2, p1, p4, p2, leg, p3, p5, ncol=2, nrow=4, heights=c(0.15,1,1,1))
fig = ggarrange(col_lab1, p1, p2, p3, col_lab2, p4, leg, p5, ncol=4, nrow=2, widths=c(0.1,1,1,1))
annotate_figure(ggplotGrob(fig),
                left=text_grob(durcent.ylab, rot=90, size=8, lineheight=1),
                bottom=text_grob(durcent.xlab, size=8, lineheight=1))
save.plot('duration-centrality', width=plot.width.small.full, height=4.25)

###
# Tie Strength
###

### Frequency 

df.ties = dat.interact %>%
  count(BID1=pmin(sender.id, local.id), BID2=pmax(sender.id, local.id))
ggplot(data=df.ties, mapping=aes(x=n)) +
  geom_histogram(binwidth=1, color='black') +
  lims(x=c(0, 300)) +
  labs(x='Number of pair interactions', y='Frequency') +
  scale_fill_brewer('Time', palette='Spectral')
g.ties = as_tbl_graph(df.ties) %E>%
  mutate(tie.strength=factor(ifelse(n <= 7, 'weak', 'strong'), levels=c('weak', 'strong')))
ggraph(g.ties, layout='kk') +
  geom_edge_fan(aes(color=tie.strength)) +
  geom_node_point()
ggraph(g.ties %E>% filter(tie.strength=='weak'), layout='kk') +
  geom_edge_fan(aes(color=tie.strength)) +
  geom_node_point()
ggraph(g.ties %E>% filter(tie.strength=='strong'), layout='kk') +
  geom_edge_fan(aes(color=tie.strength)) +
  geom_node_point()

# Tie strength (via number of interactions with another employee) frequency
df.ties.freq = df.ties %>% pivot_longer(cols=c(BID1, BID2), values_to='BID') %>% count(BID, n, name='freq')

ties.median = median(df.ties.freq$n)

### Duration (Weak ties)

df.ties.perf = df.ties.freq %>%
  mutate(tie.strength=factor(ifelse(n <= ties.median, 'weak', 'strong'), levels=c('weak', 'strong'))) %>%
  group_by(BID, tie.strength) %>% summarize(freq=sum(freq)) %>%
  filter(tie.strength=='weak') %>%
  ungroup() %>%
  left_join(role) %>%
  group_by(role) %>%
  # mutate(tie.count=factor(Vectorize(function(x) which.min(abs(x - quantile(freq)[2:4])))(freq),
  #                         labels=c('low', 'moderate', 'high'))) %>%
  mutate(tie.count=Vectorize(function(x) which.min(abs(x - quantile(freq)[2:4])))(freq)) %>%
  ungroup() %>%
  left_join(transaction.filt %>%
              # filter(as_date(close.date) < ymd('2007-04-07', tz=time.zone)) %>%
              group_by(closed.by, complexity) %>%
              # filter(duration.day < quantile(duration.day, 0.75)+1.5*IQR(duration.day)) %>%
              summarize(avg.duration=mean(duration.day), var.duration=var(duration.day),
                        med.duration=median(duration.day), mad.duration=mad(duration.day),
                        n.task=n()), by=c('BID'='closed.by')) %>%
  drop_na()

ggplot(df.ties.perf, mapping=aes(x=tie.count, y=avg.duration)) +
  facet_wrap(role ~ complexity, scales='free') +
  labs(x='Number of weak ties', y='Duration') +
  geom_point()

fn.lm.df.ties.expl = function(f, df.tieperf, omit.bid=NULL, plot.ylin=F) {
  model.tieperf.lm <<- lm(formula=f, data=df.tieperf)
  print(summary(model.tieperf.lm)); print(coeftest(model.tieperf.lm, vcov=sandwich)); cat(red(strrep('-', 25)), '\n')
  
  res = abs(resid(model.tieperf.lm)); fit = fitted(model.tieperf.lm)
  model.tieperf.resid.lm = lm(res ~ fit)
  res.hat = fitted(model.tieperf.resid.lm)
  df.tieperf$weight = res.hat^2
  
  tmp.tieperf <<- df.tieperf  # workaround for influence.ME::influence
  model.tieperf <<- lm(formula=f, weights=1/var.duration, data=tmp.tieperf)
  model.tieperf.smmy = summary(model.tieperf)
  print(model.tieperf.smmy); print(coeftest(model.tieperf, vcov=sandwich)); cat(red(strrep('-', 25)), '\n')
  model.tieperf.cd = cooks.distance(model.tieperf)
  
  # par(mfrow=c(2,2))
  # plot(model.tieperf)
  # par(mfrow=c(1,1))
  
  p = ggplot(mapping=aes(x=factor(df.tieperf$BID), y=model.tieperf.cd)) +
    geom_point() +
    geom_hline(yintercept=3*mean(model.tieperf.cd), color='red') +
    geom_hline(yintercept=4/length(model.tieperf.cd), color='blue') +
    geom_text(aes(label=ifelse(model.tieperf.cd > 4/length(model.tieperf.cd), paste(1:length(model.tieperf.cd), format(df.tieperf$avg.duration, digits=6), sep=';'), '')), hjust=-.1, vjust=-.1)
  print(p)
  
  new.tieperf = df.tieperf
  print(new.tieperf); print(model.tieperf.cd); print(df.tieperf[as.vector(model.tieperf.cd > 4/length(model.tieperf.cd)),]$BID); print(df.tieperf[as.vector(model.tieperf.cd > 3*mean(model.tieperf.cd, na.rm=T)),]$BID); cat(red(strrep('-', 25)), '\n')
  
  if (!is.null(omit.bid)) {
    new.tieperf = new.tieperf %>% filter(!(BID %in% omit.bid))
    print(new.tieperf)
    model.tieperf <<- lm(formula=f, weights=1/var.duration, data=new.tieperf)
    model.tieperf.smmy = summary(model.tieperf)
    print(model.tieperf.smmy); print(coeftest(model.tieperf, vcov=sandwich)); cat(red(strrep('-', 25)), '\n')
  }
  
  newdata = data.frame(freq=seq(min(new.tieperf$freq, na.rm=T), max(new.tieperf$freq, na.rm=T), 0.1))
  y.hat.lm = predict(model.tieperf.lm, newdata=newdata)
  y.hat = predict(model.tieperf, newdata=newdata, interval='confidence')
  p.out = ggplot() +
    # geom_ribbon(data=newdata, mapping=aes(x=freq, ymin=y.hat[,'lwr'], ymax=y.hat[,'upr']), alpha=0.25) +
    geom_point(data=new.tieperf, mapping=aes(x=freq, y=avg.duration, shape=complexity), color='black', size=1) +
    geom_point(data=new.tieperf, mapping=aes(x=freq, y=avg.duration, color=complexity, shape=complexity, alpha=1-(var.duration-min(var.duration))/(max(var.duration)-min(var.duration))), size=0.75) +
    # geom_text(data=new.tieperf, mapping=aes(x=freq, y=avg.duration, label=BID), nudge_x=0.1, nudge_y=0.1) +
    # geom_line(data=newdata, mapping=aes(x=freq, y=y.hat.lm), linetype=2) +
    geom_line(data=newdata, mapping=aes(x=freq, y=y.hat[,'fit'])) +
    scale_color_complexity() +
    scale_shape_complexity() +
    # scale_shape_manual('Task Difficulty', values=5) +
    labs(x=NULL, y=NULL) +
    scale_x_continuous(breaks=0:max(new.tieperf$freq, na.rm=T))
  if (!plot.ylin)
    p.out = p.out + scale_y_continuous(labels=math_format(10^.x, format=scales::number_format(accuracy=0.1)))
  p.out = p.out + theme(legend.position='none', text=element_text(size=8), panel.grid=element_blank())
  return(p.out)
}
p1 = fn.lm.df.ties.expl(formula(avg.duration ~ freq + I(freq^2)),
                        df.ties.perf %>% filter(role=='Configuration' & complexity=='Basic'), plot.ylin=T,
                        omit.bid=c(256))
p2 = fn.lm.df.ties.expl(formula(avg.duration ~ freq),
                   df.ties.perf %>% filter(role=='Configuration' & complexity=='Advanced'), plot.ylin=T,
                   omit.bid=c(256,264))
p3 = fn.lm.df.ties.expl(formula(avg.duration ~ freq),
                   df.ties.perf %>% filter(role=='Configuration' & complexity=='Complex'), plot.ylin=T,
                   omit.bid=c(256))
p4 = fn.lm.df.ties.expl(formula(avg.duration ~ freq + I(freq^2)),
                   df.ties.perf %>% filter(role=='Pricing' & complexity=='Basic'), plot.ylin=T,
                   omit.bid=NULL)
p5 = fn.lm.df.ties.expl(formula(avg.duration ~ freq),
                   df.ties.perf %>% filter(role=='Pricing' & complexity=='Complex'), plot.ylin=T,
                   omit.bid=NULL)
fig = ggarrange(col_lab1, col_lab2, p1, p4, p2, leg, p3, p5, ncol=2, nrow=4, heights=c(0.15,1,1,1))
annotate_figure(ggplotGrob(fig),
                left=text_grob("Avg. Task Processing Time (min.)", rot=90, size=8, lineheight=1),
                bottom=text_grob("Number of Weak Ties", size=8, lineheight=1))
save.plot('duration-weakties-lin', width=plot.width.small.full, height=4.25)

p1 = fn.lm.df.ties.expl(formula(avg.duration ~ freq + I(freq^2)),
                        df.ties.perf %>% filter(role=='Configuration' & complexity=='Basic') %>% mutate(avg.duration=log10(avg.duration), var.duration=log10(var.duration)),
                        omit.bid=c(256))
p1 + labs(x='Number of Weak Ties', y='AVg. Task Processing Time')
save.plot('duration-weakties-p1', width=plot.width.small.full, height=plot.width.small.full)
p2 = fn.lm.df.ties.expl(formula(avg.duration ~ freq),
                        df.ties.perf %>% filter(role=='Configuration' & complexity=='Advanced') %>% mutate(avg.duration=log10(avg.duration), var.duration=log10(var.duration)),
                        omit.bid=c(256,264))
p3 = fn.lm.df.ties.expl(formula(avg.duration ~ freq),
                        df.ties.perf %>% filter(role=='Configuration' & complexity=='Complex') %>% mutate(avg.duration=log10(avg.duration), var.duration=log10(var.duration)),
                        omit.bid=c(256))
p4 = fn.lm.df.ties.expl(formula(avg.duration ~ freq + I(freq^2)),
                        df.ties.perf %>% filter(role=='Pricing' & complexity=='Basic') %>% mutate(avg.duration=log10(avg.duration), var.duration=log10(var.duration)),
                        omit.bid=NULL)
p4 + labs(x='Number of Weak Ties', y='AVg. Task Processing Time')
save.plot('duration-weakties-p4', width=plot.width.small.full, height=plot.width.small.full)
p5 = fn.lm.df.ties.expl(formula(avg.duration ~ freq),
                        df.ties.perf %>% filter(role=='Pricing' & complexity=='Complex') %>% mutate(avg.duration=log10(avg.duration), var.duration=log10(var.duration)),
                        omit.bid=NULL)
fig = ggarrange(col_lab1, col_lab2, p1, p4, p2, leg, p3, p5, ncol=2, nrow=4, heights=c(0.15,1,1,1))
annotate_figure(ggplotGrob(fig),
                left=text_grob("Avg. Task Processing Time (min.)", rot=90, size=8, lineheight=1),
                bottom=text_grob("Number of Weak Ties", size=8, lineheight=1))
save.plot('duration-weakties', width=plot.width.small.full, height=4.25)

ggplot(df.ties.perf %>% group_by(role, tie.count) %>% summarize(avg.duration=median(avg.duration)),
       mapping=aes(x=tie.count, y=avg.duration)) +
  facet_wrap(role ~ ., scales='free') +
  labs(x='Number of weak ties', y='Duration') +
  geom_point()

### Duration (Strong ties)

df.ties.perf = df.ties.freq %>%
  mutate(tie.strength=factor(ifelse(n <= ties.median, 'weak', 'strong'), levels=c('weak', 'strong'))) %>%
  group_by(BID, tie.strength) %>% summarize(freq=sum(freq)) %>%
  filter(tie.strength=='strong') %>%
  ungroup() %>%
  left_join(role) %>%
  group_by(role) %>%
  mutate(tie.count=Vectorize(function(x) which.min(abs(x - quantile(freq)[2:4])))(freq)) %>%
  ungroup() %>%
  left_join(transaction.filt %>%
              # filter(as_date(close.date) < ymd('2007-04-07', tz=time.zone)) %>%
              group_by(closed.by, complexity) %>%
              filter(duration.day < quantile(duration.day, 0.75)+1.5*IQR(duration.day)) %>%
              summarize(avg.duration=mean(duration.day), var.duration=var(duration.day),
                        med.duration=median(duration.day), mad.duration=mad(duration.day),
                        n.task=n()), by=c('BID'='closed.by')) %>%
  drop_na()

ggplot(df.ties.perf, mapping=aes(x=tie.count, y=avg.duration)) +
  facet_wrap(role ~ ., scales='free') +
  labs(x='Number of strong ties', y='Avg. Duration') +
  geom_point()

ggplot(df.ties.perf, mapping=aes(x=freq, y=avg.duration)) +
  facet_wrap(role ~ complexity, scales='free') +
  labs(x='Number of strong ties', y='Avg. Duration') +
  geom_point() +
  geom_smooth(method=lm) +
  geom_smooth(method=MASS::rlm, color='red')

p1 = fn.lm.df.ties.expl(formula(avg.duration ~ freq),
                        df.ties.perf %>% filter(role=='Configuration' & complexity=='Basic'), plot.ylin=T,
                        omit.bid=c(256,105,281))
p2 = fn.lm.df.ties.expl(formula(avg.duration ~ freq),
                        df.ties.perf %>% filter(role=='Configuration' & complexity=='Advanced'), plot.ylin=T,
                        omit.bid=c(256,264,105))
p3 = fn.lm.df.ties.expl(formula(avg.duration ~ freq),
                        df.ties.perf %>% filter(role=='Configuration' & complexity=='Complex'), plot.ylin=T,
                        omit.bid=c(256))
p4 = fn.lm.df.ties.expl(formula(avg.duration ~ freq),
                        df.ties.perf %>% filter(role=='Pricing' & complexity=='Basic'), plot.ylin=T,
                        omit.bid=NULL)
p5 = fn.lm.df.ties.expl(formula(avg.duration ~ freq),
                        df.ties.perf %>% filter(role=='Pricing' & complexity=='Complex'), plot.ylin=T,
                        omit.bid=NULL)
fig = ggarrange(col_lab1, col_lab2, p1, p4, p2, leg, p3, p5, ncol=2, nrow=4, heights=c(0.15,1,1,1))
annotate_figure(ggplotGrob(fig),
                left=text_grob("Avg. Task Processing Time (min.)", size=8, rot=90),
                bottom=text_grob("Number of Strong Ties", size=8))
save.plot('duration-strongties-lin', width=plot.width.small.full, height=4.25)

p1 = fn.lm.df.ties.expl(formula(avg.duration ~ freq),
                        df.ties.perf %>% filter(role=='Configuration' & complexity=='Basic') %>% mutate(avg.duration=log10(med.duration), var.duration=log10(mad.duration+2)),
                        omit.bid=c(256,105,281))
p2 = fn.lm.df.ties.expl(formula(avg.duration ~ freq),
                        df.ties.perf %>% filter(role=='Configuration' & complexity=='Advanced') %>% mutate(avg.duration=log10(med.duration), var.duration=log10(mad.duration+2)),
                        omit.bid=c(256,264,105))
p3 = fn.lm.df.ties.expl(formula(avg.duration ~ freq),
                        df.ties.perf %>% filter(role=='Configuration' & complexity=='Complex') %>% mutate(avg.duration=log10(med.duration), var.duration=log10(mad.duration+2)),
                        omit.bid=c(256))
p4 = fn.lm.df.ties.expl(formula(avg.duration ~ freq),
                        df.ties.perf %>% filter(role=='Pricing' & complexity=='Basic') %>% mutate(avg.duration=log10(med.duration), var.duration=log10(mad.duration+2)),
                        omit.bid=NULL)
p5 = fn.lm.df.ties.expl(formula(avg.duration ~ freq),
                        df.ties.perf %>% filter(role=='Pricing' & complexity=='Complex') %>% mutate(avg.duration=log10(med.duration), var.duration=log10(mad.duration+2)),
                        omit.bid=NULL)
fig = ggarrange(col_lab1, col_lab2, p1, p4, p2, leg, p3, p5, ncol=2, nrow=4, heights=c(0.15,1,1,1))
annotate_figure(ggplotGrob(fig),
                left=text_grob("Avg. Task Processing Time (min.)", size=8, rot=90),
                bottom=text_grob("Number of Strong Ties", size=8))
save.plot('duration-strongties', width=plot.width.small.full, height=4.25)

ggplot(df.ties.perf %>% group_by(role, tie.count) %>% summarize(avg.duration=median(avg.duration)),
       mapping=aes(x=tie.count, y=avg.duration)) +
  facet_wrap(role ~ ., scales='free') +
  labs(x='Number of strong ties', y='Avg. Duration') +
  geom_point()

###
# Number of within group interactions
###

df.interact.role = ir.filt %>%
  left_join(role, by=c('sender.id'='BID')) %>%
  left_join(role, by=c('local.id'='BID'), suffix=c('.sender', '.local'))
# df.interact.role = df.interact.role[unique(floor(runif(10, min=0, max=nrow(df.interact.role)))),]  # Select 10 random

# Interaction counts ignoring order of BID
df.interact.role %>%
  mutate(role.sender=replace_na(role.sender, "NA"), role.local=replace_na(role.local, "NA"),
         key.role=paste(pmin(role.sender, role.local), pmax(role.sender, role.local), sep=';')) %>%
  separate(key.role, c('role.1', 'role.2'), sep=';') %>%
  group_by(role.1, role.2) %>%
  summarize(n.interact=n()) #%>%
# pivot_wider(id_cols=role.1, names_from=role.2, values_from=n.interact)

# Interaction counts considering order of BID
df.interact.role %>% ungroup() %>%
  mutate(role.sender=replace_na(role.sender, "NA"), role.local=replace_na(role.local, "NA"),
         key.id=paste(pmin(sender.id, local.id), pmax(sender.id, local.id), sep=';'),
         key.role=paste(pmin(role.sender, role.local), pmax(role.sender, role.local), sep=';')) %>%
  distinct(key.id, .keep_all=T) %>%
  # separate(key.id, c('sender', 'to'), sep=';') %>%
  separate(key.role, c('role.1', 'role.2'), sep=';') %>%
  group_by(role.1, role.2) %>%
  summarize(n.interact=n()) #%>%
# pivot_wider(id_cols=role.1, names_from=role.2, values_from=n.interact)

fn.total.n = function(role.1, role.2) {
  role.count = role %>% count(role) %>% column_to_rownames('role')
  if (role.1 == role.2) {
    return(choose(role.count[role.1,], 2))
  } else {
    return(role.count[role.1,] * role.count[role.2,])
  }
}

df.interact.role.cont.tbl = df.interact.role %>% ungroup() %>%
  mutate(role.sender=replace_na(role.sender, "NA"), role.local=replace_na(role.local, "NA"),
         key.id=paste(pmin(sender.id, local.id), pmax(sender.id, local.id), sep=';'),
         key.role=paste(pmin(role.sender, role.local), pmax(role.sender, role.local), sep=';')) %>%
  distinct(key.id, .keep_all=T) %>%
  # separate(key.id, c('sender', 'to'), sep=';') %>%
  separate(key.role, c('role.1', 'role.2'), sep=';') %>%
  group_by(role.1, role.2) %>%
  summarize(n.interact=n()) %>%
  rowwise() %>%
  mutate(total.n=fn.total.n(role.1, role.2), n.nointeract=total.n-n.interact)

library(mosaic)
for (i in 2:6) {
  print(oddsRatio(df.interact.role.cont.tbl[c(1, i), c('n.interact', 'n.nointeract')], verbose=T))
}

### Logistic regression on interaction

library(aod)
library(performance)

### Log-odds of interaction depending on the role of two individuals
df.logistic.role = expand_grid(BID1=unique(office$BID), BID2=unique(office$BID)) %>%
  filter(BID1 != BID2) %>%
  left_join(role, by=c('BID1'='BID')) %>%
  left_join(role, by=c('BID2'='BID'), suffix=c('.1', '.2')) %>%
  left_join(df.interact.role %>% ungroup() %>% mutate(interact=T) %>% select(sender.id, local.id, interact),
            by=c('BID1'='sender.id', 'BID2'='local.id')) %>%
  mutate(interact=replace_na(interact, F)) %>%
  mutate(role.1=replace_na(role.1, "NA"), role.2=replace_na(role.2, "NA"),
         key.role=paste(pmin(role.1, role.2), pmax(role.1, role.2), sep=';')) %>%
  # separate(key.role, c('role.1', 'role.2'), sep=';', convert=F) %>%
  select(BID1, BID2, role.1, role.2, interact, key.role)
df.logistic.role %>% filter(role.1 != 'NA', role.2 != 'NA') %>% group_by(role.1, role.2, interact) %>% summarize(n=n()) %>% filter(interact)

fit.logistic.role = glm(interact ~ key.role, data=df.logistic.role %>% filter(role.1 != 'NA', role.2 != 'NA'), family='binomial')
summary(fit.logistic.role)
wald.test(b=coef(fit.logistic.role), Sigma=vcov(fit.logistic.role), Terms=4:9)
exp(cbind(OR=coef(fit.logistic.role), confint(fit.logistic.role)))

### Log-odds of interaction depending on the role of two individuals (regardless of order)
df.logistic.role = expand_grid(BID1=unique(office$BID), BID2=unique(office$BID)) %>%
  filter(BID1 != BID2) %>%
  left_join(role, by=c('BID1'='BID')) %>%
  left_join(role, by=c('BID2'='BID'), suffix=c('.1', '.2')) %>%
  mutate(key.bid=paste(pmin(BID1, BID2), pmax(BID1, BID2), sep=';')) %>%
  distinct(key.bid, .keep_all=T) %>%
  separate(key.bid, c('BID1', 'BID2'), sep=';', convert=F) %>%
  left_join(df.interact.role %>% ungroup() %>%
              mutate(key.bid=paste(pmin(sender.id, local.id), pmax(sender.id, local.id), sep=';'), interact=T) %>%
              distinct(key.bid, .keep_all=T) %>%
              separate(key.bid, c('sender.id', 'local.id'), sep=';') %>%
              select(sender.id, local.id, interact),
            by=c('BID1'='sender.id', 'BID2'='local.id')) %>%
  mutate(interact=replace_na(interact, F)) %>%
  mutate(role.1=replace_na(role.1, "NA"), role.2=replace_na(role.2, "NA"),
         key.role=paste(pmin(role.1, role.2), pmax(role.1, role.2), sep=';')) %>%
  # separate(key.role, c('role.1', 'role.2'), sep=';', convert=F) %>%
  select(BID1, BID2, role.1, role.2, interact, key.role)
df.logistic.role %>% filter(role.1 != 'NA', role.2 != 'NA') %>% group_by(role.1, role.2, interact) %>% summarize(n=n())

fit.logistic.role = glm(interact ~ key.role, data=df.logistic.role %>% filter(role.1 != 'NA', role.2 != 'NA'), family='binomial')
summary(fit.logistic.role)
wald.test(b=coef(fit.logistic.role), Sigma=vcov(fit.logistic.role), Terms=4:9)
exp(cbind(OR=coef(fit.logistic.role), confint(fit.logistic.role)))

###
# Clustering
###
bid.ignore = c(298, 103, 291, 256)
cont.tbl = as_tibble(table(dat.interact %>% select(sender.id, local.id))) %>%
  mutate(sender.id=as.integer(sender.id), local.id=as.integer(local.id))
edge.list.inter.nosym = expand_grid(sender.id=unique(office$BID), local.id=unique(office$BID)) %>%
  left_join(cont.tbl) %>%
  mutate(n=replace_na(n, 0)) %>%
  filter(!(sender.id %in% bid.ignore | local.id %in% bid.ignore))
edge.list.inter.sym = edge.list.inter.nosym %>%
  mutate(key.id=paste(pmin(sender.id, local.id), pmax(sender.id, local.id), sep=';')) %>%
  group_by(key.id) %>%
  summarize(n=sum(n)) %>%
  separate(key.id, into=c('sender.id', 'local.id'), sep=';')
edge.list.inter.sym2 = edge.list.inter.sym %>%
  rbind(edge.list.inter.sym %>% select(sender.id=local.id, local.id=sender.id, n)) %>%
  distinct(sender.id, local.id, .keep_all=T)
adj.matrix.inter.sym = as.matrix(edge.list.inter.sym2 %>%
                                   pivot_wider(id_cols=sender.id, names_from=local.id, values_from=n) %>%
                                   column_to_rownames('sender.id'))
adj.matrix.inter.nosym = as.matrix(edge.list.inter.nosym %>%
                                     pivot_wider(id_cols=sender.id, names_from=local.id, values_from=n) %>%
                                     column_to_rownames('sender.id'))

adj.matrix.inter = adj.matrix.inter.sym
adj.matrix.inter[adj.matrix.inter >= 1] = 1

### Cosine
similarity = (adj.matrix.inter / sqrt(rowSums(adj.matrix.inter * adj.matrix.inter))) %*% t(adj.matrix.inter / sqrt(rowSums(adj.matrix.inter * adj.matrix.inter)))
similarity = replace(similarity, is.na(similarity), 0)
diag(similarity) = 1
distance.cos = as.dist(1-similarity)
### Euclidean
distance.euc = dist(adj.matrix.inter, method='euclidean')

### Hierarchical

k = 3  # For the number of roles

# Using cosine similarity
cluster.cos = hclust(distance.cos, method='ward.D2')
cluster.cos$height[length(cluster.cos$height)] = 1.15
dend.full.cos = as.dendrogram(cluster.cos)
groups.cos = cutree(cluster.cos, k=k)
df.groups.cos = tibble(BID=as.integer(names(groups.cos)), Group=groups.cos)
plot(cluster.cos, hang=-1)  # snippet

dend.color.role = tibble(role=(role %>% column_to_rownames("BID"))[names(groups.cos)[order.dendrogram(dend.full.cos)],]) %>%
  left_join(tibble(role=names(color.role), color=color.role)) %>%
  mutate(color=replace_na(color, "#FFFFFF"), role=replace_na(role, 'NA'))
dend.color.labels = dend.color.role$color

setEPS()
postscript(file.path(dir.output.plot, 'dend-full.eps'), width=plot.width.large, height=4.5)
png(file.path(dir.output.plot, 'dend-full.png'), width=plot.width.large, height=plot.width.small.full, units='in', res=300)
par(mar=c(2,1,1,1))
dend.full.cos %>%
  # set("branches_k_color", k=3, c(2,11,20)) %>%
  set("leaves_pch", 15) %>%
  set("leaves_cex", 1.15) %>%
  set('leaves_col', dend.color.labels) %>%
  set("labels_cex", 0.65) %>%
  plot(axes=F, xaxs="i")
dend.full.cos %>% rect.dendrogram(k=k, border=8, lty=5, lwd=2)
legend(x=29, y=1.18, cex=0.65, pch=15, legend=unique(dend.color.role$role), col=unique(dend.color.labels))
mtext(c('Branch 1', 'Branch 2', 'Branch 3'), at=c(4, 9.5, 23), line=0.5, side=1, cex=0.65)
dev.off()

library(gplots)

dend.hm = dend.full.cos %>%
  # set("branches_k_color", k=3, c(2,11,20)) %>%
  set("leaves_pch", 15) %>%
  set("leaves_cex", 1.15) %>%
  set('leaves_col', dend.color.labels) %>%
  set("labels_cex", 0.65)
fn.hmcolor.wrapper = function(n) c('#FFFFFF', brewer_pal(palette='Reds')(n))[1:n]

setEPS()
postscript(file.path(dir.output.plot, 'dend-heatmap.eps'), width=plot.width.small.full, height=plot.width.small.full)
png(file.path(dir.output.plot, 'dend-heatmap.png'), width=plot.width.small.full, height=plot.width.small.full, units='in', res=300)
heatmap.2(log(adj.matrix.inter.sym+1), Rowv=dend.hm, Colv='Rowv', trace='none', key=F, margins=c(1.25,1.25), col=fn.hmcolor.wrapper, cexRow=0.45, cexCol=0.45, offsetRow=0, offsetCol=0)
dev.off()

###
# Interactions within groups from clustering
###

dat.interact.cluster = dat.interact %>%
  left_join(df.groups.cos, by=c('sender.id'='BID')) %>%
  left_join(df.groups.cos, by=c('local.id'='BID'), suffix=c('.sender', '.local'))

# Branch 1 interacts with one another more than 70% of the time
dat.interact.cluster %>% filter(Group.sender == 1 | Group.local == 1) %>% summarize(n=n())
dat.interact.cluster %>% filter(Group.sender == 1 & Group.local == 1) %>% summarize(n=n())
# Branch 2 interacts with one another about 50% of the time
dat.interact.cluster %>% filter(Group.sender == 2 | Group.local == 2) %>% summarize(n=n())
dat.interact.cluster %>% filter(Group.sender == 2 & Group.local == 2) %>% summarize(n=n())
# Branch 3 have few interactions and none among themselves
dat.interact.cluster %>% filter(Group.sender == 3 | Group.local == 3) %>% summarize(n=n())
dat.interact.cluster %>% filter(Group.sender == 3 & Group.local == 3) %>% summarize(n=n())

###
# Minimum bounding envelope
###

mbe.df = function(df, delta, epsilon) {
  bound.upper = Vectorize(
    function(x, ref) {
      row_idx = seq(max(x - delta, 1), min(x + delta, nrow(df)))
      max(ref[row_idx] + epsilon, na.rm=T)
    },
    c('x')
  )
  bound.lower = Vectorize(
    function(x, ref) {
      row_idx = seq(max(x - delta, 1), min(x + delta, nrow(df)))
      min(ref[row_idx] - epsilon, na.rm=T)
    },
    c('x')
  )

  df %>%
    drop_na(x.query, y.query) %>%
    mutate(high.a.x=bound.upper(row_number(), x.query), low.a.x=bound.lower(row_number(), x.query),
           high.a.y=bound.upper(row_number(), y.query), low.a.y=bound.lower(row_number(), y.query),
           mask = x.seq <= high.a.x & x.seq >= low.a.x & y.seq <= high.a.y & y.seq >= low.a.y)
}

tmp = badge.loc %>%
  filter((id==258|id==290) & date.time %within% interval(ymd_hms('2007-03-29 08:40:00', tz=time.zone), ymd_hms('2007-03-29 09:40:00', tz=time.zone))) %>%
  group_by(id, date.time) %>%
  summarize(x=mean(x), y=mean(y)) %>%
  mutate(id=factor(id))
tmp.mbe = mbe.df(full_join(tmp[tmp$id==258,c('x','y','date.time')],
                           tmp[tmp$id==290,c('x','y','date.time')],
                           by='date.time', suffix=c('.query', '.seq')),
                 delta=2, epsilon=500)
ggplot(data=tmp.mbe %>% drop_na(), mapping=aes(x=1:nrow(tmp.mbe %>% drop_na()))) +
  geom_ribbon(mapping=aes(ymin=low.a.y, ymax=high.a.y), fill='grey70') +
  geom_line(mapping=aes(y=y.query), color='red') +
  geom_line(mapping=aes(y=y.seq), color='blue') +
  labs(x='Time', y='y') +
  # scale_y_continuous(labels=NULL) +
  scale_x_continuous(breaks=seq(5, nrow(tmp.mbe), 5), labels=scales::number_format(accuracy=1)) +
  geom_segment(mapping=aes(x=23, xend=27, y=1600, yend=1600), arrow=arrow(length=unit(1, 'pt'), ends='both', type='closed')) +
  geom_segment(mapping=aes(x=25, xend=25, y=1800, yend=2100), arrow=arrow(length=unit(1, 'pt'), ends='both', type='closed')) +
  annotate('text', x=25, y=1500, label=expression(2*delta), size=2) +
  annotate('text', x=26, y=1950, label=expression(epsilon), size=2) +
  annotate('text', x=1, y=1850, label='Q(t)', size=2) +
  annotate('text', x=1, y=4750, label='T(t)', size=2) +
  theme(text=element_text(size=8), panel.grid=element_blank())
save.plot('mbe', width=plot.width.small, height=plot.width.small)

###
# Task Counts
###

tmp.transcomp.c = transaction.filt %>%
  filter(role=='Configuration') %>%
  mutate(complexity=factor(complexity, levels=c('Basic', 'Advanced', 'Complex'), labels=c('B', 'A', 'C'))) %>%
  count(assigned.to, complexity, .drop=F)
tmp.transcomp.p = transaction.filt %>%
  filter(role=='Pricing') %>%
  mutate(complexity=factor(complexity, levels=c('Basic', 'Advanced', 'Complex'), labels=c('B', 'A', 'C'))) %>%
  count(assigned.to, complexity, .drop=F)
tmp.transcomp.p1 = ggplot(tmp.transcomp.c, mapping=aes(x=complexity, y=n, fill=complexity)) +
  facet_wrap(~assigned.to, ncol=10, scales='free_y') +
  geom_bar(position='dodge',stat='identity') +
  scale_y_continuous(breaks=function(l) max(l), labels=scales::number_format(accuracy=1)) +
  labs(x=NULL, y=NULL) +
  theme(legend.position='none', panel.grid=element_blank(),
        strip.background=element_rect(fill='transparent', color='transparent'))
save.plot('task-count-config', width=plot.width.large, height=2)

tmp.transcomp.p2 = ggplot(tmp.transcomp.p, mapping=aes(x=complexity, y=n, fill=complexity)) +
  facet_wrap(~assigned.to, ncol=10, scales='free_y') +
  geom_bar(position='dodge',stat='identity') +
  scale_y_continuous(breaks=function(l) max(l), labels=scales::number_format(accuracy=1)) +
  labs(x=NULL, y=NULL) +
  theme(legend.position='none', panel.grid=element_blank(),
        strip.background=element_rect(fill='transparent', color='transparent'))
save.plot('task-count-pricing', width=0.7*plot.width.large, height=1)
