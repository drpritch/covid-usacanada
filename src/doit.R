library('sf');
library('dplyr');
library('ggplot2');
library('ggpubr');

#theCoords <- 26917; #UTM17N
#theCoords <- 4269; # NAD 83
#theCoords <- 4326; # WGS 84
theCoords <- 3347; # StatsCan Lambert
theCoords <- st_crs(theCoords);
# Week 47: Nov. 15-21
theWeek <- 47;
#provinceFilter <- c('Saskatchewan', 'Manitoba','Ontario','Quebec');
provinceUnfilter <- c('Yukon', 'NWT', 'Nunavut');
#stateFilter <- c('Minnesota','Wiscosin','Michigan','Ohio','Pennsylvania','New York','Vermont','Massachusetts','Connecticut','Rhode Island','New Hampshire','Maine');
stateUnfilter <- c('Hawaii','Alaska','Puerto Rico','Virgin Islands','Northern Mariana Islands');

canadaFiles <- c(
  #Canada = 'HR_000a18a_e',
  NL = 'HR_010a18a_e',
  PEI = 'HR_011a18a_e',
  'Nova Scotia' = 'HR_012a18a_e',
  'New Brunswick' = 'HR_013a18a_e',
  Quebec = 'HR_024a18a_e',
  Ontario = 'HR_035a18a_e',
  Manitoba = 'HR_046a18a_e',
  Saskatchewan = 'HR_047a18a_e',
  Alberta = 'HR_048a18a_e',
  BC = 'HR_059a18a_e');
for (i in 1:length(canadaFiles)) {
  canadaFile <- canadaFiles[i];
  foo <- st_read(paste0('../input/', canadaFile, '/', canadaFile, ".shp"));
  foo$province <- names(canadaFiles)[i];
  if (canadaFile == canadaFiles[1]) {
    canadaGeo <- foo;
  } else {
    canadaGeo <- rbind(canadaGeo, foo);
  }
}
canadaCases <- read.csv('../input/cases_timeseries_hr.csv');
levels(canadaCases$health_region) <- c(levels(canadaCases$health_region),
    'Central Alberta', 'Central Saskatchewan',
    'North Alberta', 'North Saskatchewan',
    'South Alberta', 'South Saskatchewan',
    'Eastern Ontario', 'Eastern NL',
    'Northern Manitoba', 'Northern BC');
disambiguateRegion <- function(canadaCases, region, province1, province2) {
  canadaCases[canadaCases$health_region == region & canadaCases$province == province1,]$health_region = paste0(region, ' ', province1);
  canadaCases[canadaCases$health_region == region & canadaCases$province == province2,]$health_region = paste0(region, ' ', province2);
  canadaCases;
}
canadaCases <- disambiguateRegion(canadaCases, 'Central', 'Alberta', 'Saskatchewan');
canadaCases <- disambiguateRegion(canadaCases, 'North', 'Alberta', 'Saskatchewan');
canadaCases <- disambiguateRegion(canadaCases, 'South', 'Alberta', 'Saskatchewan');
canadaCases <- disambiguateRegion(canadaCases, 'Eastern', 'Ontario', 'NL');
canadaCases <- disambiguateRegion(canadaCases, 'Northern', 'Manitoba', 'BC');

canadaGeo$province <- factor(canadaGeo$province, levels=levels(canadaCases$province));
# Convert from factor to number.
canadaGeo$HR_UID <- levels(canadaGeo$HR_UID)[as.numeric(canadaGeo$HR_UID)];
canadaGeo$health_region <- forcats::fct_recode(canadaGeo$HR_UID,
  !!!unlist(lapply(list(
    'South Alberta'=4831,
    Calgary=4832,
    'Central Alberta'=4833,
    Edmonton=4834,
    'North Alberta'=4835,
    
    Regina=4704,
    Saskatoon=4706,
    
    'Interlake-Eastern'=4603,
    'Northern Manitoba'=4604,
    'Prairie Mountain'=4602,
    'Southern Health'=4605,
    Winnipeg=4601,
    
    Algoma=3526,
    Brant=3527,
    'Chatham-Kent'=3540,
    Durham=3530,
    'Eastern Ontario'=3558,
    'Grey Bruce'=3533,
    'Haldimand-Norfolk'=3534,
    'Haliburton Kawartha Pineridge'=3535,
    Halton=3536,
    Hamilton=3537,
    'Hastings Prince Edward'=3538,
  #  'Huron Perth'=???, # Huron county=3539, Perth=3554
    "Kingston Frontenac Lennox & Addington"=3541,
    Lambton=3542,                      
    "Leeds Grenville and Lanark"=3543,
    "Middlesex-London"=3544,     
    Niagara=3546,
    "North Bay Parry Sound"=3547,
    Northwestern=3549,
  #  "Not Reported"                         
    Ottawa=3551,
    Peel=3553,
    Peterborough=3555,
    Porcupine=3556,
    Renfrew=3557,
    "Simcoe Muskoka"=3560,                  
    Southwestern=3575, # Oxford Elgin St Thomas=3575
    Sudbury=3561,
    "Thunder Bay"=3562,
    Timiskaming=3563,
    Toronto=3595,
    Waterloo=3565,
    "Wellington Dufferin Guelph"=3566,
    "Windsor-Essex"=3568,
    York=3570,
  
  "Abitibi-Témiscamingue"=2408,
  "Bas-Saint-Laurent"=2401,
  "Capitale-Nationale"=2403,
  "Chaudière-Appalaches"=2412,        
  "Côte-Nord"=2409,
  Estrie=2405,
  "Gaspésie-Îles-de-la-Madeleine"=2411,
  Lanaudière=2414,
  Laurentides=2415,
  Laval=2413,
  Mauricie=2404,
  Montérégie=2416,
  Montréal=2406,
  "Nord-du-Québec"=2410,
  #"Not Reported"
  Nunavik=2417,
  Outaouais=2407,
  Saguenay=2402,
  "Terres-Cries-de-la-Baie-James"=2418,
  
  # PEI & NB & NS names match
  
  Central=1011,
  "Labrador-Grenfell"=1012,
  Western=1013,
  "Eastern NL"=1014),
  as.character)));

# health_region == ENGNAME for these provinces
hrFilter <-canadaGeo$province %in% c('PEI', 'New Brunswick', 'Nova Scotia');
canadaGeo$health_region <- as.character(canadaGeo$health_region);
canadaGeo$health_region[hrFilter] <- as.character(canadaGeo$ENGNAME[hrFilter]);
canadaGeo$health_region <- factor(canadaGeo$health_region);

bc <- canadaGeo %>% filter(HR_UID %in% 5911:5914) %>%
  mutate(health_region = 'Interior');
canadaGeo <- rbind(canadaGeo, st_union(st_union(bc[1,], bc[2,]),
                              st_union(bc[3,], st_make_valid(bc[4,])))[,1:7]);
bc <- canadaGeo %>% filter(HR_UID %in% 5921:5923) %>%
  mutate(health_region = 'Fraser');
canadaGeo <- rbind(canadaGeo, st_union(st_union(bc[1,], bc[2,]), bc[3,])[,1:7]);
bc <- canadaGeo %>% filter(HR_UID %in% 5931:5933) %>%
  mutate(health_region = 'Vancouver Coastal');
canadaGeo <- rbind(canadaGeo, st_union(st_union(bc[1,], bc[2,]), bc[3,])[,1:7]);
bc <- canadaGeo %>% filter(HR_UID %in% 5941:5943) %>%
  mutate(health_region = 'Island');
canadaGeo <- rbind(canadaGeo, st_union(st_union(bc[1,], bc[2,]), bc[3,])[,1:7]);
bc <- canadaGeo %>% filter(HR_UID %in% 5951:5953) %>%
  mutate(health_region = 'Northern BC');
canadaGeo <- rbind(canadaGeo, st_union(st_union(bc[1,], bc[2,]), bc[3,])[,1:7]);

sask <- canadaGeo %>% filter(HR_UID %in% 4711:4713) %>%
    mutate(health_region = 'Far North');
canadaGeo <- rbind(canadaGeo, st_union(st_union(sask[1,], sask[2,]), sask[3,])[,1:7]);
sask <- canadaGeo %>% filter(HR_UID %in% 4708:4710) %>%
    mutate(health_region = 'North Saskatchewan');
canadaGeo <- rbind(canadaGeo, st_union(st_union(sask[1,], sask[2,]), sask[3,])[,1:7]);
sask <- canadaGeo %>% filter(HR_UID %in% c(4705,4707)) %>%
  mutate(health_region = 'Central Saskatchewan');
canadaGeo <- rbind(canadaGeo, st_union(sask[1,], sask[2,])[,1:7]);
sask <- canadaGeo %>% filter(HR_UID %in% 4701:4703) %>%
  mutate(health_region = 'South Saskatchewan');
canadaGeo <- rbind(canadaGeo, st_union(st_union(sask[1,], sask[2,]), sask[3,])[,1:7]);

huronPerth <- canadaGeo %>% filter(HR_UID %in% c(3539,3554)) %>%
    mutate(health_region = 'Huron Perth');
canadaGeo <- rbind(canadaGeo, st_union(huronPerth[1,], huronPerth[2,])[,1:7]);

pops <- unlist(list(
  Fraser=295763+639245+784977,
  Interior=79856+78463+362258+219467,
  Island=270817+122233+383360,
  'Vancouver Coastal'=284389+198309+649028,
  'Northern BC'=71553+67885+140452,
  
  Calgary=1551876,
  Edmonton=1320798,
  'Central Alberta'=461553,
  'North Alberta'=441836,
  'South Alberta'=291112,
  
  'Central Saskatchewan'=55094+41890,
  'Far North'=22282+10539+2632,
  'North Saskatchewan'=39574+75637+75147,
  Regina=278281,
  Saskatoon=344458,
  'South Saskatchewan'=56230+53261+42967,
    
  'Interlake-Eastern'=127601,
  'Northern Manitoba'=72220,
  'Prairie Mountain'=165600,
  'Southern Health'=192061,
  Winnipeg=720883,
  
  Algoma=113084,
  Brant=134943,
  'Chatham-Kent'=102042,
  Durham=645862,
  'Eastern Ontario'=202762,
  'Grey Bruce'=161977,
  'Haldimand-Norfolk'=109652,
  'Haliburton Kawartha Pineridge'=179083,
  Halton=548430,
  Hamilton=536917,
  'Hastings Prince Edward'=161180,
  'Huron Perth'=59297+76796,
  "Kingston Frontenac Lennox & Addington"=193363,
  Lambton=126638,
  "Leeds Grenville and Lanark"=169244,
  "Middlesex-London"=455526,
  Niagara=447888,
  "North Bay Parry Sound"=123820,
  Northwestern=76455,
  Ottawa=934243,
  Peel=1381744,
  Peterborough=138236,
  Porcupine=84201,
  Renfrew=103593,
  "Simcoe Muskoka"=540249,                  
  Southwestern=88978+110862,
  Sudbury=196448,
  "Thunder Bay"=151884,
  Timiskaming=33049,
  Toronto=2731571,
  Waterloo=535154,
  "Wellington Dufferin Guelph"=284461,
  "Windsor-Essex"=398953,
  York=1109909,
  
  "Abitibi-Témiscamingue"=146717,
  "Bas-Saint-Laurent"=197385,
  "Capitale-Nationale"=729997,
  "Chaudière-Appalaches"=420082,        
  "Côte-Nord"=92518,
  Estrie=472615,
  "Gaspésie-Îles-de-la-Madeleine"=90311,
  Lanaudière=494796,
  Laurentides=589400,
  Laval=422993,
  Mauricie=508511,
  Montérégie=1353459,
  Montréal=1942044,
  "Nord-du-Québec"=14232,
  #"Not Reported"
  Nunavik=13188,
  Outaouais=382604,
  Saguenay=276368,
  "Terres-Cries-de-la-Baie-James"=17141,
  
  "Prince Edward Island"=142907,
  
  # New Brunswick
  "Zone 1 (Moncton area)"=209256,
  "Zone 2 (Saint John area)"=170537,
  "Zone 3 (Fredericton area)"=174348,
  "Zone 4 (Edmundston area)"=47775,
  "Zone 5 (Campbellton area)"=25250,
  "Zone 6 (Bathurst area)"=76374,
  "Zone 7 (Miramichi area)"=43561,
  
  # Nova Scotia
  "Zone 1 - Western"=194376,
  "Zone 2 - Northern"=146249,
  "Zone 3 - Eastern"=158936,
  "Zone 4 - Central"=424037,
  
  Central=92690,
  "Labrador-Grenfell"=36072,
  Western=77687,
  "Eastern NL"=313267
  ));
canadaCases$date_report <- as.Date(canadaCases$date_report, '%d-%m-%Y');
pops <- data.frame(health_region=factor(names(pops), levels=levels(canadaCases$health_region)), pop100k=pops/100000);
canadaCases <- canadaCases %>%
  left_join(pops, by='health_region');

# Week definition:
# Normally: week 1 = Jan 1...7, week 2 = Jan 8...15
# By shifting by three: week 1 = Dec 29...Jan 4 (Sun-Sat)
# and week 47 = Nov. 15-21
canadaCases <- canadaCases %>% group_by(health_region, province, pop100k, week = lubridate::week(date_report + 3)) %>%
  summarise(cases=sum(cases));

canada <- canadaCases %>%
  filter(week == theWeek) %>%
  filter(!province %in% provinceUnfilter);

canada <- canada %>%
  left_join(canadaGeo %>% st_transform(theCoords) %>% st_simplify(dTolerance=500), by = 'health_region') %>%
  select(health_region, cases, pop100k, geometry);
canada$HR_UID <- NULL;
canada$province <- canada$province.x;
canada$province.x <- NULL;
#ggplot(canada) +
#  geom_sf(aes(geometry=geometry, fill=weekly / pop100k), color='#00000010') +
#  scale_fill_fermenter(palette='GnBu', direction=1,);

usaGeo <- st_read('../input/cb_2018_us_county_20m.shp');
#usaGeo <- st_read('../input/tl_2019_us_county.shp');
usaGeo$STATEFP <- as.numeric(levels(usaGeo$STATEFP)[usaGeo$STATEFP]);
usaGeo$COUNTYFP <- as.numeric(levels(usaGeo$COUNTYFP)[usaGeo$COUNTYFP]);
usaGeo$fips <- usaGeo$STATEFP * 1000 + usaGeo$COUNTYFP;

usaCases <- read.csv('../input/us-counties.csv');
usaCases$date <- as.Date(usaCases$date);
usaPop <- read.csv('../input/co-est2019-alldata.csv');
usaPop$fips <- usaPop$STATE * 1000 + usaPop$COUNTY
usaCases <- usaCases %>% left_join(usaPop[,c('fips','POPESTIMATE2019')], by='fips');
usaCases$pop100k <- usaCases$POPESTIMATE2019/100000;

usaCases <- usaCases %>% group_by(fips, state, county, week = lubridate::week(date + 3)) %>%
  summarise(cases=max(cases));
usaCases <- usaCases %>% group_by(fips) %>%
  mutate(prev.cases=lag(cases, order_by=week)) %>%
  mutate(weekCases = cases - prev.cases);
usaCases <- usaCases %>% left_join(usaPop[,c('fips','POPESTIMATE2019')], by='fips');
usaCases$pop100k <- usaCases$POPESTIMATE2019/100000;
usaCases$cumCases <- usaCases$cases;
usaCases$cases <- pmax(usaCases$weekCases, 0);
usa <- usaCases %>% filter(week == theWeek) %>%
  filter(!state %in% stateUnfilter) %>%
  left_join(usaGeo %>% st_transform(theCoords),
        #    %>% st_simplify(dTolerance=500),
          by='fips') %>%
  select(health_region=fips, cases, pop100k, geometry, province=state);
usa$health_region <- as.character(usa$health_region);

canadaOutline <- st_read('../input/gpr_000b11a_e.shp') %>%
  st_simplify(dTolerance=0.01) %>% # degrees
  st_transform(theCoords);
canadaOutline$PRENAME <- forcats::fct_recode(canadaOutline$PRENAME,
    BC='British Columbia',
    NL='Newfoundland and Labrador',
    PEI='Prince Edward Island',
    NWT='Northwest Territories');
canadaOutline <- canadaOutline %>% filter(!PRENAME %in% provinceUnfilter);

usaOutline <- st_read('../input/cb_2018_us_state_20m.shp') %>%
  st_simplify(dTolerance=0.01) %>% # degrees
  st_transform(theCoords);
usaOutline <- usaOutline %>% filter(!NAME %in% stateUnfilter);

both <- rbind(as.data.frame(canada), as.data.frame(usa));

limGTHA <- list(x = c(7150000, 7320000), y = c(870000, 980000));
limGGH <- list(x = c(7000000, 7420000), y = c(770000, 1080000));
limON <-      list(x = c(5960000, 7584000), y = c(658000, 2296600));
limMBONQC <-  list(x = c(5508000, 8487000), y = c(658000, 3540000));
limON_extra <-list(x = c(5508000, 8000000), y = c(280000, 2280000));

limInterp <- function(interp) {
  list(x = limGGH$x * (1-interp) + limON_extra$x * interp,
                  y = limGGH$y * (1-interp) + limON_extra$y * interp)
}
lim <- limInterp(1);


thresh = 300;
scale_bi <- scales::trans_new('bi',
  function(x) { ifelse(x<thresh, x, (x-thresh)/((1500-thresh)/thresh)+thresh) },
  function(x) { ifelse(x<thresh, x, (x-thresh)*((1500-thresh)/thresh)+thresh) });

plotit <- function(data, interp, filename, bRate = TRUE) {
  baseDate <- as.Date('2019-12-29') + (theWeek-1)*7;
  p <- ggplot(data) +
    geom_sf(aes(geometry=geometry, fill=pmin(cases / pop100k, 1500)),
            color='#00000010') +
    scale_fill_distiller(palette='GnBu', direction=1,
                         trans=scale_bi, breaks = c(1:3*100, 2:5*thresh),
                         limits = c(0, 1500)) +
    geom_sf(data=canadaOutline, aes(geometry=geometry), colour='black', alpha=0.5, fill=NA, size=0.1) +
    geom_sf(data=usaOutline, aes(geometry=geometry), colour='black', alpha=0.5, fill=NA, size=0.1) +
    theme_minimal() +
    labs(fill='Weekly cases/100,000',x=NULL,y=NULL,
         caption='Data: COVID-19 Canada Open Data Working Group, covid-data@nytimes.com. Graphics: @drpritch2') +
    theme(legend.position="bottom", legend.key.width=unit(3, 'line')) +
    ggtitle(paste0('COVID 19 Cases per 100,000 for ',
                   format.Date(baseDate, "%b %d"), '-', format.Date(baseDate+6, "%b %d")));

    if (!is.na(interp)) {
      lim <- limInterp(interp);
      p <- p + coord_sf(xlim=lim$x, ylim=lim$y, expand=FALSE, crs=st_crs(3347));
      if (interp < 0.5) {
        provinceFilter <- ifelse(interp<0.3, 'Ontario', NA);
        p <- p + geom_sf_text(aes(geometry=geometry,
                        label=ifelse(province==provinceFilter,
                                paste0(health_region, '\n', round(cases/pop100k, -1)),
                                round(cases/pop100k, -1))),
                     size=2, alpha=0.5);
      }
    }
  if (!is.na(filename)) {
    ggsave(filename, scale=1.5, width=5, height=4.3, units='in');
  }
}

plotit(canada, 0, '0_ggh.png');
plotit(both, 0, '1_ggh_usa.png');
plotit(both, 0.3, '2_30_percent.png');
plotit(both, 1.0, '3_ontario.png');
plotit(both, NA, '4_north_america.png');


