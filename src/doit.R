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
theWeek <- 49;
#provinceFilter <- c('Saskatchewan', 'Manitoba','Ontario','Quebec');
provinceUnfilter <- c();#'Yukon', 'NWT', 'Nunavut');
#stateFilter <- c('Minnesota','Wiscosin','Michigan','Ohio','Pennsylvania','New York','Vermont','Massachusetts','Connecticut','Rhode Island','New Hampshire','Maine');
stateUnfilter <- c('Hawaii','Alaska','Puerto Rico','Virgin Islands','Northern Mariana Islands');

canadaFiles <- c(
  #Canada = 'HR_000a18a_e',
  NL = 'HR_010b18a_e',
  PEI = 'HR_011b18a_e',
  'Nova Scotia' = 'HR_012b18a_e',
  'New Brunswick' = 'HR_013b18a_e',
  Quebec = 'HR_024b18a_e',
  Ontario = 'HR_035b18a_e',
  Manitoba = 'HR_046b18a_e',
  Saskatchewan = 'HR_047b18a_e',
  Alberta = 'HR_048b18a_e',
  BC = 'HR_059b18a_e',
  Yukon = 'HR_060b18a_e',
  NWT = 'HR_061b18a_e',
  Nunavut = 'HR_062b18a_e');
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
canadaCases$health_region <- factor(canadaCases$health_region);
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
canadaGeo$HR_UID <- factor(as.numeric(canadaGeo$HR_UID));
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
  
  # PEI & NB & NS & Yukon & Nunavut names match
  
  Central=1011,
  "Labrador-Grenfell"=1012,
  Western=1013,
  "Eastern NL"=1014,
  
  'NWT'=6101),
  as.character)));

# health_region == ENGNAME for these provinces
# Sadly, province field seems to be absent from cartographic shape files.
#hrFilter <- canadaGeo$province %in% c('PEI', 'New Brunswick', 'Nova Scotia');
hrFilter <- floor(as.numeric(levels(canadaGeo$HR_UID)[canadaGeo$HR_UID])/100) %in% c(11:13,60,62);
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
  Fraser=1902586,
  Interior=805216,
  Island=846030,
  'Vancouver Coastal'=1219436,
  'Northern BC'=298068,
  
  Calgary=1680755,
  Edmonton=1443597,
  'Central Alberta'=479601,
  'North Alberta'=460964,
  'South Alberta'=306399,
  
  'Central Saskatchewan'=98566,
  'Far North'=37811,
  'North Saskatchewan'=196246,
  Regina=304261,
  Saskatoon=380403,
  'South Saskatchewan'=157175,
    
  'Interlake-Eastern'=133882,
  'Northern Manitoba'=75783,
  'Prairie Mountain'=171368,
  'Southern Health'=208018,
  Winnipeg=780414,
  
  Algoma=117036,
  Brant=151034,
  'Chatham-Kent'=106091,
  Durham=697355,
  'Eastern Ontario'=213064,
  'Grey Bruce'=173372,
  'Haldimand-Norfolk'=119146,
  'Haliburton Kawartha Pineridge'=189982,
  Halton=596363,
  Hamilton=574263,
  'Hastings Prince Edward'=170793,
  'Huron Perth'=144801,
  "Kingston Frontenac Lennox & Addington"=208613,
  Lambton=132243,
  "Leeds Grenville and Lanark"=177605,
  "Middlesex-London"=506008,
  Niagara=479183,
  "North Bay Parry Sound"=129642,
  Northwestern=81472,
  Ottawa=1028514,
  Peel=1542001,
  Peterborough=147908,
  Porcupine=85422,
  Renfrew=107955,
  "Simcoe Muskoka"=594494,                  
  Southwestern=215401,
  Sudbury=204640,
  "Thunder Bay"=158165,
  Timiskaming=33800,
  Toronto=2965712,
  Waterloo=595465,
  "Wellington Dufferin Guelph"=308963,
  "Windsor-Essex"=428556,
  York=1181485,
  
  "Abitibi-Témiscamingue"=147542,
  "Bas-Saint-Laurent"=197322,
  "Capitale-Nationale"=750645,
  "Chaudière-Appalaches"=428618,
  "Côte-Nord"=90704,
  Estrie=489479,
  "Gaspésie-Îles-de-la-Madeleine"=90334,
  Lanaudière=515448,
  Laurentides=620264,
  Laval=438973,
  Mauricie=520285,
  Montérégie=1421586,
  Montréal=2052910,
  "Nord-du-Québec"=13633,
  #"Not Reported"
  Nunavik=14102,
  Outaouais=397193,
  Saguenay=277796,
  "Terres-Cries-de-la-Baie-James"=18131,
  
  "Prince Edward Island"=156947,
  
  # New Brunswick
  "Zone 1 (Moncton area)"=222694,
  "Zone 2 (Saint John area)"=176280,
  "Zone 3 (Fredericton area)"=183421,
  "Zone 4 (Edmundston area)"=48254,
  "Zone 5 (Campbellton area)"=25199,
  "Zone 6 (Bathurst area)"=76763,
  "Zone 7 (Miramichi area)"=44216,
  
  # Nova Scotia
  "Zone 1 - Western"=199165,
  "Zone 2 - Northern"=148470,
  "Zone 3 - Eastern"=162088,
  "Zone 4 - Central"=461672,
  
  Central=91201,
  "Labrador-Grenfell"=36069,
  Western=76608,
  "Eastern NL"=317664,
  
  Yukon=40854,
  NWT=44826,
  Nunavut=38780));

canadaCases$date_report <- as.Date(canadaCases$date_report, '%d-%m-%Y');
pops <- data.frame(health_region=factor(names(pops), levels=levels(canadaCases$health_region)), pop100k=pops/100000);
canadaCases <- canadaCases %>%
  left_join(pops, by='health_region');

# Week definition:
# Normally: week 1 = Jan 1...7, week 2 = Jan 8...15
# By shifting by three: week 1 = Dec 29...Jan 4 (Sun-Sat)
# and week 47 = Nov. 15-21
canadaCases <- canadaCases %>% group_by(health_region, province, pop100k, week = lubridate::week(date_report + 3)) %>%
  summarise(cases=sum(cases)) %>%
  filter(week < 50) %>%
  tidyr::pivot_wider(names_from=week, values_from=cases, names_prefix='cases');

canada <- canadaCases %>%
  filter(!province %in% provinceUnfilter) %>%
  left_join(canadaGeo %>% st_transform(theCoords) %>% st_simplify(dTolerance=500) %>% select(health_region),
            by = 'health_region');
#ggplot(canada) +
#  geom_sf(aes(geometry=geometry, fill=weekly / pop100k), color='#00000010') +
#  scale_fill_fermenter(palette='GnBu', direction=1,);

usaGeo <- st_read('../input/cb_2018_us_county_20m.shp');
#usaGeo <- st_read('../input/tl_2019_us_county.shp');
usaGeo$STATEFP <- as.numeric(usaGeo$STATEFP);
usaGeo$COUNTYFP <- as.numeric(usaGeo$COUNTYFP);
usaGeo$fips <- usaGeo$STATEFP * 1000 + usaGeo$COUNTYFP;
usaGeo <- usaGeo %>% st_transform(theCoords);
#    %>% st_simplify(dTolerance=500),

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
usaCases <- usaCases %>%
  filter(!is.na(fips)) %>%
  select(fips, state, pop100k, week, cases) %>%
  tidyr::pivot_wider(names_from=week, values_from=cases, names_prefix='cases');

usa <- usaCases %>%
  filter(!state %in% stateUnfilter) %>%
  left_join(usaGeo %>% select('fips'), by='fips');
usa$province <- usa$state;
usa$state <- NULL;
usa$health_region <- as.character(usa$fips);
usa$fips <- NULL;


both <- rbind(as.data.frame(canada), as.data.frame(usa));


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

limGTHA <- list(x = c(7150000, 7320000), y = c(870000, 980000));
limGGH <- list(x = c(7000000, 7420000), y = c(770000, 1080000));
limON <-      list(x = c(5960000, 7584000), y = c( 658000, 2296600));
limMBONQC <-  list(x = c(5508000, 8487000), y = c( 658000, 3540000));
limABSKMB <-  list(x = c(4427326, 6371650), y = c(1433502, 2965270));
limABSKMB_extra <-  list(x = c(4100000, 6380000), y = c(658000, 2970000));
limON_extra <-list(x = c(5508000, 8000000), y = c( 280000, 2280000));
limNA <- list(x = c(3415360, 9015737), y = c(-1398365, 4360410));

limInterp <- function(interp) {
  list(x = limGGH$x * (1-interp) + limON_extra$x * interp,
                  y = limGGH$y * (1-interp) + limON_extra$y * interp)
}
lim <- limInterp(1);


plotON <- function(data, week, interp, filename) {
  lim <- NULL;
  bShowLabels <- FALSE;
  labelProvinceFilter <- NA;
  if (!is.na(interp)) {
    lim <- limInterp(interp);
    bShowLabels <- interp < 0.5;
    labelProvinceFilter <- ifelse(interp < 0.3, 'Ontario', NA);
  }
  plotit(data, week, lim, filename, bShowLabels, labelProvinceFilter, theCanadaOutline = canadaOutline);
}
plotit <- function(data, week, lim, filename, bShowLabels = FALSE, labelProvinceFilter = NA, theCanadaOutline = canadaOutline) {
  baseDate <- as.Date('2019-12-29') + (week-1)*7;
  data <- data %>% select(pop100k, cases=paste0('cases', week), geometry, province, health_region);
  p <- ggplot(data) +
    geom_sf(aes(geometry=geometry, fill=cases / pop100k),
            color='#00000010') +
    scale_fill_fermenter(palette='GnBu', direction=1,
                         breaks = c(25,1:4*100, 700, 1000, 1300),
                         limits = c(0, 1600)) +
    geom_sf(data=theCanadaOutline, aes(geometry=geometry), colour='black', alpha=0.5, fill=NA, size=0.1) +
    geom_sf(data=usaOutline, aes(geometry=geometry), colour='black', alpha=0.5, fill=NA, size=0.1) +
    theme_minimal() +
    labs(fill='Weekly cases/100,000',x=NULL,y=NULL,
         caption='Data: COVID-19 Canada Open Data Working Group, covid-data@nytimes.com. Graphics: @drpritch2') +
    theme(legend.position="bottom", legend.key.width=unit(3, 'line')) +
    ggtitle(paste0('COVID 19 Cases per 100,000 for ',
                   format.Date(baseDate, "%b %d"), '-', format.Date(baseDate+6, "%b %d")));

    if (!is.null(lim)) {
      p <- p + coord_sf(xlim=lim$x, ylim=lim$y, expand=FALSE, crs=st_crs(3347));
      if (bShowLabels) {
        p <- p + geom_sf_text(aes(geometry=geometry,
                        label=ifelse(province==labelProvinceFilter,
                                paste0(health_region, '\n', round(cases/pop100k, -1)),
                                round(cases/pop100k, -1))),
                     size=2, alpha=0.5);
      }
    }
  if (!is.na(filename)) {
    ggsave(filename, scale=1.5, width=5, height=4.3, units='in');
  }
  p
}

canadaOutlineSimplified <- canadaOutline %>% st_simplify(dTolerance = 0.1);

#plotON(canada, theWeek, 0, '0_ggh.png');
plotON(both, theWeek, 0, '1_ggh_usa.png');
plotON(both, theWeek, 0.3, '2_30_percent.png');
plotON(both, theWeek, 1.0, '3_ontario.png');
plotit(both, theWeek, limNA, '4_north_america.png', theCanadaOutline = canadaOutlineSimplified);
plotit(both, theWeek, limABSKMB_extra, '5_prairies.png');

for (aWeek in 20:49) {
  plotON(both, aWeek, 1.0, paste0('ontario_', aWeek, '.png'));
  plotit(both, aWeek, limNA, paste0('north_america_', aWeek, '.png'), theCanadaOutline = canadaOutlineSimplified);
  plotit(both, aWeek, limABSKMB_extra, paste0('prairies_', aWeek, '.png'));
}
