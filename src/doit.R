library('sf');
library('dplyr');
library('ggplot2');
library('ggpubr');

#theCoords <- 26911; #UTM11N
#theCoords <- 26917; #UTM17N
#theCoords <- 4269; # NAD 83
#theCoords <- 4326; # WGS 84
theCoords <- 3347; # StatsCan Lambert
theCoords <- st_crs(theCoords);
# Week 47: Nov. 15-21
theWeek <- 55;
maxWeek <- theWeek;
stateUnfilter <- c('Hawaii','Puerto Rico','Virgin Islands','Northern Mariana Islands');

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
    'Central AB', 'Central SK',
    'North AB', 'North SK',
    'South AB', 'South SK',
    'Eastern ON', 'Eastern NL',
    'Northern MB', 'Northern BC');
disambiguateRegion <- function(canadaCases, region, province1, abbrev1, province2, abbrev2) {
  canadaCases[canadaCases$health_region == region & canadaCases$province == province1,]$health_region = paste0(region, ' ', abbrev1);
  canadaCases[canadaCases$health_region == region & canadaCases$province == province2,]$health_region = paste0(region, ' ', abbrev2);
  canadaCases;
}
canadaCases <- disambiguateRegion(canadaCases, 'Central', 'Alberta', 'AB', 'Saskatchewan', 'SK');
canadaCases <- disambiguateRegion(canadaCases, 'North', 'Alberta', 'AB', 'Saskatchewan', 'SK');
canadaCases <- disambiguateRegion(canadaCases, 'South', 'Alberta', 'AB', 'Saskatchewan', 'SK');
canadaCases <- disambiguateRegion(canadaCases, 'Eastern', 'Ontario', 'ON', 'NL', 'NL');
canadaCases <- disambiguateRegion(canadaCases, 'Northern', 'Manitoba', 'MB', 'BC', 'BC');

canadaGeo$province <- factor(canadaGeo$province, levels=levels(canadaCases$province));
canadaGeo$HR_UID <- factor(as.numeric(canadaGeo$HR_UID));
canadaGeo$health_region <- forcats::fct_recode(canadaGeo$HR_UID,
  !!!unlist(lapply(list(
    'South AB'=4831,
    Calgary=4832,
    'Central AB'=4833,
    Edmonton=4834,
    'North AB'=4835,
    
    Regina=4704,
    Saskatoon=4706,
    
    'Interlake-Eastern'=4603,
    'Northern MB'=4604,
    'Prairie Mountain'=4602,
    'Southern Health'=4605,
    Winnipeg=4601,
    
    Algoma=3526,
    Brant=3527,
    'Chatham-Kent'=3540,
    Durham=3530,
    'Eastern ON'=3558,
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
    mutate(health_region = 'North SK');
canadaGeo <- rbind(canadaGeo, st_union(st_union(sask[1,], sask[2,]), sask[3,])[,1:7]);
sask <- canadaGeo %>% filter(HR_UID %in% c(4705,4707)) %>%
  mutate(health_region = 'Central SK');
canadaGeo <- rbind(canadaGeo, st_union(sask[1,], sask[2,])[,1:7]);
sask <- canadaGeo %>% filter(HR_UID %in% 4701:4703) %>%
  mutate(health_region = 'South SK');
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
  'Central AB'=479601,
  'North AB'=460964,
  'South AB'=306399,
  
  'Central SK'=98566,
  'Far North'=37811,
  'North SK'=196246,
  Regina=304261,
  Saskatoon=380403,
  'South SK'=157175,
    
  'Interlake-Eastern'=133882,
  'Northern MB'=75783,
  'Prairie Mountain'=171368,
  'Southern Health'=208018,
  Winnipeg=780414,
  
  Algoma=117036,
  Brant=151034,
  'Chatham-Kent'=106091,
  Durham=697355,
  'Eastern ON'=213064,
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
# That gave: lubridate::week(date_report + 3)
# Then revised to go beyond 2020.
canadaCases <- canadaCases %>% group_by(health_region, province, pop100k, week = as.numeric(floor((date_report - as.Date('2019-12-22'))/7))) %>%
  summarise(cases=sum(cases)) %>%
  tidyr::pivot_wider(names_from=week, values_from=cases, names_prefix='cases');

canada <- inner_join(canadaGeo %>% select(health_region),
                     canadaCases, by = 'health_region') %>%
  st_transform(theCoords) %>% st_simplify(dTolerance=500);
# Trim north, with crazy coastline+islands that increases filesize and extent
# Reduces json filesize about 700kB (10%)
canada <- canada %>% st_transform(4326) %>%
  st_crop(xmin=-180, ymin=0, xmax=180, ymax=67) %>%
  st_transform(theCoords);

canada$id <- canada$health_region;
canada$country <- 'canada';
#ggplot(canada) +
#  geom_sf(aes(geometry=geometry, fill=weekly / pop100k), color='#00000010') +
#  scale_fill_fermenter(palette='GnBu', direction=1,);

usaGeo <- st_read('../input/cb_2018_us_county_20m.shp');
#usaGeo <- st_read('../input/tl_2019_us_county.shp');
usaGeo$STATEFP <- as.numeric(usaGeo$STATEFP);
usaGeo$COUNTYFP <- as.numeric(usaGeo$COUNTYFP);
usaGeo$fips <- usaGeo$STATEFP * 1000 + usaGeo$COUNTYFP;
usaGeo <- usaGeo %>% st_transform(theCoords) %>%
  select(fips, geometry);
#    %>% st_simplify(dTolerance=500),

usaCases <- read.csv('../input/us-counties.csv');
usaCases$date <- as.Date(usaCases$date);
usaPop <- read.csv('../input/co-est2019-alldata.csv');
usaPop$fips <- usaPop$STATE * 1000 + usaPop$COUNTY;

# NYT case data aggregates NYC (NYC county, Kings, Queens, Bronx, Richmond).
usaCases[usaCases$county=='New York City','fips'] <- 36999;
nycPop <- usaPop %>%
  filter(fips %in% c('36061','36047','36081','36005','36085')) %>%
  select(POPESTIMATE2019) %>%
  summarise_all(sum);
usaPop <- bind_rows(usaPop, c(fips = 36999, nycPop));
nycGeo <- usaGeo %>%
  filter(fips %in% c('36061','36047','36081','36005','36085')) %>%
  select(geometry);
nycGeo <- st_union(st_union(st_union(nycGeo[1,],nycGeo[2,]), st_union(nycGeo[3,],nycGeo[4,])), nycGeo[4,]);
usaGeo <- bind_rows(usaGeo, c(fips = 36999, nycGeo));


usaCases <- usaCases %>% left_join(usaPop[,c('fips','POPESTIMATE2019')], by='fips');
usaCases$pop100k <- usaCases$POPESTIMATE2019/100000;

usaCases <- usaCases %>% group_by(fips, state, county, week = as.numeric(floor((date - as.Date('2019-12-22'))/7))) %>%
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
  select(fips, health_region = county, state, pop100k, week, cases) %>%
  tidyr::pivot_wider(names_from=week, values_from=cases, names_prefix='cases', values_fill=0);

usa <- inner_join(usaGeo %>% select('fips'),
                 usaCases %>% filter(!state %in% stateUnfilter),
                 by='fips') %>%
  rename(province = state, id = fips) %>%
  st_transform(st_crs(theCoords));
usa$id <- as.character(usa$id);
usa$country <- 'usa';


both <- bind_rows(canada, usa);
both11N <- both %>% st_transform(26911);
both13N <- both %>% st_transform(26913);


canadaOutline <- st_read('../input/gpr_000b11a_e.shp') %>%
  st_simplify(dTolerance=0.01) %>% # degrees
  st_transform(theCoords);
canadaOutline$PRENAME <- forcats::fct_recode(canadaOutline$PRENAME,
    BC='British Columbia',
    NL='Newfoundland and Labrador',
    PEI='Prince Edward Island',
    NWT='Northwest Territories');

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
limABSKMB_extra_13N <-  list(x = c(-700000, 1500000), y = c(5000000, 6800000));
limON_extra <- list(x = c(5508000, 8000000), y = c( 280000, 2280000));
limBC <- list(x = c(3700000, 4800000), y = c(1500000, 2300000));
limBC_11N <- list(x = c(-300000, 1000000), y = c(5000000, 5800000));
limNA <- list(x = c(3415360, 9015737), y = c(-1398365, 4360410));

# a southern Ontario view
limON30 <- list(x = c(6702400, 7804000), y = c(480000, 1340000));
limWindsor <- list(x = c(6702400, 7304000), y = c(440000, 980000));
limWindsor <- list(x = c(6852400, 7154000), y = c(585000, 845000));
limInterp <- function(interp) {
  if (interp < 0.3) {
    interp <- interp / 0.3;
    result <- list(x = limGGH$x * (1-interp) + limON30$x * interp,
         y = limGGH$y * (1-interp) + limON30$y * interp);
  } else {
    interp <- (interp - 0.3)/0.7;
    result <- list(x = limON30$x * (1-interp) + limON_extra$x * interp,
                    y = limON30$y * (1-interp) + limON_extra$y * interp)
  }
  result;
}


# For midweek approx graphs - e.g., 7/4 to scale up from 4 to 7 days.
scaleFactor <- 1;

plotON <- function(data, week, interp, filename, week0 = NA) {
  lim <- NULL;
  bShowLabels <- FALSE;
  if (!is.na(interp)) {
    lim <- limInterp(interp);
    if (interp < 0.3) {
      bShowLabels <- 'both';
    } else if (interp < 0.5) {
      bShowLabels <- 'trim';
    }
  }
  plotit(data, week, lim, filename, bShowLabels, theCanadaOutline = canadaOutline, week0 = week0);
}
plotit <- function(theData, week, lim, filename, bShowLabels = FALSE, theCanadaOutline = canadaOutline, week0 = NA, crs=3347) {
  baseDate <- as.Date('2019-12-29') + (week-1)*7;
  breaks <- c(25,1:4*100, 700, 1000, 1300);
  if (is.na(week0)) {
    data <- theData %>% select(pop100k, cases=paste0('cases', week), geometry, province, health_region, country);
    theScaleFill <- scale_fill_fermenter(palette='GnBu', direction=1,
                                           breaks = breaks,
                                           limits = c(0, 1600));
    data$value <- data$cases / data$pop100k * scaleFactor;
    data$numValue <- data$value;
    legendPosition <- 'bottom';
    legendTitle <- 'Weekly cases/100,000';
    title <- paste0('COVID 19 Cases per 100,000 for ',
                    format.Date(baseDate, "%b %d"), '-', format.Date(baseDate+6, "%b %d"));
  } else {
    data <- theData %>% select(pop100k, cases=paste0('cases', week), cases0 = paste0('cases', week0), geometry, province, health_region, country);
    data$bin <- cut(data$cases/data$pop100k * scaleFactor, breaks=c(-10, breaks, 100000));
    data$bin0 <- cut(data$cases0/data$pop100k * scaleFactor, breaks=c(-10, breaks, 100000));
    data$numValue <- (data$cases - data$cases0)/data$pop100k * scaleFactor;
    data$value <- factor(pmax(pmin(as.numeric(data$bin) - as.numeric(data$bin0), 4), -4), levels=-4:4);
    theScaleFill <- scale_fill_brewer(palette='RdBu', type='seq', direction=-1);
    legendPosition = 'right';
    legendTitle <- 'Category growth';
    title <- paste0('COVID-19 Case growth over ', week-week0, ' weeks prior');
  }
  data$titleTrim <- substr(data$health_region, 0, 10);
  data$titleTrim[data$country=='usa'] <- substr(data$titleTrim[data$country=='usa'], 0, 3);
  p <- ggplot(data) +
    geom_sf(aes(geometry=geometry, fill=value), color='#00000010') +
    theScaleFill +
    geom_sf(data=theCanadaOutline, aes(geometry=geometry), colour='black', alpha=0.5, fill=NA, size=0.1) +
    geom_sf(data=usaOutline, aes(geometry=geometry), colour='black', alpha=0.5, fill=NA, size=0.1) +
    theme_minimal() +
    labs(fill=legendTitle,x=NULL,y=NULL,
         caption='Data: COVID-19 Canada Open Data Working Group, covid-data@nytimes.com. Graphics: @drpritch2') +
    theme(legend.position=legendPosition, legend.key.width=unit(3, 'line')) +
    ggtitle(title);

    if (!is.null(lim)) {
      p <- p + coord_sf(xlim=lim$x, ylim=lim$y, expand=FALSE, crs=st_crs(crs), label_graticule = '', label_axes='');
      if (bShowLabels == 'both') {
        p <- p + geom_sf_text(aes(geometry=geometry,
                        label=paste0(health_region, '\n', round(numValue, -1))),
                     size=2, alpha=0.5);
      }
      else if (bShowLabels == 'canada') {
        p <- p + geom_sf_text(data = data %>% filter(country=='canada'),
                              aes(geometry=geometry,
                                  label=paste0(health_region, '\n', round(numValue, -1))),
                              size=2, alpha=0.5);
      }
      else if (bShowLabels == 'trim') {
        p <- p + geom_sf_text(aes(geometry=geometry,
                                  label=paste0(titleTrim, '\n', round(numValue, -1))),
                              size=1.5, alpha=0.5);
      }
      else if (bShowLabels == 'trimCanada') {
        p <- p + geom_sf_text(data = data %>% filter(country=='canada'),
                              aes(geometry=geometry,
                                  label=paste0(titleTrim, '\n', round(numValue, -1))),
                              size=1.5, alpha=0.5);
      }
    }
  if (!is.na(filename)) {
    if (is.na(week0)) {
      ggsave(paste0('../images/', filename, '_', week, '.png'), scale=1.5, width=5, height=4.3, units='in');
    } else {
      ggsave(paste0('../images/', filename, '_delta_', week, '.png'), scale=1.5, width=5, height=4.3, units='in');
      plotit(theData, week, lim, filename, bShowLabels, theCanadaOutline, NA, crs);
    }
  }
  p
}

canadaOutlineSimplified <- canadaOutline %>% st_simplify(dTolerance = 0.1) %>%
  st_transform(4326) %>%
  st_transform(theCoords);
theWeek0 <- 53;
plotON(both, theWeek, 0, 'ggh_usa', week0 = theWeek0);
plotON(both, theWeek, 0.3, 'southontario', week0 = theWeek0);
plotON(both, theWeek, 1.0, 'ontario', week0 = theWeek0);
plotit(both, theWeek, limNA, 'north_america', theCanadaOutline = canadaOutlineSimplified, week0 = theWeek0);
plotit(both13N, theWeek, limABSKMB_extra_13N, 'prairies', 'canada', crs=26913, week0 = theWeek0);
plotit(both, theWeek, limWindsor, 'windsor', 'both', week0 = theWeek0);
plotit(both11N, theWeek, limBC_11N, 'bc', 'both', crs=26911, week0 = theWeek0);

write(geojsonsf::sf_geojson(
  both %>% st_transform(4326), digits=3), '../dist/covidUsaCanada.json');
write(geojsonsf::sf_geojson(
  canadaOutlineSimplified %>% st_transform(4326) %>% st_crop(xmin=-180, ymin=0, xmax=180, ymax=67), digits=3), '../dist/provinceOutline.json');
write(geojsonsf::sf_geojson(
  usaOutline %>% st_transform(4326), digits=3), '../dist/stateOutline.json');

stop();

for (aWeek in 40:maxWeek) {
  plotON(both, aWeek, 0, 'ggh_usa');
  plotON(both, aWeek, 0.3, 'southontario');
  plotON(both, aWeek, 1.0, 'ontario');
  plotit(both, aWeek, limNA, 'north_america', theCanadaOutline = canadaOutlineSimplified);
  plotit(both13N, aWeek, limABSKMB_extra_13N, 'prairies', 'canada', crs=26913);
  plotit(both, aWeek, limWindsor, 'windsor', 'both');
  plotit(both11N, aWeek, limBC_11N, 'bc', 'both', crs=26911);
}
