import React from 'react';
import DeckGL from '@deck.gl/react';
import {GeoJsonLayer} from '@deck.gl/layers';
import {StaticMap} from 'react-map-gl';
import Box from '@material-ui/core/box';
import Fab from '@material-ui/core/Fab';
import FormControlLabel from '@material-ui/core/formcontrollabel';
import IconButton from '@material-ui/core/iconbutton';
import MenuItem from '@material-ui/core/MenuItem';
import Select from '@material-ui/core/select';
import Slider from '@material-ui/core/slider';
import Switch from '@material-ui/core/switch';
import Typography from '@material-ui/core/typography';

import PauseIcon from '@material-ui/icons/Pause';
import PlayArrowIcon from '@material-ui/icons/PlayArrow';

const palettes = {
    // from R: scales::brewer_pal(palette='GnBu')(9)
    GnBu: [ "#F7FCF0", "#E0F3DB", "#CCEBC5", "#A8DDB5", "#7BCCC4",
            "#4EB3D3", "#2B8CBE", "#0868AC", "#084081" ],
    Blues: [ "#F7FBFF", "#DEEBF7", "#C6DBEF", "#9ECAE1", "#6BAED6",
            "#4292C6", "#2171B5", "#08519C", "#08306B" ],
    Purples: [ "#FCFBFD", "#EFEDF5", "#DADAEB", "#BCBDDC", "#9E9AC8",
            "#807DBA", "#6A51A3", "#54278F", "#3F007D" ],
    Greens: [ "#F7FCF5", "#E5F5E0", "#C7E9C0", "#A1D99B", "#74C476",
        "#41AB5D", "#238B45", "#006D2C", "#00441B" ]
};
const COLOR_SCALE = {
    canadaUsa: palettes.GnBu,
    canada: palettes.Purples,
    atlantic: palettes.Purples
};
const COLOR_SCALE_COMPARE = [
    "#2166AC", "#4393C3", "#92C5DE", "#D1E5F0", "#F7F7F7",
    "#FDDBC7", "#F4A582", "#D6604D", "#B2182B",
];
const BREAKS = {
        canadaUsa: [ 25, 100, 200, 300, 400, 700, 1000, 1300 ],
        canada:    [ 25,  50,  75, 100, 150, 200, 250, 300 ],
        atlantic:  [ 4,   8,   12,  16,  20,  24,  28,  32 ]
};
const BREAKS_COMPARE = {
        canadaUsa: [-300,-200,-100, -25, 25, 100, 200, 300 ],
        canada:    [-150,-100, -50, -25, 25,  50, 100, 150 ],
        atlantic:  [ -16, -12,  -8,  -4,  4,   8,  12,  16 ]
};

type CUCState = {
    week: number;
    maxWeek: number;
    minWeek: number;
    numWeeks: number;
    data: JSON;
    playbackTimer: number;
    scaleType: 'canadaUsa' | 'canada' | 'atlantic';
}

const weekToDateStr = (week: number, dayofweek: number = 0): string => {
  let temp = new Date('2019-12-23');
  temp.setDate(temp.getDate() + week * 7 + dayofweek);
  return temp.toLocaleDateString('en-US', {month: 'short', day: 'numeric'});
}

function filterData(data: JSON, scaleType: 'canadaUsa' | 'canada' | 'atlantic'): JSON {
        let newData = {...data};
        if (scaleType != 'canadaUsa') 
                newData.features = data.features.filter((f) => f.properties.country == 'canada');
        return newData;
}

export class CovidUsaCanada extends React.Component<{}, CUCState> {
    constructor(props: any) {
        super(props);
        this.state = {
            week: null,
            data: null,
            minWeek: 12,
            maxWeek: null,
            scaleType: 'canadaUsa',
            numWeeks: 0,
            playbackTimer: null
        };
        this.download();
    }
    async download() {
        let data = await fetch(/*https://storage.googleapis.com/davidpritchard-website/*/'covidUsaCanada.json');
        let json = await data.json();
        const weeks: number[] = Object.keys(json.features[0].properties)
            .filter((x: string) => x.match(/^cases/))
            .map((x: string) => +x.substr(5));
        const maxWeek = Math.max(...weeks);
                
        this.setState({week: maxWeek, maxWeek: maxWeek, data: json});
    }
    timerCallback() {
        if(this.state.week==this.state.maxWeek) {
                window.clearInterval(this.state.playbackTimer);
                this.setState({playbackTimer: null});
        }
        else
                this.setState({week: this.state.week+1});
    }
    render(): React.ReactNode {
        if (!this.state.data) {
                return (<div></div>);
        }
        const bGrowth = this.state.numWeeks > 0;
        const bPlaying = this.state.playbackTimer != null;
        const getFillColor = (feature, week: number, numWeeks: number) => {
                const cases = feature.properties['cases' + week] / feature.properties.pop100k;
                let scale: string[];
                let breaks: number[];
                let value : number;
                if (numWeeks > 0) {
                        scale = COLOR_SCALE_COMPARE;
                        breaks = BREAKS_COMPARE[this.state.scaleType];
                        let cases2 = cases;
                        if (feature.properties.hasOwnProperty('cases' + (week - numWeeks))) {
                                cases2 = feature.properties['cases' + (week - numWeeks)] / feature.properties.pop100k;
                                value = cases - cases2;
                        }
                        else
                                value = 0;
                }
                else {
                        scale = COLOR_SCALE[this.state.scaleType];
                        breaks = BREAKS[this.state.scaleType];
                        value = cases;
                }
                const BREAKSextra = [ ...breaks, 100000 ];
                const bin = BREAKSextra.findIndex(x => x > value);
                const hex = scale[bin];
                var result = /^#?([a-f\d]{2})([a-f\d]{2})([a-f\d]{2})$/i.exec(hex);
                return result ? [
                        parseInt(result[1], 16),
                        parseInt(result[2], 16),
                        parseInt(result[3], 16)
                ] : null;
            };
        const layer = new GeoJsonLayer({
                id: 'geojson-layer',
                data: filterData(this.state.data, this.state.scaleType),
                pickable: true,
                stroked: true,
                filled: true,
                extruded: false,
                lineWidthScale: 20,
                lineWidthMinPixels: 0.5,
                getFillColor: (f) => getFillColor(f, this.state.week, this.state.numWeeks),
                updateTriggers: {
                        getFillColor: [ this.state.week, this.state.numWeeks, this.state.scaleType ],
                        data: this.state.scaleType
                },
                opacity: 0.7,
                getLineColor: [80,80,80,40],
                Radius: 100,
                getLineWidth: 1,
                getElevation: 0
        });

        const provinceLayer = new GeoJsonLayer({
                id: 'geojson-province-layer',
                data: /*https://storage.googleapis.com/davidpritchard-website/*/'provinceOutline.json',
                pickable: false,
                stroked: true,
                filled: false,
                extruded: false,
                lineWidthScale: 20,
                lineWidthMinPixels: 1,
                getLineColor: [0,0,0,70],
                Radius: 100,
                getLineWidth: 10,
                getElevation: 0
        });
        const stateLayer = new GeoJsonLayer({
                id: 'geojson-state-layer',
                data: /*https://storage.googleapis.com/davidpritchard-website/*/'stateOutline.json',
                pickable: false,
                stroked: true,
                filled: false,
                extruded: false,
                lineWidthScale: 20,
                lineWidthMinPixels: 1,
                getLineColor: [0,0,0,70],
                Radius: 100,
                getLineWidth: 10,
                getElevation: 0
        });
        return <div>
                <DeckGL
                        layers={[layer, stateLayer, provinceLayer]}
                        mapStyle="https://basemaps.cartocdn.com/gl/positron-gl-style/style.json"
                        initialViewState={{
                                longitude: -79.350,
                                latitude: 44.382,
                                zoom: 6,
                                maxZoom: 11,
                                pitch: 0,
                                bearing: 0
                        }}
                        controller={true}
                        getTooltip={({object}) => {
                                if (!object) return null;
                                const p = object.properties;
                                let result = p.health_region + ", " + p.province + '\n';
                                const rate1 = Math.round(p['cases' + this.state.week] / p.pop100k);
                                if (!bGrowth) {
                                        result += rate1 + ' weekly cases/100k\n';
                                        result += Math.round(p.pop100k * 100000) + ' people';
                                } else {
                                        const rate0 = Math.round(p['cases' + (this.state.week - this.state.numWeeks)] / p.pop100k);
                                        result += weekToDateStr(this.state.week - this.state.numWeeks, 6) + ': ' +
                                                rate0 + ' weekly cases/100k\n';
                                        result += weekToDateStr(this.state.week, 6) + ': ' +
                                                rate1 + ' weekly cases/100k\n';
                                        result += 'Change: ' + (rate1-rate0) + '\n';
                                }
                                return result;
                        }}
                        style={{zIndex: 0}}>
                    {/*<StaticMap mapStyle='mapbox://styles/mapbox/light-v10'/>*/}
                </DeckGL>

                <div style={{position: 'absolute', width: '300px', zIndex: 1, left: 0, top: 0}}>
                        <Box m={2} p={2} className='blur'>
                                <Typography variant='h5'>Covid Canada+USA</Typography>
                                <Typography>{bGrowth ? 'Case Growth' : 'Cases/100,000'} for week of <br/>
                                        {weekToDateStr(this.state.week) + '-' + weekToDateStr(this.state.week, 6)}</Typography>
                                <Typography>{bGrowth ? 'vs ' + this.state.numWeeks + ' weeks earlier' : '\u00a0'}</Typography>
                        {bGrowth ?
                                <Slider value={[this.state.week - this.state.numWeeks, this.state.week]} min={this.state.minWeek} max={this.state.maxWeek}
                                        onChange={(event: React.ChangeEvent<{}>, newValue: number|number[]) =>
                                                this.setState({week: newValue[1], numWeeks: newValue[1]-newValue[0]})}></Slider>
                        : <Slider value={this.state.week} min={this.state.minWeek} max={this.state.maxWeek}
                                onChange={(event: React.ChangeEvent<{}>, newValue: number|number[]) => this.setState({week: newValue as number})}></Slider>
    }
                        <Fab color="primary" onClick={() => {
                                if (bPlaying) {
                                        window.clearInterval(this.state.playbackTimer);
                                        this.setState({playbackTimer: null});
                                } else {
                                        let handle = window.setInterval(() => this.timerCallback(), 500);
                                        if (this.state.week==this.state.maxWeek)
                                                this.setState({ playbackTimer: handle, numWeeks: 0, week: this.state.minWeek});
                                        else
                                                this.setState({ playbackTimer: handle, numWeeks: 0});
                                }
                                }}>
                                {bPlaying ? <PauseIcon/> : <PlayArrowIcon/>}
                        </Fab>
                        <FormControlLabel style={{marginLeft: '12px'}} control={
                                <Switch checked={bGrowth}
                                        onChange={() => this.setState({numWeeks: bGrowth ? 0 : 2})}
                                        color='primary'/>}
                                        label='Growth'/>
                        </Box>
                </div>
                <div style={{position: 'absolute', width: '100vw', zIndex: 1, left: 0, bottom: 0}}>
                        <Box m={2}>
                        <Typography variant='caption'>Data by <a href="https://opencovid.ca">COVID-19 Canada Open Data Working Group</a>,
                        <a href="https://www.nytimes.com/interactive/2020/us/coronavirus-us-cases.html">The New York Times</a>.
                        Graphics by <a href="https://davidpritchard.org">David Pritchard</a> / <a href="https://twitter.com/drpritch2">@drpritch2</a>.
                        Sourcecode available from <a href="https://github.com/drpritch/covid-usacanada">github</a>.</Typography>
                        </Box>
                </div>
                <div style={{position: 'absolute', height: '136px', zIndex: 1, right: 0, top: 0}}>
                        <Select value={this.state.scaleType} onChange={(event) => this.setState({scaleType: event.target.value})}>
                                <MenuItem value='canadaUsa'>Canada/USA</MenuItem>
                                <MenuItem value='canada'>Canada</MenuItem>
                                <MenuItem value='atlantic'>Atlantic</MenuItem>
                        </Select>
                        <Box m={2}>
                                <div style={{display: 'flex', flexDirection: 'row'}}>
                                        <div style={{display: 'flex', flexDirection: 'column', flexBasis: '50%', flexGrow: 0}}>
                                                {(bGrowth ? COLOR_SCALE_COMPARE : COLOR_SCALE[this.state.scaleType]).map(x => 
                                                        <div style={{backgroundColor: x, flex: 1, width: '25px'}}>&nbsp;</div>)}
                                        </div>
                                        <div style={{display: 'flex', flexDirection: 'column', flexBasis: '50%', flexGrow: 0, paddingLeft: '4px'}}>
                                                <div style={{flex: 0.5}} />
                                                {(bGrowth ? BREAKS_COMPARE : BREAKS)[this.state.scaleType].map(
                                                        x => <Typography style={{flex: 1}} variant='caption'>{x}</Typography>)}
                                                <div style={{flex: 0.5}} />
                                        </div>
                                </div>
                        </Box>
                </div>
        </div>;
    }
}
export default CovidUsaCanada;
