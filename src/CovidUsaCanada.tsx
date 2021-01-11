import React from 'react';
import DeckGL from '@deck.gl/react';
import {GeoJsonLayer} from '@deck.gl/layers';
import {StaticMap} from 'react-map-gl';
import Box from '@material-ui/core/box';
import FormControlLabel from '@material-ui/core/formcontrollabel';
import Slider from '@material-ui/core/slider';
import Switch from '@material-ui/core/switch';
import Typography from '@material-ui/core/typography';

const COLOR_SCALE = [
    // from R: scales::brewer_pal(palette='GnBu')(9)
    "#F7FCF0", "#E0F3DB", "#CCEBC5", "#A8DDB5",
    "#7BCCC4", "#4EB3D3", "#2B8CBE", "#0868AC",
    "#084081"
];
const COLOR_SCALE_COMPARE = [
    "#B2182B", "#D6604D", "#F4A582", "#FDDBC7",
    "#F7F7F7", "#D1E5F0", "#92C5DE", "#4393C3",
    "#2166AC"
];
const BREAKS = [ 25, 100, 200, 300, 400, 700, 1000, 1300, 100000 ];

type CUCState = {
    week: number;
    numWeeks: number;
}

const weekToDateStr = (week: number, dayofweek: number = 0): string => {
  let temp = new Date('2019-12-23');
  temp.setDate(temp.getDate() + week * 7 + dayofweek);
  return temp.toLocaleDateString('en-US', {month: 'short', day: 'numeric'});
}

export class CovidUsaCanada extends React.Component<{}, CUCState> {
    constructor(props: any) {
        super(props);
        this.state = {
            week: 54,
            numWeeks: 0
        };
    }
    render() {
        const getFillColor = (feature, week: number, numWeeks: number) => {
                const bin = BREAKS.findIndex(x => x > feature.properties['cases' + week] / feature.properties.pop100k);
                let hex: string;
                if (numWeeks > 0) {
                        let bin2 = bin;
                        if (feature.properties.hasOwnProperty('cases' + (week - numWeeks)))
                                bin2 = BREAKS.findIndex(x => x > feature.properties['cases' + (week - numWeeks)] / feature.properties.pop100k);
                        hex = COLOR_SCALE_COMPARE[Math.min(Math.max(bin2 - bin + 4, 0), 8)];
                }
                else
                        hex = COLOR_SCALE[bin];
                var result = /^#?([a-f\d]{2})([a-f\d]{2})([a-f\d]{2})$/i.exec(hex);
                return result ? [
                        parseInt(result[1], 16),
                        parseInt(result[2], 16),
                        parseInt(result[3], 16)
                ] : null;
            };
        
        const layer = new GeoJsonLayer({
                id: 'geojson-layer',
                data: /*https://storage.googleapis.com/davidpritchard-website/*/'covidUsaCanada.json',
                pickable: true,
                stroked: true,
                filled: true,
                extruded: false,
                lineWidthScale: 20,
                lineWidthMinPixels: 0.5,
                getFillColor: (f) => getFillColor(f, this.state.week, this.state.numWeeks),
                updateTriggers: {
                        getFillColor: this.state.week + this.state.numWeeks
                },
                opacity: 0.7,
                getLineColor: [80,80,80,40],
                Radius: 100,
                getLineWidth: 1,
                getElevation: 0
        });

        return <div>
                <DeckGL
                        layers={[layer]}
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
                                let result = p.health_region + '\n';
                                const rate1 = Math.round(p['cases' + this.state.week] / p.pop100k);
                                if (this.state.numWeeks == 0) {
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

                <div style={{position: 'absolute', width: '300px', zIndex: 1, right: 0, top: 0}}>
                        <Box m={4}>
                        <Typography variant='h2'>COVID-19 Cases</Typography>
                        <Typography>per 100,000 for the week of {weekToDateStr(this.state.week) + '-' + weekToDateStr(this.state.week, 6) +
                                (this.state.numWeeks > 0 ? (' vs ' + this.state.numWeeks + ' weeks earlier') : '')}</Typography>
                        {this.state.numWeeks > 0 ?
                                <Slider value={[this.state.week - this.state.numWeeks, this.state.week]} min={12} max={54}
                                        onChange={(event: ReactChangeEvent<{}>, newValue: number[]) =>
                                                this.setState({week: newValue[1], numWeeks: newValue[1]-newValue[0]})}></Slider>
                        : <Slider value={this.state.week} min={12} max={54}
                                onChange={(event: ReactChangeEvent<{}>, newValue: number) => this.setState({week: newValue})}></Slider>
    }
                        <FormControlLabel control={
                                <Switch checked={this.state.numWeeks>0}
                                        onChange={() => this.setState({numWeeks: this.state.numWeeks > 0 ? 0 : 2})}
                                        color='primary'/>}
                                label='Compare'/>
                        <p>
                        <Typography variant='caption'>Data by <a href="https://opencovid.ca">COVID-19 Canada Open Data Working Group</a>,
                        <a href="https://www.nytimes.com/interactive/2020/us/coronavirus-us-cases.html">The New York Times</a>.
                        Graphics by <a href="https://davidpritchard.org">David Pritchard</a> / <a href="https://twitter.com/drpritch2">@drpritch2</a>.
                        Sourcecode available from <a href="https://github.com/drpritch/covid-usacanada">github</a>.</Typography>
                        </p>
                        </Box>
                </div>
        </div>;
    }
}
export default CovidUsaCanada;
