import React from 'react';
import DeckGL from '@deck.gl/react';
import {GeoJsonLayer} from '@deck.gl/layers';
import {StaticMap} from 'react-map-gl';
import Slider from '@material-ui/core/slider';

const COLOR_SCALE = [
    // from R: scales::brewer_pal(palette='GnBu')(9)
    "#F7FCF0", "#E0F3DB", "#CCEBC5", "#A8DDB5",
    "#7BCCC4", "#4EB3D3", "#2B8CBE", "#0868AC",
    "#084081"
];
const BREAKS = [ 25, 100, 200, 300, 400, 700, 1000, 1300, 100000 ];

type CUCState = {
    week: number;
}

export class CovidUsaCanada extends React.Component<{}, CUCState> {
    constructor(props: any) {
        super(props);
        this.state = {
            week: 53
        };
    }
    render() {
        const getFillColor = (feature, week) => {
                const hex = COLOR_SCALE[
                        BREAKS.findIndex(x => x > feature.properties['cases' + week] / feature.properties.pop100k)
                ];
                var result = /^#?([a-f\d]{2})([a-f\d]{2})([a-f\d]{2})$/i.exec(hex);
                return result ? [
                        parseInt(result[1], 16),
                        parseInt(result[2], 16),
                        parseInt(result[3], 16)
                ] : null;
            };
        
        const layer = new GeoJsonLayer({
                id: 'geojson-layer',
                data: 'covidUsaCanada.json',
                pickable: true,
                stroked: true,
                filled: true,
                extruded: false,
                lineWidthScale: 20,
                lineWidthMinPixels: 0.5,
                getFillColor: (f) => getFillColor(f, this.state.week),
                updateTriggers: {
                        getFillColor: this.state.week
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
                                return p.health_region + '\n' +
                                        Math.round(p['cases' + this.state.week] / p.pop100k) + ' weekly cases/100k' + '\n' +
                                        Math.round(p.pop100k * 100000) + ' people';
                        }}>
                    {/*<StaticMap mapStyle='mapbox://styles/mapbox/light-v10'/>*/}
                </DeckGL>
                <div>
                        <Slider defaultValue={53} min={12} max={53}
                                onChange={(event: ReactChangeEvent<{}>, newValue: number) => this.setState({week: newValue})}></Slider>
                </div>
        </div>;
    }
}
export default CovidUsaCanada;
