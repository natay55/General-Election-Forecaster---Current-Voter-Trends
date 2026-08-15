import dash
import dash_leaflet as dl
from dash import html, dcc, Input, Output
from dash_extensions.javascript import assign
import json
import geopandas as gpd
import pandas as pd

#--------------------------------------------------
# Load data
#--------------------------------------------------
constituency_sf = gpd.read_file(
    r"C:\Users\natay\Documents\GE explore\Westminster_Parliamentary_Constituencies_July_2024_Boundaries_UK_BUC_2210534386407638194\PCON_JULY_2024_UK_BUC.shp"
).to_crs(epsg=4326)

predictions = pd.read_excel(
    r"C:\Users\natay\Documents\GE explore\general_election_predictions\frontend\data\uk_election_predictions.xlsx",
    sheet_name = "predictions"
)

map_data = constituency_sf.merge(
    predictions,
    on  = "PCON24CD",
    how = "left"
)

ni_parties = ["sinn fein", "dup", "sdlp", "alliance", "uup", "tuv", "other"]

seat_totals = (
    map_data[~map_data["party"].isin(ni_parties)]
    .groupby("party")["PCON24CD"]
    .count()
    .reset_index()
    .rename(columns={"PCON24CD": "seats"})
    .sort_values("seats", ascending=False)
    .to_dict(orient="records")
)

#--------------------------------------------------
# Party colours
#--------------------------------------------------
party_colours = {
    "Labour"                        : "#E4003B",
    "Conservative"                  : "#0087DC",
    "Brexit Party/Reform UK"        : "#12B6CF",
    "Liberal Democrat"              : "#FAA61A",
    "Green Party"                   : "#02A95B",
    "Scottish National Party (SNP)" : "#FDF38E",
    "Plaid Cymru"                   : "#005B54",
    "Other"                         : "#AAAAAA",
    "sinn fein"                     : "#326760",
    "dup"                           : "#D46A4C",
    "sdlp"                          : "#2AA82C",
    "alliance"                      : "#F6CB2F",
    "uup"                           : "#48A5EE",
    "tuv"                           : "#0C3A6A",
    "Other (NI)"                         : "#888888"
}

visible_parties = ["Labour", "Conservative", "Brexit Party/Reform UK", "Liberal Democrat", "Green Party", "Scottish National Party (SNP)", "Plaid Cymru", "Other"]

#--------------------------------------------------
# GeoJSON
#--------------------------------------------------
geojson_data = json.loads(map_data.to_json())
for feature in geojson_data["features"]:
    party = feature["properties"].get("party", "")
    feature["properties"]["color"] = party_colours.get(party, "#AAAAAA")

#--------------------------------------------------
# JavaScript functions
#--------------------------------------------------
style_function = assign("""
    function(feature) {
        return {
            fillColor  : feature.properties.color,
            weight     : 1,
            color      : 'black',
            fillOpacity: 1
        };
    }
""")

hover_style = assign("""
    function(feature) {
        return {
            fillColor  : feature.properties.color,
            weight     : 3,
            color      : 'white',
            fillOpacity: 1
        };
    }
""")

tooltip_function = assign("""
    function(feature, layer) {
        layer.bindTooltip(
            '<b>' + feature.properties.PCON24NM + '</b><br>' +
            'Party: ' + feature.properties.party + '<br>' +
            'Vote share: ' + (feature.properties.vote_share * 100).toFixed(1) + '%',
            {sticky: true}
        );
    }
""")

#--------------------------------------------------
# App
#--------------------------------------------------
app = dash.Dash(__name__)

app.layout = html.Div(
    style={
        "margin"          : "0",
        "padding"         : "0",
        "height"          : "100vh",
        "display"         : "flex",
        "flexDirection"   : "column",
        "fontFamily"      : "'Segoe UI', Arial, sans-serif",
        "backgroundColor" : "#f5f5f5"
    },
    children=[
        # Header
        html.Div(
            style={
                "display"         : "flex",
                "flexDirection"   : "column",
                "justifyContent"  : "center",
                "alignItems"      : "center",
                "width"           : "100%",
                "padding"         : "12px 20px",
                "borderBottom"    : "2px solid #333",
                "backgroundColor" : "#1a1a2e",
                "color"           : "white"
            },
            children=[
                html.H1(
                    "UK General Election Forecast",
                    style={"margin": "0", "fontSize": "24px", "fontWeight": "700"}
                ),
                html.P(
                    "Last Updated: 14th August 2026",
                    style={"margin": "4px 0", "fontSize": "12px", "opacity": "0.7"}
                ),
            ]
        ),
        # Main content
        html.Div(
            style={
                "display"  : "flex",
                "flex"     : "1",
                "overflow" : "hidden",
                "minHeight": "0"
            },
            children=[
                # Map
                html.Div(
                    style={"width": "75%", "height": "100%"},
                    children=[
                        dl.Map(
                            id        = "constituency-map",
                            center    = [54.5, -3.5],
                            zoom      = 6,
                            minZoom   = 5,
                            maxZoom   = 12,
                            maxBounds = [[48, -12], [62, 4]],
                            style     = {
                                "height"         : "100%",
                                "width"          : "100%",
                                "backgroundColor": "white"
                            },
                            children=[
                                dl.GeoJSON(
                                    id            = "geojson-layer",
                                    data          = geojson_data,
                                    style         = style_function,
                                    hoverStyle    = hover_style,
                                    onEachFeature = tooltip_function,
                                    zoomToBounds  = True,
                                    n_clicks      = 0
                                )
                            ]
                        )
                    ]
                ),
                # Sidebar
                html.Div(
                    style={
                        "width"           : "25%",
                        "padding"         : "20px",
                        "overflowY"       : "auto",
                        "backgroundColor" : "#ffffff",
                        "borderLeft"      : "1px solid #ddd",
                        "boxShadow"       : "-2px 0 5px rgba(0,0,0,0.05)"
                    },
                    children=[
                        html.H3(
                            "National Seat Distribution",
                            style={
                                "textAlign"   : "center",
                                "marginBottom": "15px",
                                "fontSize"    : "16px",
                                "fontWeight"  : "700",
                                "color"       : "#1a1a2e"
                            }
                        ),
                        html.Table(
                            style={"width": "100%", "borderCollapse": "collapse"},
                            children=[
                                html.Thead(
                                    html.Tr([
                                        html.Th("Party", style={"textAlign": "left", "padding": "8px", "borderBottom": "2px solid #333", "fontSize": "13px"}),
                                        html.Th("Seats", style={"textAlign": "right", "padding": "8px", "borderBottom": "2px solid #333", "fontSize": "13px"})
                                    ])
                                ),
                                html.Tbody([
                                    html.Tr(
                                        style={"borderBottom": "1px solid #eee"},
                                        children=[
                                            html.Td(
                                                row["party"],
                                                style={
                                                    "padding"   : "8px",
                                                    "fontSize"  : "13px",
                                                    "color"     : party_colours.get(row["party"], "#000"),
                                                    "fontWeight": "600"
                                                }
                                            ),
                                            html.Td(
                                                row["seats"],
                                                style={
                                                    "padding"   : "8px",
                                                    "textAlign" : "right",
                                                    "fontSize"  : "13px",
                                                    "fontWeight": "700"
                                                }
                                            )
                                        ]
                                    )
                                    for row in seat_totals
                                ])
                            ]
                        )
                    ]
                )
            ]
        )
    ]
)

#--------------------------------------------------
# Run
#--------------------------------------------------
if __name__ == "__main__":
    app.run(debug=True)