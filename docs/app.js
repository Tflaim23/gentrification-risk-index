mapboxgl.accessToken = 'pk.eyJ1IjoidGZsYWltMjMiLCJhIjoiY21kM3EzdmFoMDg0ajJqcHhuZHY3YjRnMSJ9.1IEtKQJybEbgjnLS_l6MSw';

const map = new mapboxgl.Map({
  container: 'map',
  style: 'mapbox://styles/mapbox/light-v10',
  center: [-86.1581, 39.7684],
  zoom: 10
});

map.on('load', () => {
  Promise.all([
    d3.json('data/census_tracts.geojson'),
    d3.csv('data/scores_with_percentiles.csv', d3.autoType),
    d3.csv('data/tract_to_neighborhood.csv', d3.autoType)
  ]).then(([geojson, scores, neighborhoods]) => {
    const scoreMap = {};
    scores.forEach(row => {
      const key = `${row.GEOID}_${row.Year}`;
      scoreMap[key] = row;
    });

    const neighborhoodMap = {};
    neighborhoods.forEach(row => {
      neighborhoodMap[row.GEOID] = row.NEIGH_NAME_max;
    });

    geojson.features.forEach(feature => {
      const GEOID = feature.properties.GEOID;
      feature.properties.percentile = null;
      feature.properties.confidence = '';
      feature.properties.neighborhood = neighborhoodMap[GEOID] || 'Unknown';
    });

    map.addSource('tracts', {
      type: 'geojson',
      data: geojson
    });

    map.addLayer({
      id: 'tracts-fill',
      type: 'fill',
      source: 'tracts',
      paint: {
        'fill-color': [
          'case',
          ['==', ['get', 'percentile'], null], '#333333',
          ['interpolate', ['linear'], ['get', 'percentile'],
            0, '#08306b',
            10, '#2171b5',
            30, '#deebf7',
            50, '#ffffff',
            70, '#fcbba1',
            90, '#cb181d',
            100, '#67000d'
          ]
        ],
        'fill-opacity': 0.75,
        'fill-outline-color': '#ccc'
      }
    });

    map.addLayer({
      id: 'tracts-hover',
      type: 'line',
      source: 'tracts',
      paint: {
        'line-color': '#000000',
        'line-width': 2
      },
      filter: ['==', 'GEOID', '']
    });

    const popup = new mapboxgl.Popup({ closeButton: false, closeOnClick: false });

    map.on('mousemove', 'tracts-fill', (e) => {
      const feature = e.features[0];
      const props = feature.properties;
      const hasData = props.percentile !== null && !isNaN(props.percentile);
      const content = hasData
        ? `<strong>Neighborhood:</strong> ${props.neighborhood}<br>
           <strong>Tract GEOID:</strong> ${props.GEOID}<br>
           <strong>Confidence:</strong> ${props.confidence}<br>
           <strong>Percentile:</strong> ${Math.round(props.percentile)}%`
        : `<strong>Neighborhood:</strong> ${props.neighborhood}<br>
           <strong>Tract GEOID:</strong> ${props.GEOID}<br>
           <em>Not enough data</em>`;

      map.setFilter('tracts-hover', ['==', 'GEOID', props.GEOID]);
      popup.setLngLat(e.lngLat).setHTML(content).addTo(map);
    });

    map.on('mouseleave', 'tracts-fill', () => {
      map.setFilter('tracts-hover', ['==', 'GEOID', '']);
      popup.remove();
    });

    const yearSlider = document.getElementById('year');
    const yearValue = document.getElementById('year-value');
    const modeSelect = document.getElementById('mode');

    function updateMap() {
      const year = parseInt(yearSlider.value);
      const mode = modeSelect.value;

      geojson.features.forEach(f => {
        const GEOID = f.properties.GEOID;
        const key = `${GEOID}_${year}`;
        const row = scoreMap[key];

        if (row) {
          f.properties.percentile = row[mode];
          f.properties.confidence = row.Confidence;
        } else {
          f.properties.percentile = null;
          f.properties.confidence = 'N/A';
        }
      });

      map.getSource('tracts').setData(geojson);

      map.setPaintProperty('tracts-fill', 'fill-color-transition', {
        duration: 500,
        delay: 0
      });

      yearValue.textContent = year;
    }

    yearSlider.addEventListener('input', updateMap);
    modeSelect.addEventListener('change', updateMap);
    updateMap();

    map.on('resize', () => {
      document.getElementById('controls').style.display = 'block';
      document.getElementById('legend').style.display = 'block';
      document.getElementById('overlay-toggle').style.display = 'block';
    });

    const overlayCheckbox = document.getElementById('toggle-overlay');
    overlayCheckbox.addEventListener('change', () => {
      const visibility = overlayCheckbox.checked ? 'visible' : 'none';
      map.setLayoutProperty('tracts-fill', 'visibility', visibility);
    });
map.on('click', 'tracts-fill', (e) => {
  const feature = e.features[0];
  const bbox = turf.bbox(feature); 

  const [west, south, east, north] = bbox;

  const zillowURL = `https://www.zillow.com/homes/for_sale/?searchQueryState=` + encodeURIComponent(JSON.stringify({
    pagination: {},
    mapBounds: { west, south, east, north },
    isMapVisible: true,
    mapZoom: 15,
    filterState: {},
    isListVisible: true
  }));

  window.open(zillowURL, '_blank');
});


  });
});
