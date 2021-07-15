# Anomaly detection support for IVIS framework

## Zásady pro vypracování

> IVIS is a web-based data processing and visualization framework that provides components to setup highly customized visualization. IVIS relies on Node.js on the server side and the ReactJS and D3.js libraries on the client side. While IVIS has some basic component for visualizing time-series data, predominantly in the form of a line-chart, it lacks visualization components for deeper analytics insights such as correlations among data (e.g. scatter plot, histograms, etc.)

IVIS is a web-based data processing and visualization framework that provides components to setup highly customized visualization. IVIS relies on Node.js on the server side with support for jobs in Python and the ReactJS and D3.js libraries on the client side.

Since IVIS works with and stores data, it could be expected of it to also support forms of data analysis.

> The goal of this thesis is to implement components for statistical graphs, mainly focusing on correlation of two or more variables. In particular, the thesis will provide components for histogram, scatterplot, and heatmap (histogram of two variables). These components will be connected to existing parts of the framework. Special emphasis will be given to interactivity of the plots.

The goal of this thesis is to implement support for a new way of analysing data stored inside the system - it is to connect an anomaly detection framework and include visualization of the outputs. In particular, the thesis will provide components for creating and managing such analyses and also a *graphical* panel where an overview of the analysis can be viewed. Special emphasis will be given to complete coverage of the anomaly detection frameworks functions.

*An eventual goal of the system could be to have an entirely new section called Analysis or Processing, which would contain the already present data aggregation and the planned anomaly detection, as well as other planned sections.*

## Seznam odborné literatury

- A Framework for Tunable Anomaly Detection (https://wwwbroy.in.tum.de/~gerostat/pubs/ICSA19-AnomalyDetection-camera-ready.pdf)

>- D3.js library reference (https://github.com/d3/d3/wiki)
>- ReactJS documentation (https://reactjs.org/docs/)
>- Elasticsearch documentatnion (https://www.elastic.co/guide/index.html)
>- Yan Holtz: From Data to Viz (https://www.data-to-viz.com/) and The D3.js Graph Gallery (https://www.d3-graph-gallery.com/)