# [Components for visualization of correlations for IVIS framework](https://is.cuni.cz/studium/dipl_st/index.php?id=e0154501eaae5a6c0ae6b10944ee0bb8&tid=1&do=main&doo=detail&did=222158)

## Zásady pro vypracování

IVIS is a web-based data processing and visualization framework that provides components to setup highly customized visualization. IVIS relies on Node.js on the server side and the ReactJS and D3.js libraries on the client side. While IVIS has some basic component for visualizing time-series data, predominantly in the form of a line-chart, it lacks visualization components for deeper analytics insights such as correlations among data (e.g. scatter plot, histograms, etc.)

The goal of this thesis is to implement components for statistical graphs, mainly focusing on correlation of two or more variables. In particular, the thesis will provide components for histogram, scatterplot, and heatmap (histogram of two variables). These components will be connected to existing parts of the framework. Special emphasis will be given to interactivity of the plots.

## Seznam odborné literatury

- D3.js library reference (https://github.com/d3/d3/wiki)
- ReactJS documentation (https://reactjs.org/docs/)
- Elasticsearch documentatnion (https://www.elastic.co/guide/index.html)
- Yan Holtz: From Data to Viz (https://www.data-to-viz.com/) and The D3.js Graph Gallery (https://www.d3-graph-gallery.com/)

# [Animated visualizations for IVIS framework](https://is.cuni.cz/studium/dipl_st/index.php?id=16911be04829acc64b2fc2eb4878f3a6&tid=4&do=main&doo=detail&did=223643)

## Zásady pro vypracování

VIS is a web framework offering a platform for data visualization and analysis. It uses various technologies, namely Node.js as the server-side runtime, ReactJS library for building UI components and D3 library for creating data visualizations.

The goal of this thesis is to extend IVIS's visualization capabilities by creating a foundation for animated visualization components. The
thesis should demonstrate this by providing examples of complex animations of components SVG, PieChart and BarChart with clear UI allowing the user to control the animation. The solution should support both animations over pre-existing data and animations from data generated on the fly.

## Seznam odborné literatury

- D3.js library reference (https://github.com/d3/d3/wiki)
- ReactJS documentation (https://reactjs.org/docs/)
- Node.js runtime: https://nodejs.org/en/
- IVIS project on GitHub: https://github.com/smartarch/ivis-core