# Elemental

This is an R6-based rewrite of the modular framework we developed for www.zichtopzorgaanbieders.nl, a dashboard created at the Dutch Healthcare Authority (NZa, www.nza.nl/english), together with the Health and Youth Care Inspectorate (IGJ, english.igj.nl) and Information Hub Healthcare Fraud (IKZ, www.ikz.nl). All healthcare regulation specific content has been removed. 

It features a layout-as-code approach where the dashboard is created from a JSON specification. The JSON contains everything necessary to populate the dashboard with pages and modules, as well as layout information to put everything in its proper place. Dynamic reactive dependencies are created between modules based on information in the JSON (at runtime), rather than in the code (at creation). This means that modules can be created more independently from how they are used. For example, a module might need a certain variable from another module. In the code you simply assume this variable will be available and in the JSON you define from which module this variable is coming from.

More info coming.

