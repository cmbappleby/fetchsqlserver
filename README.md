# fetchsqlserver_package
`fetchsqlserver` is an R package that helps with data package publication to the DataStore. Data tables are fetched using queries from a SQL Server database. Nearly everything you need to create a data package and the associated metadata are fetched as well, including data table column names, descriptions and data types to create metadata attribute tables, foreign key reference table and schema names to create categorical variable tables, as well as table descriptions. There are functions to convert data types to EML classes, add units to numeric classes, create categorical variable tables, add missing value codes and explanations to attribute tables, and to simplify the creation of EML metadata and adding NPS-specific metadata. The [user guide](https://github.com/cmbappleby/fetchsqlserver/blob/main/inst/doc/fetchsqlserver_user_guide.pdf) explains installing fetchsqlserver, setting up data tables queries, and creating the files needed by the package to create metadata. The package includes an R markdown template that walks through the creation of a data package.

If you have any questions, please contact Christina Appleby at [cmbappleby@gmail.com](cmbappleby@gmail.com).
