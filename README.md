# Architecture 2030 Baseline Tool in partnership with Maalka (http://www.maalka.com) for Assessing Building Energy Efficiency Relative to National Median Performance for similar building types.

## Intro

The EPA is in the process of updating its comparison and goal setting tools to be driven by newly available 2012 data.
 The goal of this tool is to provide users (e.g. building owners, 2030 members) with an engaging and intuitive tool
 with which they can effectively compare their energy-use intensity with similar buildings types, understanding
 the change of their energy-use efficiency over time, and to set energy-use intensity targets for achieving desired
 performance scores based on 2003 CBECS data. Basing the tool functionality on the 2003 CBECS dataset will enable
2030 members to maintain pre-existing goals and baselines so that progress can continue to be consistently measured

## Code Organization

The JavaScript modules are organized as follows:

    |- app
    |-- assets
    |--- javascripts    <- contains all the JavaScript/CoffeeScript modules
    |---- app.js        <- app module, wires everything together
    |---- main.js       <- tells RequireJS how to load modules and bootstraps the app
    |---- common/       <- a module, in this case
    |----- main.js      <- main file of the module, loads all sub-files in this folder
    |----- filters.js   <- common's filters
    |----- directives/  <- common's directives
    |----- services/    <- common's services
    |---- ...


## Trying It Out

### Install SBT
 [http://www.scala-sbt.org/release/docs/Setup.html](http://www.scala-sbt.org/release/docs/Setup.html)
 
### Dev Mode

* Load dependencies via `sbt update`
* Run via `sbt ~run`
* Go to [localhost:9000](http://localhost:9000)

This uses the normal JavaScript files and loads libraries from the downloaded WebJars.

### Prod Mode

Running:

* Run `sbt testProd`

Deployment:

* Produce executable via `sbt clean dist`
* Extract `unzip target/universal/maalka-benchmark--2.x.x.zip`
* Run `maalka-benchmark--2.x.x/bin/maalka-benchmark- -Dhttp.port=9000 -Dconfig.resource=prod.conf`


This uses the uglified JavaScript files, versioned and compressed assets, and loads WebJars resources from the jsDelivr CDN.

### Install deb
* run sbt debian:packageBin
* dpkg -i target/maalka-benchmark_<<VERSION>>.deb
* apt-get install -f

To remove a previous version of maalka-benchmark
* dpkg --remove maalka-benchmark

To see what version is installed
* dpkg-query -l | grep maalka-benchmark
