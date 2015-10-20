# Example application for Play+AngularJS+RequireJS.

## Intro

This template application shows how to organize your application with
[Playframework 2](http://www.playframework.com), [WebJars](http://www.webjars.org),
[RequireJS](http://www.requirejs.org) and [AngularJS](http://www.angularjs.org).

It shows an alternative way of organizing modules than the official
[Angular-Play-Seed](https://github.com/typesafehub/angular-seed-play).

For a full explanation, read the following blog post:
http://www.mariussoutier.com/blog/2013/08/25/requirejs-optimization-play-webjars/

The updates made for Play 2.2 and newer WebJars are explained here:
http://www.mariussoutier.com/blog/2014/03/25/requirejs-optimization-play-2-2-webjars-cdns/

The changes for Play 2.3 and sbt-web are discussed in this series about sbt-web:
http://mariussoutier.com/blog/2014/10/20/intro-sbt-web/ 

The changes for Play 2.4 are summarized in this short summary post:
http://mariussoutier.com/blog/2015/07/25/play-angular-require-seed-updates/

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
* Extract `unzip target/universal/play-angular-require-seed-2.x.x.zip`
* Run `play-angular-require-seed-2.x.x/bin/play-angular-require-seed -Dhttp.port=9000 -Dconfig.resource=prod.conf`


This uses the uglified JavaScript files, versioned and compressed assets, and loads WebJars resources from the jsDelivr CDN.

### Activator

This application is also published as a Typesafe Activator template:

http://typesafe.com/activator/template/play-angular-require-seed

To use it from Activator, type

~~~bash
$ activator new <project-name>
~~~

Activator will now ask you to choose a template:

~~~bash
Browse the list of templates: http://typesafe.com/activator/templates
Choose from these featured templates or enter a template name:
  1) minimal-java
  2) minimal-scala
  3) play-java
  4) play-scala
> 
~~~

Enter `play-angular-require-seed` (you can use tab completion).

~~~bash
> play-angular-require-seed
OK, application "<project-name>" is being created using the "play-angular-require-seed" template.
~~~

Your new app is now ready for development. Switch to its directory and run activator or sbt.

~~~bash
$ cd <project-name>
$ activator ~run
~~~
# Benchmark
