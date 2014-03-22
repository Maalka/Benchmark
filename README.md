# Example application for Play+AngularJS+RequireJS.

## Intro

This template application shows how to organize your application with
[Playframework 2](http://www.playframework.com), [WebJars](http://www.webjars.org),
[RequireJS](http://www.requirejs.org) and [AngularJS](http://www.angularjs.org).

It shows an alternative way of organizing modules than the official
[Angular-Play-Seed](https://github.com/typesafehub/angular-seed-play).

For a full explanation, read the following blog post:
http://www.mariussoutier.com/blog/2013/08/25/requirejs-optimization-play-webjars/


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

* Load dependencies via `play update`
* Run via `play ~run`
* Go to [localhost:9000](http://localhost:9000)

This uses the normal JavaScript files and loads libraries from the downloaded WebJars.

### Prod Mode

* Produce executable via `play clean dist`
* Extract `unzip target/universal/play-angular-require-seed-2.2.2.zip`
* Run `play-angular-require-seed-2.2.2/bin/play-angular-require-seed -Dhttp.port=9000 -Dconfig.resource=prod.conf`

This uses the uglified JavaScript files and loads WebJars resources from the jsDelivr CDN.

### Activator

This application is also published an a Typesafe Activator template:

http://typesafe.com/activator/template/play-with-angular-requirejs
