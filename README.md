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
    |---- mainDev.js    <- tells RequireJS how to load modules in DEV mode and bootstraps app
    |---- mainProd.js   <- tells RequireJS how to load modules in PROD mode and bootstraps app
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
* Go to localhost:9000

### Prod Mode

* Produce executable via `play stage`
* Run `target/start -Dhttp.port=9000`

