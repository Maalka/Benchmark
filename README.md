# Example application for Play+AngularJS+RequireJS.

## Blog Post

http://www.mariussoutier.com/blog/2013/08/25/requirejs-optimization-play-webjars/

## Summary

A small app that shows how to organize your application with
[Play](http://www.playframework.com), [WebJars](http://www.webjars.org),
[RequireJS](http://www.requirejs.org) and [AngularJS](http://www.angularjs.org).

It shows an alternative way of organizing modules than the official
[Angular-Play-Seed](https://github.com/typesafehub/angular-seed-play).

## Trying It Out

### Dev Mode

* Load dependencies via `play update`
* Run via `play ~run`
* Go to localhost:9000

### Prod Mode

* Produce executable via `play stage`
* Run `target/start -Dhttp.port=9000`
