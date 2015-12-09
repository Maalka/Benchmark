var gulp = require('gulp'),
    jshint = require('gulp-jshint'),
    del = require('del'),
    rename = require('gulp-rename'),
    minifyCSS = require('gulp-minify-css'),
    sourcemaps = require('gulp-sourcemaps'),
    uglify = require('gulp-uglifyjs');

gulp.task('clean', function () {
    del(['daterangepicker.min.*']);
});

gulp.task('jshint', function () {
    gulp.src(['gulpfile.js', 'daterangepicker.js'])
        .pipe(jshint())
        .pipe(jshint.reporter('jshint-stylish'));
});

gulp.task('minifycss', function () {
    gulp.src('daterangepicker.css')
        .pipe(sourcemaps.init())
        .pipe(minifyCSS())
        .pipe(rename("daterangepicker.min.css"))
        .pipe(sourcemaps.write('.'))
        .pipe(gulp.dest('.'));
});

gulp.task('uglifyjs', function () {
    gulp.src('daterangepicker.js')
        .pipe(uglify('daterangepicker.min.js', {
            outSourceMap: true
        }))
        .pipe(gulp.dest('.'));
});

gulp.task('default', ['clean', 'jshint', 'minifycss', 'uglifyjs']);
