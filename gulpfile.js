var gulp = require("gulp");
var ts = require("gulp-typescript");
var tsProject = ts.createProject("tsconfig.json");
var jasmine = require('gulp-jasmine');

gulp.task("default", function () {
    return tsProject.src()
        .pipe(tsProject())
        .js.pipe(gulp.dest("out"));
});
gulp.task("test", ["default"], function () {
    return gulp.src("out/**/test.js").pipe(jasmine())
});
gulp.task("shrink", ["default"], function () {
    var shrink = require('./out/chapter3/shrink');
    shrink.autoShrink();
});
gulp.task("linneus", ["default"], function () {
    var linneus = require('./out/chapter4/linneus');
    linneus.linneus();
});
gulp.task("painted-squares", ["default"], function () {
    var paintedSquares = require('./out/chapter5/painted-squares');
    paintedSquares.showSolution();
});
gulp.task("debug", ["default"], function () {
    return gulp.src("out/chapter5/test.js").pipe(jasmine({ verbose: true }));
});
