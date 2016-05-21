var gulp = require("gulp");
var ts = require("gulp-typescript");
var tsProject = ts.createProject("tsconfig.json");
var jasmine = require('gulp-jasmine');

gulp.task("default", function () {
    return tsProject.src()
        .pipe(ts(tsProject))
        .js.pipe(gulp.dest("out"));
});
gulp.task("test", ["default"], function () {
    gulp.src("out/**/test.js").pipe(jasmine())
});
