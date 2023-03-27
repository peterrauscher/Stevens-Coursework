const gulp = require("gulp");
const concatenate = require("gulp-concat");
const cleanCSS = require("gulp-clean-css");
const autoPrefix = require("gulp-autoprefixer");
const gulpSASS = require("gulp-sass")(require("sass"));
const uglify = require("gulp-uglify");
const rename = require("gulp-rename");
const imagemin = require("gulp-imagemin");

const sassFiles = [
  "./src/styles/custom.scss",
  "./node_modules/bootstrap/scss/_variables.scss",
];

const vendorJsFiles = [
  "./node_modules/jquery/dist/jquery.js",
  "./node_modules/bootstrap/dist/js/bootstrap.js",
];

gulp.task("sass", function (done) {
  gulp
    .src(sassFiles)
    .pipe(gulpSASS())
    .pipe(concatenate("styles.css"))
    .pipe(gulp.dest("./public/css/"))
    .pipe(autoPrefix())
    .pipe(cleanCSS())
    .pipe(rename("styles.min.css"))
    .pipe(gulp.dest("./public/css/"));
  done();
});

gulp.task("js:vendor", function (done) {
  gulp
    .src(vendorJsFiles)
    .pipe(concatenate("vendor.js"))
    .pipe(gulp.dest("./public/js/"))
    .pipe(uglify())
    .pipe(rename("vendor.min.js"))
    .pipe(gulp.dest("./public/js/"));
  done();
});

gulp.task("imagemin", function (done) {
  var imgSrc = "public/images/*.+(png|jpg|gif)",
    imgDst = "public/minages";
  gulp.src(imgSrc).pipe(imagemin()).pipe(gulp.dest(imgDst));
  done();
});

gulp.task("build", gulp.parallel(["sass", "js:vendor", "imagemin"]));

gulp.task("watch", function (done) {
  gulp.watch(sassFiles, gulp.series("sass"));
  gulp.watch(vendorJsFiles, gulp.series("js:vendor"));
  done();
});

gulp.task("default", gulp.series("watch"));
