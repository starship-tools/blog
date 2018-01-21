<img src="/assets/images/clojure.png" alt="Clojure Logo" class="about-portrait img-responsive"></span>

The code that generates this site is hosted on
[Github](https://github.com/starship-tools/blog). It's written using the
following open source languages/libraries:

* [Clojure programming language](https://clojure.org/)
* The [Dragon](https://github.com/clojusc/dragon) static site generator,
  which in turn incorporates functionality from:
  * the [Stasis](https://github.com/magnars/stasis) library
  * [Selmer page templates](https://github.com/yogthos/Selmer) (Django-based)
  * and [more](https://github.com/clojusc/dragon/blob/master/project.clj#L10)
* The [{less}](http://lesscss.org/about/) CSS compiler

The generated content is then published to
[AWS S3](https://aws.amazon.com/s3/) and served from there.
