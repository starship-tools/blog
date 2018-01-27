# Starship Tools Blog

*A Blog to Explore Really, Really Cool Stuff. Code Suff. In Space.*


#### Contents

* [Dependencies](#dependencies-)
* [Building](#building-)
* [License](#license-)


## Dependencies [&#x219F;](#contents)

* Clojure
* `lein`
* `npm`
* [SASS](http://sass-lang.com)
* AWS CLI

To set up the blog SASS infrastructure, run the following:

```bash
$ lein setup-sass
```


## Building [&#x219F;](#contents)

To setup non-content resources (e.g., css, images, a few static pages):

```
$ lein gen-all
```

After changes in SASS files, you can do:

```
$ lein gen-css
```

Updates to images, `.js` files, etc., will need this:

```
$ lein gen-assets
```

To (re)generate the content files, start up a Clojure REPL:

```bash
$ lein repl
```

Then start up the system, which includes serving content at
[http://localhost:5099](http://localhost:5099):

```clj
[starship.blog.dev] λ=> (startup)
```

To generate the blog:

```clj
[starship.blog.dev] λ=> (generate)
```


## License [&#x219F;](#contents)

```
Copyright © 2017 Starship Hackers

Distributed under the Apache License, Version 2.0.
```
