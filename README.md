# Starship Tools Blog

[![][lfe-tiny]][lfe-large]

*A Blog to Explore Really, Really Cool Stuff. Code Suff. In Space.*


#### Contents

* [Dependencies](#dependencies-)
* [Building](#building-)
   * [With make](#with-make-)
   * [In the REPL](#in-the-repl-)
* [License](#license-)


## Dependencies [&#x219F;](#contents)

* Erlang
* `rebar3`
* `npm`

To set up the blog SASS infrastructure, run the following:

```bash
$ make gulp-setup
```

## Building

There are two supported ways of building the Starship Hackers Blog HTML
content and CSS:

* using `make` targets (which call erlang under the hood)
* running `blog` functions from the LFE REPL


### With `make` [&#x219F;](#contents)

To (re)generate the static files:

```bash
$ make blog
```

That will build both the HTML files as well as the CSS.

To only build the HTML:

```bash
$ make blog-html-only
```

To only build the CSS, JS, etc.:

```bash
$ make assets
```

Additionally, a `make` target is provided which compiles everything fresh,
starts up a local dev HTTP server, and watches for changes in CSS, HTML
templates, and LFE code:

```bash
$ make serve-watch
```

The CSS watcher is a backgrounded `sass` process, and not native LFE, so you
will need to kill it when you are done:

```bash
$ make css-unwatch
```


### In the REPL [&#x219F;](#contents)

To (re)generate the static files, start up an LFE REPL:

```bash
$ make repl
```
```
Erlang/OTP 18 [erts-7.3] [source] [64-bit] [smp:4:4] [async-threads:10] ...

   ..-~.~_~---..
  (      \\     )    |   A Lisp-2+ on the Erlang VM
  |`-.._/_\\_.-':    |   Type (help) for usage info.
  |         g |_ \   |
  |        n    | |  |   Docs: http://docs.lfe.io/
  |       a    / /   |   Source: http://github.com/rvirding/lfe
   \     l    |_/    |
    \   r     /      |   LFE v1.3-dev (abort with ^G)
     `-E___.-'

lfe> (blog:start)
ok
```

To generate the blog:

```cl
lfe> (blog:gen)
Created docs/index.html.
...
ok
```


To run a local copy of the development server and view your work at
[http://localhost:8080](http://localhost:8080), run the following:

```bash
lfe> (blog:httpd)
ok
```

The CSS files are managed with [sass](http://sass-lang.com). After changing
values in the `priv/sass/lfe*.scss` files or in the
`priv/sass/lfe-sass/` subdirectories, you'll need to rebuild:

```bash
$ make assets
```


## License [&#x219F;](#contents)

```
Copyright © 2017 Starship Hackers

Distributed under the Apache License, Version 2.0.
```


[lfe-tiny]: priv/static/images/logos/lfe-tiny.png
[lfe-large]: priv/static/images/logos/lfe-large.png
