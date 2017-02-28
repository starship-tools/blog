# Blog Templates

These templates are ErlyDTL (Erlang Django Template Language) and are
automatically compiled when you run `rebar3 compile`.

1. This blog expects its templates to be in `priv/templates`
1. This blog expects template files to end in the `.html` extension

When the blog ErlyDTL templates are compiled, several things happen:

1. An `.erl` source file is generated for each template, saved in the
   same directory as the template
1. A `.beam` file is saved to the default `ebin` directory for the project;
   note, however, that `-tmpl` is appended to the file name, e.g.: the
   template `base.html` is compiled to `base-tmpl.beam`
1. The compiled `.beam` file exports the `render/{0,1,2}` functions. These are
   what the static site generator for the blog uses to generate the site
   content from the templates.

As for the templates themselves, here are some quick link docs for your
reference:

* [ErlyDTL](https://django.readthedocs.io/en/1.6.x/ref/templates/builtins.html#include)
* [Template Inheritance](https://django.readthedocs.io/en/1.6.x/topics/templates.html#template-inheritance)
* Commonly used tags:
  * [for](https://django.readthedocs.io/en/1.6.x/ref/templates/builtins.html#for)
  * [ifequal](https://django.readthedocs.io/en/1.6.x/ref/templates/builtins.html#ifequal)
  * [include](https://django.readthedocs.io/en/1.6.x/ref/templates/builtins.html#include)
