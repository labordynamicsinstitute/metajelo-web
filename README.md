# metajelo-web

[![Build Status](https://travis-ci.com/labordynamicsinstitute/metajelo-web.svg?token=fzz41xcnJ15QPD7QhZkZ&branch=master)](https://travis-ci.com/labordynamicsinstitute/metajelo-web)

Web tools to display
[metajelo](https://github.com/labordynamicsinstitute/metajelo) packages.

# Usage

## Installing CSS

### PureCSS

Pure CSS is used for responsive web design, but, you can use something else:
custom CSS or Bootstrap. Or, you can even omit it entirely, though this is
not generally recommended.

See the "Add Pure to Your Page "
and "Add the Viewport Meta Element" for how to install it under [Get
Started](https://purecss.io/start/), or check the `test/index.html`file in this
repository for how this is done.

### Default or Custom CSS Styles.

By default, CSS files are retrieved from a [separate repository](https://github.com/labordynamicsinstitute/metajelo-ui-css-classes) using `scripts/getcss`; the file `css/style.css` contains some default styles used in our examples.
Feel free to include it, or modify it (renaming the file is also possible):

```html
<link rel="stylesheet" href="css/style.css">
```

### Creating the application element

Here we make use of PureCSS's responsive grid: you can use an alternative grid system
instead, e.g. Bootstrap, but PureCSS was lighter weight and fit our needs.


```html
<div class= "pure-g">
  <div class="pure-u-1-1">
    <div id="metajelo_root"></div>
  </div>
</div>
```

# Building

If you have the relevant build tools intalled (npm, spago, pulp, etc.), you can
build using `npm run build && npm run prod`. For a more convient approach, see
the section on Docker below, and for complete build commands used in CI, see
`scripts/dist_build_commands.sh`.

## Docker

* Run `./psc.sh <command>`, e.g. `./psc.sh npm run prod`. This will run `npm run
prod` the command in the container with the CWD mounted and then exit.
Alternatively if you want to issue multiple commands in the container quickly,
you can run `./psc.sh bash`.


# API Documentation

API docs are available [on Pursuit](https://pursuit.purescript.org/packages/purescript-metajelo-web).
