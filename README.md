![Monet](./images/monet-overview.png)

A web editor for mixed pixel-svg graphical effects.

### Setup

Monet runs entirely on the front-end using [Scala JS](https://www.scala-js.org/). We use the [Mill](http://www.lihaoyi.com/mill/) build tool for Scala, which must be installed first. One ce this is done, we run:

```
npm install             // Adds esbuild dependency for minification
mill minifier --watch   // Compiles frontend using scala.js
```

to compile the code on any changes. `index.html` can then be loaded in any browser or served locally.
