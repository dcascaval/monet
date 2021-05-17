![Monet](./images/monet-overview.png)

A web editor for mixed pixel-svg graphical effects.

### Setup

Note: this project uses Scala 3, and so will need an SBT version >= 1.5.0

```
npm install             // Adds esbuild dependency for minification
sbt
> ~fastOptJS            // Compiles to JS and watches for changes
```

`index.html` can then be loaded in any browser or served locally.

<!-- Monet runs entirely on the front-end using [Scala JS](https://www.scala-js.org/). We use the [Mill](http://www.lihaoyi.com/mill/) build tool for Scala, which must be installed first. One ce this is done, we run:

```
npm install             // Adds esbuild dependency for minification
mill minifier --watch   // Compiles frontend using scala.js
```

DC: I would like to use Mill again eventually (easier to script against) but for now the project is small enough that it doesn't matter.
-->
