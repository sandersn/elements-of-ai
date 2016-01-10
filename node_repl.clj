(require 'cljs.repl)
(require 'cljs.build.api)
(require 'cljs.repl.node)
(cljs.repl/repl (cljs.repl.node/repl-env))
