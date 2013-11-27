(setq semantic-load-turn-everything-on t)
(semantic-mode 1)

(require 'semantic/lex)
(require 'semantic/senator)
(require 'semantic/java)
(require 'semantic/ia)
(require 'semantic/ctxt)
(require 'semantic/wisent)
(require 'semantic/wisent/comp)
(require 'semantic/wisent/java-tags)

;; Use the full Java 1.5 grammer to parse Java
(autoload 'wisent-java-default-setup "wisent" "Hook run to setup Semantic in 'java-mode'." nil nil)
(require 'cl)
(require 'jdibug)


