(add-hook 'java-mode-hook
          (lambda () (setq flycheck-java-ecj-jar-path "~/srv/java/ecj-4.4.jar")))
