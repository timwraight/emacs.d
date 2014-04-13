(:name lalopmak-vimp
       :type github
       :pkgname "lalopmak/lalopmak-evil"
       :features lalopmak-vimp
       :build (("rename 's/evil/vimp/' lalopmak-evil*")
               ("sed -i .orig -e 's/evil/vimp/g' *.el"))
       :depends (vimp ace-jump-mode vimp-surround))
