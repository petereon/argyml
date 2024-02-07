fmt:
    ormolu --mode inplace $(find app -name '*.hs')
    hlint $(find app -name '*.hs')
    cabal-fmt *.cabal -i

watch target:
    watchexec -e cabal,hs -c -r "just {{target}}"
