all: cjp2p


deps: cabal.txt
	cabal install --lib $$(cat $<)
	>$@

 
cjp2p: cjp2p.hs deps
#	ghc -make cjp-haskel.hs
	ghc --make cjp2p.hs
