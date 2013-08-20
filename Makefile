bfi: bfi.hs
	ghc -Wall $< -o $@

clean:
	rm -f *.hi *.o *~ bfi
