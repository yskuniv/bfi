bfi: bfi.hs
	ghc $< -o $@

clean:
	rm -f *.hi *.o *~ bfi
