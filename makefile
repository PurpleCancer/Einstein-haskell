all: Main.hs EinStein/Game.hs EinStein/Graphics.hs
	ghc -dynamic Main -o einstein
