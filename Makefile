.PHONY: clean

build: soilro.exe

soilro.exe: main.hs InputDataParser.hs DataDef.hs
	ghc -o soilro.exe --make main.hs


clean:
	rm *.hi *.o *.exe

