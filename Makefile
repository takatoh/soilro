.PHONY: build clean

build: soilro.exe

soilro.exe: main.hs InputDataParser.hs DataDef.hs
	ghc -o soilro.exe --make main.hs


clean:
	del *.hi *.o *.exe soilro.exe.manifest

