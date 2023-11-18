#!/bin/bash

mkdir -p build

ghc -outputdir=build -o build/adventure *.hs

cd build

# Check if compilation was successful
if [ $? -eq 0 ]; then
    echo "Compilation successful. Running the program..."
    ./adventure
else
    echo "Compilation failed."
fi