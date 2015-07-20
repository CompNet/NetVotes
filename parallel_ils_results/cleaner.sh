#!/bin/bash

for dir in ./*/; do
	cd "$dir"
	find . -name "cc-result.txt" -type f -exec mv {} . \;
	rm -r graph.g
	cd ..
done
