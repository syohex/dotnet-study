all:
	echo "hello world"

.PHONY: clean
clean:
	find . -name "bin" -type d | xargs rm -rf
	find . -name "obj" -type d | xargs rm -rf
	find . -name ".ionide" -type d | xargs rm -rf
