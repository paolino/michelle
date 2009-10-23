
clean:
	rm -f *.o 
	rm -f *.hi
	rm -f `find . -maxdepth 1 -perm -u=x -type f`
git:
	git add *.hs
	git add README
	git add LICENSE
	git add TODO
	git add makefile
	git add docs
	git commit
	git push
	
doc:
	haddock -o docs -h *.hs	
edit:
	gvim *.hs README LICENSE TODO makefile

