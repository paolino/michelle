
clean:
	rm -f *.o 
	rm -f *.hi
	rm -f `find . -maxdepth 1 -perm -u=x -type f`
git:
	git add *.hs
	git add README
	git add LICENSE
	git add makefile
	haddock -o docs -h *.hs	
	git commit
	git push
	

edit:
	gvim *.hs README LICENSE makefile

