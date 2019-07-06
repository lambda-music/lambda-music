readme.html: readme.md
	pandoc -o out/readme.html out/readme.md

readme.md: readme.txt
	kawa lib/text2list.scm < readme.txt > out/readme.md

clean:
	rm out/readme.md out/readme.html
