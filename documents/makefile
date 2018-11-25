readme.html: readme.md
	pandoc -o readme.html readme.md

readme.md: readme.txt
	kawa text2list.scm < readme.txt > readme.md
