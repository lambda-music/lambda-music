  
  About Build Files
====================== 

# lamu

- lamu/build.xml
  This builds class files the entire project.
  Because lamu depends every project in the repository. 

- lamu/build-jar.xml          
  This builds lamu.jar 
  
- lamu/buil-api-reference.xml :
  This builds api reference of pulsar.
 
- lamu/build-all.xml:
  This ant file invokes previous three ant files.
  This also invokes build-jar/build-api of Kawapad.

# kawapad

- kawapad/build.xml
  This builds class files of the kawapad and lib.
  Because kawapad only depends on `lib` project.
  
- kawapad/build-jar
  This creates kawapad.jar.
  
- kawapad/build-api
  This builds the Kawapad API reference and the Kawapad keystroke reference.

- kawapad/build-all.xml
  This and file invokes above three build files in `kawapad` project directory.  
 

# pulsar

Currently `pulsar` has no build system since it cannot run standalone. It could be possible
to create `pulsar` which runs standalone with a smaller footprint. This has not been done yet.



(Mon, 02 Mar 2020 22:38:26 +0900)
