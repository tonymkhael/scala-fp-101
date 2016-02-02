# Hello

This repository contains a tutorial (Written in asciidoctor) on how to use asciidoctor.

# How to build

The code builds using gradle.

Output formats are html5, pdf, and revealJs slides.

* To generate html5 documentation, run *gradlew generateHtml5*
* To generate pdf documentation, run *gradlew generatePdf*
* To generate revealJs documentation, run *gradlew generateSlides*

If you want to generate all formats at one, run *gradlew generateAll*

# Note on grade.properties

Since you're probably running behind the MX proxy create a *gradle.properties* file with the below *sample* content, which you will need to modify in order to specify for example a specific JDK path (like I had to)

```
#control daemon mode
org.gradle.daemon=true
#used for gradle JVM
systemProp.https.proxyHost=proxy
systemProp.https.proxyPort=3128
systemProp.http.proxyHost=proxy
systemProp.http.proxyPort=3128
systemProp.http.nonProxyHosts=*.murex.com|localhost

#use that specific JDK
org.gradle.java.home=C:\\jdk1.7.0_79-64b
```
