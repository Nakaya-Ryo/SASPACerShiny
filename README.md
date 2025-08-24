# SASPACerShiny (latest version 0.0.1 on 23August2025)
An R package(Shiny app) to help creating SAS packages

<img src="https://github.com/Nakaya-Ryo/SASPACer_shiny/blob/main/SASPACerShiny_logo.png?raw=true" alt="onigiri" width="300"/><img src="https://github.com/Nakaya-Ryo/SASPACer_shiny/blob/main/shiny.png?raw=true" alt="shiny" width="240"/>

**"サスパッカー"** in the logo stands for **SASPACer** in Japanese. Original SASPACer is a SAS package to support creating SAS packages. SASPACer **Shiny** is an R shiny application with the same purpose. By providing GUI interface, SASPACer Shiny supports R users to create SAS packages. Once SAS package source folder/file structure is done by SASPACer Shiny, users should run %generatePackage() in SAS Package Framework(SPF) using SAS to complete creating SAS packages. By the way, original SASPACer is suite tool until completion of creating SAS packages.

Sample R code (to create SAS package folder/file structure):
~~~r
remotes::install_github("Nakaya-Ryo/SASPACershiny")
SASPACerShiny::run_app()
~~~
Sample SAS code (to complete SAS package generation):
~~~sas
%generatePackage(
	filesLocation=\path\to\your\source\package\folder ,
	markdownDoc=1, easyArch=1) 
~~~

You can learn from the following training materials by Bartosz Jablonski for source files and folders structure of SAS packages.  
[My first SAS Package -a How To](https://github.com/yabwon/SAS_PACKAGES/blob/main/SPF/Documentation/SAS(r)%20packages%20-%20the%20way%20to%20share%20(a%20how%20to)-%20Paper%204725-2020%20-%20extended.pdf)   
[SAS Packages - The Way To Share (a How To)](https://github.com/yabwon/SAS_PACKAGES/blob/main/SPF/Documentation/SAS(r)%20packages%20-%20the%20way%20to%20share%20(a%20how%20to)-%20Paper%204725-2020%20-%20extended.pdf)  

## Version history  
0.0.1(23August2025)	: Initial version

## What is SAS Packages?
The package is built on top of **SAS Packages framework(SPF)** developed by Bartosz Jablonski.  
For more information about SAS Packages framework, see [SAS_PACKAGES](https://github.com/yabwon/SAS_PACKAGES).  
You can also find more SAS Packages(SASPACs) in [SASPAC](https://github.com/SASPAC).

## How to use SAS Packages? (quick start)
### 1. Set-up SPF(SAS Packages Framework)
Firstly, create directory for your packages and assign a fileref to it.
~~~sas      
filename packages "\path\to\your\packages";
~~~
Secondly, enable the SAS Packages Framework.  
(If you don't have SAS Packages Framework installed, follow the instruction in [SPF documentation](https://github.com/yabwon/SAS_PACKAGES/tree/main/SPF/Documentation) to install SAS Packages Framework.)  
~~~sas      
%include packages(SPFinit.sas)
~~~  
### 2. Install SAS package  
Install SAS package you want to use using %installPackage() in SPFinit.sas.
~~~sas      
%installPackage(packagename, sourcePath=\github\path\for\packagename)
~~~
(e.g. %installPackage(ABC, sourcePath=https://github.com/XXXXX/ABC/raw/main/))  
### 3. Load SAS package  
Load SAS package you want to use using %loadPackage() in SPFinit.sas.
~~~sas      
%loadPackage(packagename)
~~~
### Enjoy😁
---

