## Test environments

* local Windows 10 (64-Bit), R 4.1.0
* local Ubuntu Linux 20.04.04 LTS, R 4.2.1

## CMD check results

0 errors | 0 warnings | 0 notes

## Comments

- We changed maintainership from Thomas Peschel to Julia Palm

- When checking on Rhub, we sometimes get the message

```
  Found the following (possibly) invalid URLs:
       From: inst/doc/downloadResources.html
     URL: https://support.rstudio.com/hc/en-us/articles/360049776974-Using-RStudio-Server-in-Windows-WSL2
       Status: 403
       Message: Forbidden
```
 
I checked this in Postman and it seems the website only gives a http code 200 upon the first request within one session and sends http code 403 for all requests send after the first. This is not reproducible in a browser, where the website is always reachable, so we'd like to keep the link. 

## Downstream dependencies

There are currently no downstream dependencies for this package.
