# Install

---
### Building
+ [Guile](https://gnu.org/software/guile), version 3 or later
+ [GNU Make](https://gnu.org/software/make) to build using make

---
### Instructions
#### GNU Make
Simply issue `make` to build the Guile objects.  
If you wish to install them, you can alter the install locations with  
`PREFIX`, `DESTDIR`, `GUILE_SITE` and `GUILE_CCACHE`, E.g.:
```sh
make PREFIX=/usr GUILE_SITE=/usr/share/guile/site install
```
To uninstall, use `make uninstall`. You will need to provide the 
same variables on the command line if you overrode them when installing.

---
#### Manually
You can manually compile the Guile object files with the following command from the projects' root directory
```sh
find src -type f -name "*.scm" \
| cut -d. -f1 \
| xargs -I{} -r guild compile {}.scm -L src -O2 -o {}.go
```
And to install them, you can run the following (substituing 
`/path/to/install/` with your desired location):
```sh
install -m 644 src/god/*.scm -Dt      /path/to/install/site/god
install -m 644 src/god/conv/*.scm -Dt /path/to/install/site/god/conv
install -m 644 src/god/*.go -Dt       /path/to/install/site-ccache/god
install -m 644 src/god/conv/*.go -Dt  /path/to/install/site-ccache/god/conv

```


Guile libraries (depending on your environment), typically will be installed in:
+ `/usr/share/guile/site/3.0/<library>` for source files
+ `/usr/lib/guile/3.0/site-ccache/<library>` for compiled Guile object (`.go`) files

To find *exactly* where your environment stores these files, you can run:
```sh
# for source (.scm) files
guile -c '(map (lambda (x) (display x)(newline)) %load-path)'
# for compiled guile object (.go) files
guile -c '(map (lambda (x) (display x)(newline)) %load-compiled-path)'
```