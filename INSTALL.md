# Install

---
### Building
+ [Guile](https://gnu.org/software/guile), version 3 or later
+ [GNU Make](https://gnu.org/software/make) to build using make

---
### Instructions
#### GNU Make
Use the `configure` script to prepare the Makefile with your 
desired install locations:
```sh
./configure --prefix=/usr/local
make
make check # to run the tests
make install # with root (sudo) priviledge if needed
make uninstall # to uninstall
```
###### Extra Documentation
You may also install the documentation in html, pdf and epub formats:
```sh
make html pdf epub
make install-html install-pdf install-epub
make uninstall-html uninstall-pdf uninstall-epub
```

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
install -m 644 src/god.scm -Dt        /path/to/guile/site/3.0
install -m 644 src/god/*.scm -Dt      /path/to/guile/site/3.0/
install -m 644 src/god/conv/*.scm -Dt /path/to/guile/site/god/conv
install -m 644 src/god.go -Dt         /path/to/guile/3.0/site-ccache
install -m 644 src/god/*.go -Dt       /path/to/guile/3.0/site-ccache/god
install -m 644 src/god/conv/*.go -Dt  /path/to/guile/3.0/site-ccache/god/conv

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