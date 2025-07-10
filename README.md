## guile-god
---
This library is the reference implementation of a parser for the 
[God](https://github.com/wreedb/god) data format.

#### Building/Installation
See [INSTALL.md](./INSTALL.md)

#### Usage
Once installed, you can utilize it like so:
```scheme
(use-modules (ice-9 pretty-print)
             (god))

(define sample (god-parse-file "example/simple.god"))
(pretty-print (god-doc->scm sample))
(pretty-print
  (god-parse-raw "{ name = \"Will\"; age = 26; }"))
```
the `(god conv hash)` submodule can be used to convert into a hash table, 
as the default is a nested association list.

---
#### Licensing
This repository is licensed under the GNU Lesser General Public License, version 3.0.
See [LICENSE.md](./LICENSE.md) for the license text itself, or the [dedicated page](https://www.gnu.org/licenses/lgpl-3.0.en.html).
Files within the `doc` directory are licensed under the GNU Free Documentation 
License, version 1.3.