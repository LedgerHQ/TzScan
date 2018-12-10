# TzScan

TzScan is a block explorer for the Tezos blockchain.

TzScan has been developed by [OCamlPro](http://www.ocamlpro.com) since
September 2017.

## Issues and Contributions

Check the project on [Gitlab.com](https://gitlab.com/tzscan/tzscan)
for the latest sources, issues and contributions:

* Issues: Check the [Issues](https://gitlab.com/tzscan/tzscan/issues)
  and submit a new one !
* Contributions: Check the
  [Contributions](https://gitlab.com/tzscan/tzscan/merge_requests)
  and submit your MR !

## Copyright and license

Copyright OCamlPro 2017-2018. This code is licensed under the terms
of the GNU Public License version 3 (GPL v3).

## Building

You need OCaml 4.06.1.

Update submodules:

```
git submodule init
git submodule update
```

### Dependencies

TzScan has multiple OCaml and non OCaml dependencies.
The non OCaml dependencies must be installed first as they are necessary
for the OCaml ones.

#### Non OCaml dependencies

On Debian 9:
```
sudo apt-get install\
     postgresql libsodium-dev libgeoip1 \
     geoip1-database libcurl4-gnutls-dev \
     curl zlib1g-dev libgeoip-dev
```

On Ubuntu:
```
sudo apt-get install\
     postgresql libsodium-dev libgeoip1 \
     geoip-database libcurl4-gnutls-dev \
     curl zlib1g-dev libgeoip-dev
```

#### OCaml dependencies

They can (and should) be installed with opam through
```
opam install
```

```
opam install\
     camlp4\
     re.1.7.3\
     ocplib-json-typed\
     ocurl\
     js_of_ocaml\
     js_of_ocaml-ppx\
     js_of_ocaml-camlp4\
     js_of_ocaml-tyxml\
     js_of_ocaml-lwt\
     omd\
     cohttp-lwt\
     cohttp-lwt-unix\
     base64\
     ezjsonm\
     ocplib-endian\
     geoip\
     ocp-build\
     nocrypto\
     sodium\
     lru\
     alcotest\
     calendar \
     lwt_log \
     csv-lwt
```

You will also need some development versions:
```
opam pin --dev ocplib-json-typed
opam install ocplib-json-typed
```

One last dependency is pgocaml.
The OPAM version is not compatible with tz-scan, also you should install from
the sources:
https://github.com/darioteixeira/pgocaml
```
./configure --enable-p4 --prefix $OPAM_SWITCH_PREFIX --docdir $OPAM_SWITCH_PREFIX/doc
make
make doc
make install
```

If `make install` fails, you might retry it after a `ocamlfind remove pgocaml`.

### Configuring

#### Configuring Postgresql

For TzScan to compile successfully, you will need your user to be able to
create tables in Postgresql. This is done with the following commands:
```
$ sudo -i -u postgres
$ psql
CREATE USER <user>;
ALTER ROLE <user> CREATEDB;
```
where `<user>` should be replaced by your login.


#### Configuring TzScan

Then, configure tz-scan:
```
./configure
```

You can create a file `Makefile.database` with the following options:

```
DATABASE=betanet
DESTSUBDIR=tzscan
WITH_VERSION=true
API_HOST:=localhost
API_PORT:=8080
NEED_PARSEXP=true
```

The options are:
* DATABASE: the database to use (you can use different names for other networks
    or tests)
* DESTSUBDIR: only used on the production server (the destination subdir
    in /var/www)
* WITH_VERSION: set to false to avoid recompiling everything everytime
* API_HOST & API_PORT: if you want to use localhost instead of api.tzscan.io
* NEED_PARSEXP: if you use sexplib (>= v0.11), then set it to true

### Building

You can now start the compilation:
```
make
```

If no error happened, you should get:
* A command called `tzscan-crawler`
* A command called `tzscan-api-server`
* A directory `www` containing at least an `explorer-main.js` file

## Usage

### Overview

TzScan is composed of the following components:
* A Tezos crawler, called `tzscan-crawler`. It regularly connects to a
  Tezos node to query new blocks, and register these blocks in a Postgresql
  database;
* An API server, called `tzscan-api-server`. It provides a REST RPC that can
  be used to read information from the Postgresql database. Some of the requests
  are also used to query local Tezos nodes;
* A webapp, stored in `www/`. The webapp runs some Javascript code to
  display information in a browser.

There are mostly two ways to contribute to TzScan:
* You can just use the webapp. For that, you just need a local web server.
  By default, it will connect to the official TzScan API server. If you
  do modifications, you can immediately check the results without running
  your own Tezos node.
* You can run a full TzScan node: you need to run a Tezos node,
  to run TzScan `tzscan-crawler` to fill the database, to run
  `tzscan-api-server` to give access to the database, and to run a
  local web server for the webapp.

### Testing the Interface Locally

You need to run a Web Server, that will give access to the `www/` directory
of the webapp. A simple way is to use the web server included in PHP:

First, install PHP:
```
sudo apt-get install php
```

Go into `www/`:
```
cd www
php -S localhost:8000
```

and view the webpage:
```
xdg-open http://localhost:8000/
```

If you didn't modify `API_PORT` and `API_HOST` in `Makefile.database`,
the webapp will use the official TzScan API Server. Otherwise, it will
try to connect to your API server.

Note that you edit `static/info.json` (that will be copied to
`www/info.json`) to change some informations on your web server, such
as the URL of the API server, the languages available or the networks
you are tracking.

### Filling the Database with the Crawler

Create a file `config.json` containing :
```
[
  {
    "name": "betanet-main",
    "crawler" : [
        {"url" : "http://your-tezos-node", "port" : 8132}
    ],
    "api" : [
        {"url" : "http://another-tezos-node", "port" : 8132 },
    ]
  }
]
```

Start the crawler:
```
./_tzscan-crawler config.json
```

It should immediately connect to the Tezos node, and rewind the chain
to register blocks from the genesis block to the latest one. Then, it
should wait and connect to the Tezos node every 3 seconds to query for new
blocks.

### Running the API Server Locally

You need first to have started the Tezos crawler as explained in the
previous section.

Modify file `Makefile.database` with:

```
API_PORT = 8080
API_HOST = localhost
```

and rebuild everything:
```
make
```

Start the API server (on port 8080):
```
./tzscan-api-server --node-config config.json --api-config www/api-config.json
```

Note that you might want to change `www/api-config.json` (and its
source in `static/api-config.json`) to suit the parameters of your
network, if you are running a private Tezos network. For example, you
can use:

```
tezos-client -A localhost -p 8132 rpc get /chains/main/blocks/head/context/constants
```

to get the constants of your network, and modify `api-config.json`
accordingly.

Note that, if you need to modify `static/info.json` or
`static/api-config.json`, it is usually a good idea to copy them in
`static/local/` and modify these files. They will be copied in `www/`
at every `make` command.
