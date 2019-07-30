DATABASE:=betanet
DESTSUBDIR:=tzscan
WITH_EXTRAS:=false
WITH_VERSION:=true
HAS_PGOCAML:=true
API_PORT:=
API_HOST:=
AUTO_UPGRADE:=false
NEED_PARSEXP:=false

# Keep this flag until the ezpg table has been updated
DBOLDINFO=--old-info

# for betanet, create a file Makefile.database with the (uncommented) line:
# DATABASE=alphanet
# DESTSUBDIR=alphanet-tzscan
# WITH_EXTRAS:=false
# WITH_VERSION:=true
# HAS_PGOCAML:=true
# This is useful if you don't want 'db-update' to be done automatically
# AUTO_UPGRADE:=false

# for zeronet, create a file Makefile.database with the (uncommented) line:
# DATABASE=zeronet
# DESTSUBDIR=zeronet-tzscan
# WITH_EXTRAS:=false
# WITH_VERSION:=true

#If you want to host locally:
#API_PORT:=8000
#API_HOST:=localhost

# Keep this flag until the ezpg table has been updated
DBOLDINFO=--old-info

-include Makefile.database

DEST_DIR ?= ${CURDIR}/www

# do not do `git submodule init/update` here, it MUST be done in ./configure
# otherwise, it is impossible to compile without committing first the
# submodules
all: _obuild may-db-update
	PGDATABASE=$(DATABASE) ocp-build
	cp -f _obuild/tzscan-crawler/tzscan-crawler.asm tzscan-crawler
	cp -f _obuild/tzscan-api-server/tzscan-api-server.asm tzscan-api-server
	$(MAKE) website

may-db-update: ocp-autoconf/config.ocp2inc
ifeq (${AUTO_UPGRADE},true)
	$(MAKE) force-db-update
endif

force-db-update:
	$(MAKE) db-update

website:
	mkdir -p ${DEST_DIR}
	rsync -av --delete static/* ${DEST_DIR}
ifeq (${API_HOST},)
	sed -e 's|%%API%%|api1.tzscan.io|' \
		static/info.json > www/info.json
else
	sed -e 's|%%API%%|${API_HOST}:${API_PORT}|' \
		static/info.json > www/info.json
endif
	rsync -rv static/local/* ${DEST_DIR}
	cp _obuild/explorer-main/explorer-main.js ${DEST_DIR}

submodule:
	git submodule update

client:
	ocp-build explorer-main
	$(MAKE) website

ocp-autoconf/config.ocp2inc:
	@echo Updating ocp-autoconf/config.ocp2inc
	@mkdir -p ocp-autoconf.d
	@echo "(* Automatically generated from Makefile + Makefile.database *)" > ocp-autoconf.d/config.ocp2inc
	@echo "with_extras=$(WITH_EXTRAS);" >> ocp-autoconf.d/config.ocp2inc
	@echo "with_version=$(WITH_VERSION);" >> ocp-autoconf.d/config.ocp2inc
	@echo "has_pgocaml=$(HAS_PGOCAML);" >> ocp-autoconf.d/config.ocp2inc
	@echo "api_host=\"$(API_HOST)\";" >> ocp-autoconf.d/config.ocp2inc
	@echo "api_port=\"$(API_PORT)\";" >> ocp-autoconf.d/config.ocp2inc
	@echo "need_parsexp=$(NEED_PARSEXP);" >> ocp-autoconf.d/config.ocp2inc

build-deps: tezos-explorer.opam
	opam pin tezos-explorer .

DBUPDATER=tzscan-db-updater
DBWITNESS=--witness db-version.txt
include libs/ez-pgocaml/libs/ez-pgocaml/Makefile.ezpg

_obuild:
	ocp-build init

release:
	sudo cp -r www/* /var/www/${DESTSUBDIR}

conf:
	ocp-autoconf

init: _obuild

clean:
	ocp-build clean
	rm -f tzscan-crawler tzscan-api-server db-version.txt
	rm -rf www

distclean: clean
	rm -rf _obuild
	rm -f autoconf/config.ocp2gen \
		autoconf/config.ocpgen \
		autoconf/config.status \
		autoconf/Makefile.config \
		autoconf/ocaml.config.h
