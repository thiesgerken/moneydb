default: dist/moneydb-server dist/moneydb-server.deb dist/swagger.json dist/moneydb.js dist/moneydb.apk web/dist

# delete build outputs and its intermediate files
clean:
	rm -Rf dist app/app/libs/moneydb-client-2.0.0.jar .tmp/moneydb-server web/dist web/src/moneydb.js

# what clean does + delete stack workspace
dist-clean:
	make clean && rm -Rf .tmp .stack-work

.PHONY: clean distclean default web/dist

dist/moneydb-server.deb: dist/moneydb-server
	bash ./server/debian/package.sh

dist/moneydb-client-2.1.0-1-x86_64.pkg.tar.xz: dist/moneydb-client
	bash ./client/arch/package.sh

# needs web/src/moneydb.js
web/dist: dist/moneydb.js
	cp dist/moneydb.js web/src/moneydb.js
	(cd web && yarn install && yarn run build)

dist/moneydb-server: # $(shell find lib server client -type f) moneydb.cabal stack.yaml
	mkdir -p web/dist dist
	touch web/dist/index.html
	stack setup
	stack build -j2 --executable-stripping --library-stripping --no-terminal
	stack install --local-bin-path dist

dist/moneydb.js: # dist/moneydb-server
	cd dist && ./moneydb-server --js > moneydb.js

dist/swagger.json: # dist/moneydb-server
	cd dist && ./moneydb-server --spec > swagger.json

app/app/libs/moneydb-client-2.0.0.jar: dist/swagger.json
	mkdir -p .tmp

	if [ ! -f .tmp/swagger-codegen-cli.jar ]; then\
    rm -Rf .tmp/swagger-codegen-3.0.3 && cd .tmp && \
		curl -L -O https://github.com/swagger-api/swagger-codegen/archive/v3.0.3.tar.gz && \
		tar xf v3.0.3.tar.gz && cd swagger-codegen-3.0.3 && mvn package && cd .. && cd ..; \
		cp .tmp/swagger-codegen-3.0.3/modules/swagger-codegen-cli/target/swagger-codegen-cli.jar .tmp; \
	fi

	echo '{"groupId":"de.thiesgerken", "artifactId":"moneydb-client", "artifactVersion":"2.0.0"}' > .tmp/swagger-config.json

	rm -Rf .tmp/android-client
	java -jar .tmp/swagger-codegen-cli.jar generate -c .tmp/swagger-config.json -i dist/swagger.json -l java --library=okhttp-gson -o .tmp/android-client

	( cd .tmp/android-client && mvn package )
	cp .tmp/android-client/target/moneydb-client-2.0.0.jar ./app/app/libs

dist/moneydb.apk: app/app/libs/moneydb-client-2.0.0.jar # $(shell find app -type f)
	(cd app && ./gradlew build)
	mkdir -p dist
	cp app/app/build/outputs/apk/release/app-release.apk dist/moneydb.apk
