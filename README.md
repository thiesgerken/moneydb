# MoneyDB

![Build status](https://git.thiesgerken.de/thies/moneydb/badges/master/build.svg)

Yet another tool to manage private finances.

Consists of the following components:

- REST server and a client application, both written in haskell.
- Web app that can be built into the server binary
- Android app

## How to build

In a clean working directory `make` will build the server, client and the android app.
It will also package the server into a debian package.
Build output can be found in the `dist` directory.
To be CI-friendly, most targets in the [Makefile](Makefile) do not specify dependencies and therefore will not be rerun if the output already exists (even if it is outdated).
In this case run `make clean`.

Detailed instructions for each component:

### Web app

Uses the package base of [`npm`](https://www.npmjs.com/) for dependencies, which can also be managed by [Yarn](https://yarnpkg.com/lang/en/).
The website uses [Semantic UI](https://semantic-ui.com/) and also makes heavy use of [jQuery](https://jquery.com/)
The code itself resides in [web/src](web/src) and should be processed by the web bundler [Parcel](https://parceljs.org/) because it relies on a module loader (Webpack might also work after some reconfiguring).
To install the dependencies run `yarn install` (or `npm install`) in the `web` directory.

To be able to package the app a java script client library for the REST api exposed by `moneydb-server` is required.
The server can generate this for you by running

```shell
moneydb-server --js > web/src/moneydb.js
```

There are two scripts defined in `web/package.json` which call `parcel` in two different ways:

- `yarn start` (or `npm run-script start`) compiles it in `debug`-mode, i.e. without minifying the resulting java script and assets, enabling hot module replacement and so on. The command will then watch for changes in `web/src` to allow for more comfortable development.
- `yarn build` (or `npm run-script build`) compiles in `release`-mode, i.e. with minifying and without watching for further file changes. It will also minify svg files and create the file `dist/report.html` with an analysis of the bundled files.

In both cases the output will go to `web/dist`, but only the `build` target empties this directory beforehand.
The web app will try to make REST requests to `/api/`, so either use `moneydb-server` to serve both or configure your web server appropriately.

The project is set up to use [ESLint](https://eslint.org/), which you can invoke by running

```shell
yarn exec eslint src
```

in the [web](web) directory. Fix problems by appending `--fix` or `--fix-dry-run` to this command.
It will not fix the indentation, because I like to use [`js-beautify`](https://github.com/beautify-web/js-beautify) for this.
Linting of html files can be done by typing

```shell
yarn htmlhint "src/**/*.html"
```

which will launch [HTMLHint](https://github.com/htmlhint/HTMLHint).

### Haskell code

To build the code you will need `mysql` and `postgres` development libraries and the Haskell tool [`stack`](https://docs.haskellstack.org/en/stable/README/), which will download the appropriate GHC and install required packages by running `stack setup -j4` in the project directory.
In `docker` you will find a Dockerfile that that can be used to create an Ubuntu 18.04 image with the neccessary dependencies pre-installed.
See `docker/build.sh` for commands on how to build and push the image (if you are logged in to this GitLab's container registry)

```shell
stack build -j4
```

will then compile the server and client applications.
In this case, the `web/dist` directory is embedded in the executable, so make sure that the directory exists.
It can be empty, e.g. if the resulting binary should only be used to create `moneydb.js` and allow bundling of the web app (also refer to the `Makefile` target which compiles the server twice for this). A change in the web directory will trigger a recompilation by `stack build`.

```shell
stack build -j4 --flag moneydb:localwww
```

will not embed the web app into the executable.
Instead, the server will (try to) serve everything that is not an API endpoint from `./web/dist` at runtime, allowing for development of the web app without having to recompile the server after each change (use with `yarn start`).

Supply `-jN` to the `stack` commands above to compile with `N` threads.

For linting I like to use [`hlint`](https://github.com/ndmitchell/hlint) and [`weeder`](https://github.com/ndmitchell/weeder). Indentation of import lists is a job for [`stylish-haskell`](https://github.com/jaspervdj/stylish-haskell).

### Android app

Just import the project into [Android Studio](https://developer.android.com/studio/).
If the Android SDK is installed, you can also try to invoke `./gradlew build` from the `app` directory.

The app needs a client library for the API endpoints as well and searches for it in `app/app/libs/moneydb-client-2.0.0.jar`.
There is a `make` target to create this library from `dist/swagger.json` (output of `moneydb-server --spec`).

## How to use

### `moneydb-server`

After compilation, you can use

```shell
stack exec moneydb-server -- --help
```

to run the server and show available options (note the `--` to separate `stack` options from options passed to `moneydb-server`).
`stack install` will copy the binaries to a location like `~/.local/bin` and the `make` target will put them in `dist` where they can be executed directly.

If you used `make` to generate `moneydb-server.deb` you can transfer and install this package on a Ubuntu 18.04 machine (dependencies: `libpq5`, `libmysqlclient20` and `libatomic1`). The package also provides a `systemd` unit for the server, which is enabled and started upon install.

```shell
sudo dpkg -i moneydb-server.deb
which moneydb-server
moneydb-server --help
```

For production use a database backend like MySQL or PostgreSQL instead of SQLite is recommended.
The database connection can be configured in the config file, a stub can be found in [server/moneydb.cfg](server/moneydb.cfg).
The server searches for this config file in `./moneydb.cfg` and `/etc/moneydb/moneydb.cfg` (in that order), or a different file when using the `-c` option.

In production use it is recommended to backup the database regularly. If you do not like SQL dumps use `moneydb-server --export` and `moneydb-server --import` to export/import the database to/from a JSON file.

#### Setup PostgreSQL

SQLite is very nice for small databases (a few hundred expenses) and for development, but tends to be the bottleneck in production use.
A faster alternative is PostgreSQL, which can be installed via the `postgres` package on Arch Linux.
Its [Arch wiki page](https://wiki.archlinux.org/index.php/PostgreSQL) is very helpful, a short summary:

Initial configuration:

```shell
sudo -u postgres -i
initdb --locale de_DE.UTF-8 -E UTF8 -D '/var/lib/postgres/data'
```

Start and enable the postgres service, and after becoming the `postgres` user again run

```shell
createuser --interactive
createdb moneydb -O [username]
```

If you use the same username for `createuser` (and `createdb`) as the system user (`moneydb` ) you will be able to connect to the database without authentication.
This worked out of the box on my Arch System, but needed some tweaking on a Ubuntu machine (change everything but the first entry's method to `trust` in `/etc/postgresql/10/main/pg_hba.conf`).
Make sure that the database only accepts connection from `localhost` (should be the default behaviour).

A suitable database configuration for `moneydb-server` would then be

```conf
postgresql {
 connections = 25
 host  = "localhost"
 port  = 5433
 db  = "moneydb"
 user  = ""
 pass  = ""
}
```

Then fill this database by `moneydb-server --import` or `moneydb-server --demo`, which have to be run as the user `moneydb`, i.e.

```shell
sudo -u moneydb moneydb-server --import < backup.json
sudo systemctl restart moneydb-server
```

### `moneydb-client`

The client app can be invoked in the same way as the server binary.
Its primary use is to deliver automatically grabbed expenses to the server (separated from the server for the likely case that you want to keep your banking credentials encrypted at home and not on a server in the internet).
See the [client](client) directory for scripts which use it in combination with [FinTS](https://www.hbci-zka.de/).
