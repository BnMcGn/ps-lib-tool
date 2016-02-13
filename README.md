Cl-Npm: A simple wrapper for npm and webpack.

Operating environment
---------------------

Cl-npm uses the following special variables to configure it's environment:
*cache-path* *npm-executable-path* *node-executable-path*
*webpack-executable-path*

*cache-path* is where downloaded javascript packages will be placed. By default it is the cache/ subdirectory of the cl-npm library.

*npm-executable-path* and *node-executable-path* specify the npm and node executables respectively. Cl-npm will fail if they can't be found.

*webpack-executable-path* is where webpack can be found. By default it is set to nil and cl-npm will install webpack into *cache-path*.

Package.json functions
----------------------

The functions load-package-json and save-package-json are for conveniently accessing the package.json file of the current cache. They use cl-json to convert the file to and from json.

Npm commands
------------

npm-version
update
install
uninstall 

See the [npm documentation](https://docs.npmjs.com/)

Webpack
-------

webpack

See the [webpack documentation](http://webpack.github.io/docs/)
