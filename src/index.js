'use strict';

require('./index.html');

var Elm = require('./Main');

var myapp = Elm.Main.embed(document.getElementById('main'));
