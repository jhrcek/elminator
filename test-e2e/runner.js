const fs = require('fs');
const path = require('path');

// Load compiled Elm
const elmPath = path.join(__dirname, 'elm-app', 'elm.js');
const inputPath = path.join(__dirname, 'elm-app', 'input.json');

const flags = JSON.parse(fs.readFileSync(inputPath, 'utf8'));

// Elm writes to global scope
const { Elm } = require(elmPath);

const app = Elm.Main.init({ flags: flags });

app.ports.output.subscribe(function(data) {
  process.stdout.write(JSON.stringify(data));
  process.exit(0);
});
