/**
/**
 * 23.05.2016
 */

 DEVELOPMENT_MODE = true;

var fs = require('fs');
var path = require('path');
var express = require('express');
var bodyParser = require('body-parser');
var chokidar = require('chokidar');

var app = express();

//Watch for changes in the specified files
 chokidar.watch('./public', {
 persistent: true,
 usePolling: false,
 }).on('ready', () => {
 if (DEVELOPMENT_MODE) console.log('allFilesWatcher Ready')
 }).on('all', (event, path) => {
 io.emit('live-reload');
 });

var PARTICIPANTS_FILE = path.join(__dirname, 'participants.json');

app.set('port', (process.env.PORT || 3000));

app.use('/', express.static(path.join(__dirname, 'public'))); // Use staticServer module within Express 'public' has precedence
app.use('/', bodyParser.json()); //default path is '/'
app.use(bodyParser.urlencoded({extended: true}));

// Additional middleware which will set headers that we need on each request.
app.use(function (req, res, next) {
  // Set permissive CORS header - this allows this server to be used only as
  // an API server in conjunction with something like webpack-dev-server.
  res.setHeader('Access-Control-Allow-Origin', '*');
  res.setHeader("Access-Control-Allow-Headers", "Content-Type");
  // Disable cache to get new participants
  res.setHeader('Cache-Control', 'no-cache');
  next();
});

app.get('/api/participants', function (req, res) {
  fs.readFile(PARTICIPANTS_FILE, function (err, data) {
    if (err) {
      console.error(err);
      process.exit(1);
    }
    res.json(JSON.parse(data));
  });
});

app.post('/api/participants', function (req, res) {
  fs.readFile(PARTICIPANTS_FILE, function (err, data) {
    if (err) {
      console.error(err);
      process.exit(1);
    }
    var participants = JSON.parse(data);
    var newParticipants = [];
    var participantResponse;

    participants = participants.forEach(function (participant) {
      if(participant.index == req.body.index){
        participant.name = req.body.name;
        participant.status = req.body.status;
        participantResponse = participant;
        return newParticipants.push(participant);
      }
      else {
        return newParticipants.push(participant);
      }
    });
    fs.writeFile(PARTICIPANTS_FILE, JSON.stringify(newParticipants, null, 4), function (err) {
      if (err) {
        console.error(err);
        process.exit(1);
      }
      res.json("This is the Node-server response");
    });
  });
});

var server = app.listen(app.get('port'), function () {
  console.log('Server started in localhost:' + app.get('port') + '/');
});

//Socket IO
const io = require('socket.io')(server);