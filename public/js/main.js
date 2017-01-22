//Socket IO Client Side
var socket = io();

socket.on('live-reload', () => {
	console.log("receiving");
  window.location.reload();
});

