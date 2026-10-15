// Schickt 200 Pixel auf einmal, als würden 200 Leute gleichzeitig malen.
// Nur für den eigenen Server auf localhost: Fremde Server so zu überschütten,
// ist ein Angriff.
const socket = new WebSocket('ws://localhost:8000/ws');
let back = 0;

socket.addEventListener('open', () => {
  for (let i = 0; i < 200; i++) {
    socket.send(JSON.stringify({ x: i % 64, y: Math.floor(i / 64) % 40, color: i % 16 }));
  }
  console.log('200 Pixel geschickt.');
});

// Fertig, sobald der Server alle 200 Pixel an alle verteilt hat
socket.addEventListener('message', (event) => {
  if (JSON.parse(event.data).type === 'pixel' && ++back === 200) {
    console.log('200 Pixel zurückbekommen.');
    socket.close();
  }
});
