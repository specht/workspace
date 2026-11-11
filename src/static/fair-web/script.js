// The banner: »Alle akzeptieren« closes it, »Einstellungen« leads nowhere.
function closeBanner() {
  document.getElementById('banner').remove();
}

function showSettings() {
  alert('Die Einstellungen kommen bald.');
}

// The countdown: five minutes, and after every reload five minutes again.
let left = 300;
setInterval(() => {
  left = Math.max(0, left - 1);
  const minutes = String(Math.floor(left / 60)).padStart(2, '0');
  const seconds = String(left % 60).padStart(2, '0');
  document.getElementById('countdown').textContent = `${minutes}:${seconds}`;
}, 1000);

// Sending: an empty name only turns its field red.
function send() {
  const name = document.getElementById('name');
  if (name.value.trim() === '') {
    name.classList.add('error');
    return;
  }
  name.classList.remove('error');
  alert('Danke, ' + name.value + '!');
}
