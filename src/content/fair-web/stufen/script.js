// The banner: both answers close it, and the answer counts.
function closeBanner() {
  document.getElementById('banner').remove();
}

// Sending: an empty name gets a sentence beside it, not only a red frame.
document.getElementById('form').addEventListener('submit', (event) => {
  event.preventDefault();
  const name = document.getElementById('name');
  const error = document.getElementById('name-error');
  if (name.value.trim() === '') {
    name.classList.add('error');
    name.setAttribute('aria-describedby', 'name-error');
    error.hidden = false;
    name.focus();
    return;
  }
  name.classList.remove('error');
  error.hidden = true;
  document.getElementById('thanks').textContent = 'Danke, ' + name.value + '!';
});
