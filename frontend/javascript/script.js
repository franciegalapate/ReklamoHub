// Toggle anonymity (disable/clear name)
const anonymous = document.getElementById('anonymous');
const nameInput = document.getElementById('name');

if (anonymous && nameInput) {
  anonymous.addEventListener('change', () => {
    if (anonymous.checked) {
      nameInput.value = '';
      nameInput.setAttribute('disabled', 'disabled');
    } else {
      nameInput.removeAttribute('disabled');
    }
  });
}

// Show/hide 'Other category' textbox
const categorySelect = document.getElementById('category');
const otherWrap = document.getElementById('category-other-wrap');
const otherInput = document.getElementById('category_other');

function toggleOther() {
  if (!categorySelect || !otherWrap || !otherInput) return;

  const isOther = categorySelect.value === 'Others';
  otherWrap.classList.toggle('hidden', !isOther);
  otherInput.toggleAttribute('required', isOther);

  if (!isOther) {
    otherInput.value = '';
  }
}

if (categorySelect) {
  categorySelect.addEventListener('change', toggleOther);
  toggleOther();
}