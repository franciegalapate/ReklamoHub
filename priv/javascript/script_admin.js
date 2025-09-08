// --- Admin Login Error Display ---
if (window.location.pathname === "/admin_login") {
  const params = new URLSearchParams(window.location.search);
  if (params.get("error") === "1") {
    const form = document.querySelector(".form");
    if (form) {
      const p = document.createElement("p");
      p.style.color = "red";
      p.style.marginBottom = "10px";
      p.textContent = "❌ Invalid username or password.";
      form.prepend(p);
    }
  }
}