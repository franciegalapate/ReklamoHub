// Handle form submit via AJAX
const form = document.querySelector(".form");

if (form) {
  form.addEventListener("submit", async (e) => {
    e.preventDefault();

    const submitBtn = form.querySelector("button[type=submit]");
    submitBtn.disabled = true; // prevent double submit

    const data = {
      resident: document.getElementById("anonymous").checked
        ? null
        : document.getElementById("name").value,
      address: document.getElementById("address1").value,
      category: document.getElementById("category").value,
      details: document.getElementById("details").value,
      img: document.getElementById("photo").files[0]?.name || null
    };

    try {
      const res = await fetch("/submit_complaint", {
        method: "POST",
        headers: { "Content-Type": "application/json" },
        body: JSON.stringify(data),
      });

      const json = await res.json();

      if (res.ok) {
        // Display the full complaint record
        const msg = `
        ✅ Complaint submitted successfully!
        -------------------------------
        Tracking ID: ${json.complaint_id}
        Resident: ${json.resident || "Anonymous"}
        Category: ${json.category}
        Status: ${json.status}
        Date: ${json.date}
        Address: ${json.address}
        Details: ${json.details}
        `;

        alert(msg.trim());
        form.reset();
      } else {
        alert(`❌ Error: ${json.error || "Could not save complaint"}`);
      }
    } catch (err) {
      console.error(err);
      alert("⚠️ Network error. Please try again.");
    } finally {
      submitBtn.disabled = false; // re-enable button
    }
  });
}