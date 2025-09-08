// Handle form submit via AJAX
const form = document.querySelector(".form");

if (form) {
  form.addEventListener("submit", async (e) => {
    e.preventDefault();

    const submitBtn = form.querySelector("button[type=submit]");
    submitBtn.disabled = false;

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
      submitBtn.disabled = false;
    }
  });
}

// Track complaint by ID
const trackForm = document.getElementById("trackForm");

if (trackForm) {
  trackForm.addEventListener("submit", async (e) => {
    e.preventDefault();

    const id = document.getElementById("trackingId").value.trim();
    const resultDiv = document.getElementById("result");
    const submitBtn = trackForm.querySelector("button[type=submit]");

    submitBtn.disabled = true; // prevent double submit

    if (!id) {
      resultDiv.style.display = "block";
      resultDiv.innerHTML = "<p style='color:red;'>❌ Please enter a tracking ID.</p>";
      submitBtn.disabled = false;
      return;
    }

    try {
      const res = await fetch(`/track_complaint?id=${encodeURIComponent(id)}`);
      const json = await res.json();

      if (res.ok) {
        resultDiv.innerHTML = `
          <p>✅ Complaint <strong>${json.complaint_id}</strong></p>
          <p>Status: <strong>${json.status}</strong></p>
          <p>Category: ${json.category}</p>
          <p>Address: ${json.address}</p>
          <p>Details: ${json.details}</p>
          <p>Date: ${json.date}</p>
        `;
      } else {
        resultDiv.innerHTML = `<p style='color:red;'>⚠️ ${json.error || "Complaint not found."}</p>`;
      }
    } catch (err) {
      console.error(err);
      resultDiv.innerHTML = "<p style='color:red;'>⚠️ Network error. Please try again.</p>";
    } finally {
      resultDiv.style.display = "block";
      submitBtn.disabled = false;
    }
  });
}