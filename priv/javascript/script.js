// Handle form submit via AJAX
if (window.location.pathname === "/" || window.location.pathname === "/index.html") {
  const form = document.querySelector(".form");

  if (form) {
    form.addEventListener("submit", async (e) => {
      e.preventDefault();

      const submitBtn = form.querySelector("button[type=submit]");
      submitBtn.disabled = false;

      // Create FormData for file upload
      const formData = new FormData();
      
      // Add form fields
      formData.append("resident", document.getElementById("anonymous").checked
        ? ""
        : document.getElementById("name").value);
      formData.append("address", document.getElementById("address1").value);
      formData.append("category", document.getElementById("category").value);
      formData.append("details", document.getElementById("details").value);
      
      // Add image file if selected
      const photoFile = document.getElementById("photo").files[0];
      if (photoFile) {
        formData.append("photo", photoFile);
      }

      // Validate required fields
      const address = formData.get("address");
      const category = formData.get("category");
      const details = formData.get("details");

      if (!address || !category || !details) {
        const missing = [];
        if (!address) missing.push("address");
        if (!category) missing.push("category");
        if (!details) missing.push("details");
        
        alert("❌ Please fill in: " + missing.join(", "));
        submitBtn.disabled = false;
        return;
      }

      try {
        const res = await fetch("/submit_complaint", {
          method: "POST",
          body: formData,
        });

        const json = await res.json();

        if (res.ok) {
          // Display the full complaint record
          let imageInfo = '';
          if (json.img) {
            imageInfo = `Image: ${json.img}\n`;
          }
          
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
          ${imageInfo}
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
        let imageHtml = '';
        if (json.img) {
          imageHtml = `<p>Image: <img src="/uploads/${json.img}" alt="Complaint image" style="max-width: 300px; max-height: 200px; border: 1px solid #ccc; margin-top: 10px;" /></p>`;
        }
        
        resultDiv.innerHTML = `
          <p>✅ Complaint <strong>${json.complaint_id}</strong></p>
          <p>Status: <strong>${json.status}</strong></p>
          <p>Category: ${json.category}</p>
          <p>Address: ${json.address}</p>
          <p>Details: ${json.details}</p>
          <p>Date: ${json.date}</p>
          ${imageHtml}
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