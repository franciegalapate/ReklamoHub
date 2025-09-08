// --- API CALLS ---

const fetchComplaintsFromBackend = async () => {
    try {
        const res = await fetch("/api/complaints");
        if (!res.ok) return [];
        return await res.json();
    } catch (err) {
        console.error("Error fetching complaints:", err);
        return [];
    }
};

const updateComplaintStatusInBackend = async (id, newStatus) => {
  try {
    const res = await fetch("/update_status", {
      method: "POST",
      headers: { "Content-Type": "application/json" },
      body: JSON.stringify({ complaint_id: id, status: newStatus })
    });

    const json = await res.json();

    if (res.ok) {
      // ✅ Update cache with the updated complaint
      const idx = complaintsCache.findIndex(c => c.complaint_id === id);
      if (idx !== -1) {
        complaintsCache[idx] = json;
      }
      return json;
    } else {
      console.error("Failed to update status:", json.error);
      alert("❌ " + (json.error || "Could not update status."));
      return null;
    }
  } catch (err) {
    console.error("Error updating status:", err);
    alert("⚠️ Network error updating status.");
    return null;
  }
};

// --- DOM Elements ---
const complaintsTableBody = document.getElementById('complaints-table-body');
const modalId = document.getElementById('modal-id');
const logoutBtn = document.getElementById('logout-btn');
const modalBackdrop = document.getElementById('modal-backdrop');
const closeModalBtn = document.getElementById('close-modal-btn');
const modalName = document.getElementById('modal-name');
const modalAddress = document.getElementById('modal-address');
const modalCategory = document.getElementById('modal-category');
const modalDescription = document.getElementById('modal-description');
const modalPhoto = document.getElementById('modal-photo');
const modalPhotoContainer = document.getElementById('modal-photo-container');
const statusFilter = document.getElementById('status-filter');

let currentFilter = 'all';
let complaintsCache = []; // hold latest complaints from backend

// Filter
if (statusFilter) {
  statusFilter.addEventListener("change", () => {
    const selected = statusFilter.value;
    let filtered = complaintsCache;

    if (selected !== "all") {
      filtered = complaintsCache.filter(c => c.status === selected);
    }

    renderComplaints(filtered);
  });
}

function renderComplaints(complaints) {
  complaintsTableBody.innerHTML = "";

  complaints.forEach(complaint => {
    const tr = document.createElement("tr");
    const statusClass = complaint.status.replace(/\s+/g, "-").toLowerCase();

    tr.innerHTML = `
      <td>${complaint.complaint_id}</td>
      <td>${complaint.resident || 'Anonymous'}</td>
      <td>${complaint.address}</td>
      <td>${complaint.category}</td>
      <td>${complaint.details.substring(0, 50)}...</td>
      <td>${complaint.img ? `<img src="${complaint.img}" alt="Photo" class="photo-thumbnail">` : '—'}</td>
      <td>
        <select class="status-select status-${statusClass}" data-id="${complaint.complaint_id}">
          <option value="submitted" ${complaint.status === 'submitted' ? 'selected' : ''}>Submitted</option>
          <option value="in progress" ${complaint.status === 'in progress' ? 'selected' : ''}>In Progress</option>
          <option value="resolved" ${complaint.status === 'resolved' ? 'selected' : ''}>Resolved</option>
          <option value="rejected" ${complaint.status === 'rejected' ? 'selected' : ''}>Rejected</option>
        </select>
      </td>
      <td><a href="#" class="view-details-btn" data-id="${complaint.complaint_id}">View Details</a></td>
    `;
    complaintsTableBody.appendChild(tr);
  });
}

// --- Dashboard Rendering ---
const renderTable = async () => {
    complaintsCache = await fetchComplaintsFromBackend();
    complaintsTableBody.innerHTML = '';

    const filteredComplaints = (currentFilter === 'all')
        ? complaintsCache
        : complaintsCache.filter(c => c.status === currentFilter);

    if (filteredComplaints.length === 0) {
        const tr = document.createElement('tr');
        tr.innerHTML = `<td colspan="7" class="text-center py-10 text-muted">No complaints to display for this status.</td>`;
        complaintsTableBody.appendChild(tr);
        return;
    }

    filteredComplaints.forEach(complaint => {
        const tr = document.createElement('tr');
        const statusClass = complaint.status.toLowerCase().replace(/\s/g, '-');
        tr.innerHTML = `
            <td>${complaint.complaint_id}</td> <!-- ✅ New column -->
            <td>${complaint.resident || 'Anonymous'}</td>
            <td>${complaint.address}</td>
            <td>${complaint.category}</td>
            <td>${complaint.details.substring(0, 50)}...</td>
            <td>
                ${complaint.img ? `<img src="${complaint.img}" alt="Photo" class="photo-thumbnail">` : '—'}
            </td>
            <td>
                <select class="status-select status-${statusClass}" data-id="${complaint.complaint_id}">
                    <option value="submitted" ${complaint.status === 'submitted' ? 'selected' : ''}>Submitted</option>
                    <option value="in progress" ${complaint.status === 'in progress' ? 'selected' : ''}>In Progress</option>
                    <option value="resolved" ${complaint.status === 'resolved' ? 'selected' : ''}>Resolved</option>
                    <option value="rejected" ${complaint.status === 'rejected' ? 'selected' : ''}>Rejected</option>
                </select>
            </td>
            <td>
                <a href="#" class="view-details-btn" data-id="${complaint.complaint_id}">View Details</a>
            </td>
        `;
        complaintsTableBody.appendChild(tr);
    });
};

// --- Helpers ---
const updateStatusColor = (selectElement) => {
    const newStatus = selectElement.value;
    selectElement.classList.remove(
        'status-submitted',
        'status-in-progress',
        'status-resolved',
        'status-rejected'
    );
    const statusClass = newStatus.toLowerCase().replace(/\s/g, '-');
    selectElement.classList.add(`status-${statusClass}`);
};

// --- Event Listeners ---
document.addEventListener('DOMContentLoaded', renderTable);

statusFilter.addEventListener('change', (e) => {
    currentFilter = e.target.value;
    renderTable();
});

complaintsTableBody.addEventListener('change', async (e) => {
  if (e.target.classList.contains('status-select')) {
    const id = e.target.dataset.id;
    const newStatus = e.target.value;

    const updatedComplaint = await updateComplaintStatusInBackend(id, newStatus);

    if (updatedComplaint) {
      updateStatusColor(e.target);

      // Show confirmation message
      const msgDiv = document.getElementById("status-message");
      msgDiv.innerHTML = `<p style="color: green;">✅ Complaint ${updatedComplaint.complaint_id} updated to <strong>${updatedComplaint.status}</strong></p>`;

      // Hide after 3 seconds
      setTimeout(() => {
        msgDiv.innerHTML = "";
      }, 3000);
    }
  }
});

complaintsTableBody.addEventListener('click', (e) => {
    if (e.target.classList.contains('view-details-btn')) {
        e.preventDefault();
        const id = e.target.dataset.id;
        const complaint = complaintsCache.find(c => c.complaint_id == id);
        if (complaint) {
            modalId.textContent = complaint.complaint_id;
            modalName.textContent = complaint.resident || 'Anonymous';
            modalAddress.textContent = complaint.address;
            modalCategory.textContent = complaint.category;
            modalDescription.textContent = complaint.details;

            if (complaint.img) {
                modalPhoto.src = complaint.img;
                modalPhoto.alt = 'Complaint photo';
                modalPhotoContainer.style.display = 'block';
            } else {
                modalPhotoContainer.style.display = 'none';
            }

            modalBackdrop.classList.add('visible');
        }
    }
});

closeModalBtn.addEventListener('click', () => {
    modalBackdrop.classList.remove('visible');
});

modalBackdrop.addEventListener('click', (e) => {
    if (e.target.id === 'modal-backdrop') {
        modalBackdrop.classList.remove('visible');
    }
});

logoutBtn.addEventListener('click', () => {
    document.cookie = "admin=; Path=/; expires=Thu, 01 Jan 1970 00:00:00 GMT";
    window.location.href = "/admin_login";
});