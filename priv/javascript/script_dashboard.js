const fetchComplaintsFromBackend = async () => {
    const res = await fetch("/admin_dashboard");
    return res.ok ? await res.json() : [];
};

const updateComplaintStatusInBackend = async (id, newStatus) => {
    await fetch("/update_status", {
        method: "POST",
        headers: { "Content-Type": "application/json" },
        body: JSON.stringify({ complaint_id: id, status: newStatus })
    });
};

// --- DOM Elements ---
const complaintsTableBody = document.getElementById('complaints-table-body');
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

let currentFilter = 'All';


/**
 * Simulates fetching complaints from the database.
 * @returns {Promise<Array>} 
 *
 * replace this with a fetch() call
 */
const fetchComplaintsFromBackend = () => {
    return new Promise(resolve => {
        setTimeout(() => {
            resolve(complaints);
        }, 500); // nominal delay to simulate network
    });
};


// --- Dashboard Rendering Logic ---
const renderTable = async () => {
    const complaintsData = await fetchComplaintsFromBackend();
    complaintsTableBody.innerHTML = '';

    const filteredComplaints = (currentFilter === 'All')
        ? complaintsData
        : complaintsData.filter(c => c.status === currentFilter);

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
            <td>${complaint.name || 'Anonymous'}</td>
            <td>${complaint.address}</td>
            <td>${complaint.category}</td>
            <td>${complaint.details.substring(0, 50)}...</td>
            <td>
                ${complaint.photoUrl ? `<img src="${complaint.photoUrl}" alt="Photo" class="photo-thumbnail">` : '—'}
            </td>
            <td>
                <select class="status-select status-${statusClass}" data-id="${complaint.id}">
                    <option value="Submitted" ${complaint.status === 'Submitted' ? 'selected' : ''}>Submitted</option>
                    <option value="In Progress" ${complaint.status === 'In Progress' ? 'selected' : ''}>In Progress</option>
                    <option value="Resolved" ${complaint.status === 'Resolved' ? 'selected' : ''}>Resolved</option>
                    <option value="Rejected" ${complaint.status === 'Rejected' ? 'selected' : ''}>Rejected</option>
                </select>
            </td>
            <td>
                <a href="#" class="view-details-btn" data-id="${complaint.id}">View Details</a>
            </td>
        `;
        complaintsTableBody.appendChild(tr);
    });
};

// ----Change status color -------
// Function to update the status in the UI
const updateStatusColor = (selectElement) => {
    const newStatus = selectElement.value;
    // Remove all existing status classes
    selectElement.classList.remove(
        'status-submitted',
        'status-in-progress',
        'status-resolved',
        'status-rejected'
    );
    // Add the new status class based on the selected value
    const statusClass = newStatus.toLowerCase().replace(/\s/g, '-');
    selectElement.classList.add(`status-${statusClass}`);
};

// --- Event Listeners ---
document.addEventListener('DOMContentLoaded', renderTable);

// Handle filter change
statusFilter.addEventListener('change', (e) => {
    currentFilter = e.target.value;
    renderTable();
});

// Handle status change
complaintsTableBody.addEventListener('change', (e) => {
    if (e.target.classList.contains('status-select')) {
        const id = e.target.dataset.id;
        const newStatus = e.target.value;
        // Find the complaint in the dummy data and update its status
        const complaint = complaints.find(c => c.id === id);
        if (complaint) {
            complaint.status = newStatus;
        }

        // Call the function to update the UI color
        updateStatusColor(e.target);

        // Simulate a backend update (this part is already there)
        updateComplaintStatusInBackend(id, newStatus);
    }
});

// Handle view details button click (This remains the same)
complaintsTableBody.addEventListener('click', (e) => {
    if (e.target.classList.contains('view-details-btn')) {
        e.preventDefault();
        const id = e.target.dataset.id;
        const complaint = complaints.find(c => c.id === id);
        if (complaint) {
            modalName.textContent = complaint.name || 'Anonymous';
            modalAddress.textContent = complaint.address;
            modalCategory.textContent = complaint.category;
            modalDescription.textContent = complaint.details;
            
            if (complaint.photoUrl) {
                modalPhoto.src = complaint.photoUrl;
                modalPhoto.alt = 'Complaint photo';
                modalPhotoContainer.style.display = 'block';
            } else {
                modalPhotoContainer.style.display = 'none';
            }

            modalBackdrop.classList.add('visible');
        }
    }
});

// Handle modal close
closeModalBtn.addEventListener('click', () => {
    modalBackdrop.classList.remove('visible');
});

modalBackdrop.addEventListener('click', (e) => {
    if (e.target.id === 'modal-backdrop') {
        modalBackdrop.classList.remove('visible');
    }
});

// Handle logout
logoutBtn.addEventListener('click', () => {
});




// --- Event Listeners ---
document.addEventListener('DOMContentLoaded', renderTable);

// Handle filter change
statusFilter.addEventListener('change', (e) => {
    currentFilter = e.target.value;
    renderTable();
});

// Handle status change
complaintsTableBody.addEventListener('change', (e) => {
    if (e.target.classList.contains('status-select')) {
        const id = e.target.dataset.id;
        const newStatus = e.target.value;
        updateComplaintStatusInBackend(id, newStatus);
    }
});

// Handle view details button click
complaintsTableBody.addEventListener('click', (e) => {
    if (e.target.classList.contains('view-details-btn')) {
        e.preventDefault();
        const id = e.target.dataset.id;
        const complaint = complaints.find(c => c.id === id);
        if (complaint) {
            modalName.textContent = complaint.name || 'Anonymous';
            modalAddress.textContent = complaint.address;
            modalCategory.textContent = complaint.category;
            modalDescription.textContent = complaint.details;
            
            if (complaint.photoUrl) {
                modalPhoto.src = complaint.photoUrl;
                modalPhoto.alt = 'Complaint photo';
                modalPhotoContainer.style.display = 'block';
            } else {
                modalPhotoContainer.style.display = 'none';
            }

            modalBackdrop.classList.add('visible');
        }
    }
});

// Handle modal close
closeModalBtn.addEventListener('click', () => {
    modalBackdrop.classList.remove('visible');
});

modalBackdrop.addEventListener('click', (e) => {
    if (e.target.id === 'modal-backdrop') {
        modalBackdrop.classList.remove('visible');
    }
});

// Handle logout
logoutBtn.addEventListener('click', () => {
});
