// dummy data to be replaced by Erlang backend
let complaints = [
    {
        id: 'c1',
        name: 'Lyshael Bernal',
        address: '123 Tiptop, Ambuklao Road, Baguio City',
        category: 'Ingay (Noise)',
        details: 'Maingay na videoke mula sa kapitbahay gabi-gabi. Nakakaabala sa pagtulog ng pamilya.',
        photoUrl: '../assets/logo/logo_subtitle.png',
        status: 'Pending'
    },
    {
        id: 'c2',
        name: 'Andrei Dela Cruz',
        address: 'Purok 5, Brgy. Irisan, Baguio City',
        category: 'Basura (Garbage)',
        details: 'Illegal dumping ng basura sa kanto malapit sa kalsada. Amoy at nag-aakit ng mga peste.',
        photoUrl: '',
        status: 'Ongoing'
    },
    {
        id: 'c3',
        name: 'Anonymous',
        address: 'Tuba Road, Brgy. Camp 7, Baguio City',
        category: 'Kakulangan (Lack of Services)',
        details: 'Ang mga ilaw sa kalsada sa aming lugar ay patay na sa mahigit isang linggo, delikado kapag gabi.',
        photoUrl: '../assets/logo/logo_subtitle.png',
        status: 'Resolved'
    },
];

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
        tr.innerHTML = `
            <td>${complaint.name || 'Anonymous'}</td>
            <td>${complaint.address}</td>
            <td>${complaint.category}</td>
            <td>${complaint.details.substring(0, 50)}...</td>
            <td>
                ${complaint.photoUrl ? `<img src="${complaint.photoUrl}" alt="Photo" class="photo-thumbnail">` : '—'}
            </td>
            <td>
                <select class="status-select status-${complaint.status.toLowerCase()}" data-id="${complaint.id}">
                    <option value="Pending" ${complaint.status === 'Pending' ? 'selected' : ''}>Pending</option>
                    <option value="Ongoing" ${complaint.status === 'Ongoing' ? 'selected' : ''}>Ongoing</option>
                    <option value="Resolved" ${complaint.status === 'Resolved' ? 'selected' : ''}>Resolved</option>
                </select>
            </td>
            <td>
                <a href="#" class="view-details-btn" data-id="${complaint.id}">View Details</a>
            </td>
        `;
        complaintsTableBody.appendChild(tr);
    });
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
