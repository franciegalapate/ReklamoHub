# ReklamoHub

<p align="center">
  <img src="priv/assets/logo/logo_subtitle.png" alt="ReklamoHub Logo" width="300"/>
</p>

A web-based community complaint management system built with **Erlang/OTP** and **MySQL**. ReklamoHub allows residents to submit barangay complaints, track their status, and enables barangay officers to manage and update complaints in real time through a live dashboard.

---

## ✨ Features

### For Residents

- **Submit a Complaint** — Fill out a form with your name (or submit anonymously), address, complaint category, details, and an optional photo
- **Track a Complaint** — Look up any complaint by its tracking ID (e.g. `CMP-0005`) to check its current status

### For Barangay Officers (Admin)

- **Secure Login** — Admin authentication with cookie-based session management
- **Live Dashboard** — View all complaints in a table with real-time updates via WebSocket — no page refresh needed
- **Filter by Status** — Filter the complaints table by `Submitted`, `In Progress`, `Resolved`, or `Rejected`
- **Update Complaint Status** — Change the status of any complaint directly from the dashboard
- **View Complaint Details** — Click any complaint to open a modal with full details and the attached photo (if any)

### Complaint Categories

Noise, Garbage, Water, Road, Street Lights, Drainage / Flooding, Health & Safety

---

## 🛠️ Tech Stack

| Layer             | Technology                          |
| ----------------- | ----------------------------------- |
| **Backend**       | Erlang/OTP (gen_server, supervisor) |
| **Web Framework** | Cowboy 2.10 (HTTP + WebSocket)      |
| **Database**      | MySQL 8+                            |
| **DB Driver**     | mysql-otp                           |
| **JSON**          | jsx                                 |
| **Frontend**      | HTML, CSS, Vanilla JavaScript       |
| **Build Tool**    | Rebar3                              |

---

## 📁 Project Structure

```
ReklamoHub/
├── src/
│   ├── reklamohub_app.erl        # OTP application entry point
│   ├── reklamohub_sup.erl        # Supervisor tree
│   ├── reklamohub_router.erl     # URL routing (maps paths to handlers)
│   ├── complaint_handler.erl     # Submit & track complaint logic
│   ├── admin_handler.erl         # Admin login, dashboard, status update
│   ├── admin_ws_handler.erl      # WebSocket handler for live dashboard
│   ├── dashboard_manager.erl     # gen_server — manages WS subscribers & broadcasts
│   ├── reklamohub_db.erl         # Database query functions
│   ├── db_manager.erl            # MySQL connection pool manager
│   ├── db_config.erl             # DB connection config (host, port, credentials)
│   └── image_handler.erl         # Image upload handling
├── priv/
│   ├── html/
│   │   ├── index.html            # Resident complaint form
│   │   ├── track_complaints.html # Complaint tracking page
│   │   ├── admin_login.html      # Admin login page
│   │   └── admin_dashboard.html  # Barangay officer dashboard
│   ├── css/
│   │   └── style.css
│   ├── javascript/
│   │   ├── script.js             # Resident-facing logic
│   │   ├── script_admin.js       # Admin login logic
│   │   └── script_dashboard.js   # Dashboard + WebSocket client
│   ├── assets/
│   │   ├── logo/
│   │   └── font/                 # Poppins font files
│   ├── sql/
│   │   └── reklamohub_db.sql     # Database schema + seed data
│   └── uploads/                  # Uploaded complaint photos
├── rebar.config                  # Dependencies and release config
└── rebar.lock
```

---

## ⚙️ Prerequisites

- **Erlang/OTP** 24 or higher
- **Rebar3** (bundled as `./rebar3`)
- **MySQL** 8.0 or higher

---

## 🚀 Setup & Running

### 1. Set Up the Database

Start MySQL and run the provided SQL dump to create the database, tables, and seed data:

```bash
mysql -u root -p < priv/sql/reklamohub_db.sql
```

This creates the `reklamohub_db` database with the `complaints` and `admin` tables.

### 2. Configure the Database Connection

Edit `src/db_config.erl` to match your MySQL credentials:

```erlang
get_db_config() ->
    #{
        host     => "localhost",
        port     => 3306,
        user     => "root",
        password => "your_password_here",   %% ← update this
        database => "reklamohub_db",
        ...
    }.
```

### 3. Build and Run

```bash
./rebar3 deps        # Fetch dependencies
./rebar3 compile     # Compile the project
./rebar3 shell       # Start the server
```

### 4. Open in Browser

```
http://localhost:8080
```

---

## 📄 Pages & Routes

| Route                              | Description                                 |
| ---------------------------------- | ------------------------------------------- |
| `GET /`                            | Resident complaint submission form          |
| `GET /track_complaints.html`       | Complaint tracking page                     |
| `GET /admin_login`                 | Admin login page                            |
| `GET /admin_dashboard`             | Barangay officer dashboard (requires login) |
| `POST /submit_complaint`           | API — submit a new complaint                |
| `GET /track_complaint?id=CMP-XXXX` | API — fetch complaint status by ID          |
| `POST /update_status`              | API — update complaint status (admin only)  |
| `WS /admin_dashboard`              | WebSocket — live dashboard updates          |

---

## 🔐 Default Admin Credentials

> ⚠️ Change these in the database before deploying!

| Username | Password |
| -------- | -------- |
| `admin`  | `admin`  |

---

## 🗄️ Database Schema

**`complaints`**

| Column         | Type         | Description                                        |
| -------------- | ------------ | -------------------------------------------------- |
| `complaint_id` | INT (auto)   | Primary key                                        |
| `resident`     | VARCHAR(100) | Resident name (nullable for anonymous)             |
| `category`     | VARCHAR(50)  | Complaint category                                 |
| `status`       | ENUM         | `submitted`, `in progress`, `resolved`, `rejected` |
| `date`         | TIMESTAMP    | Submission timestamp                               |
| `address`      | VARCHAR(255) | Resident address                                   |
| `details`      | TEXT         | Full complaint description                         |
| `img`          | VARCHAR(255) | Path to uploaded photo (nullable)                  |

Complaints are queried through the `complaints_view` view, which formats the ID as `CMP-XXXX`.

---

## 📸 Screenshots

> Add screenshots to a `screenshots/` folder and update the paths below.

| Complaint Form                                    | Track Complaint                                     |
| ------------------------------------------------- | --------------------------------------------------- |
| ![Complaint Form](screenshots/complaint_form.png) | ![Track Complaint](screenshots/track_complaint.png) |

| Admin Login                                 | Admin Dashboard                                     |
| ------------------------------------------- | --------------------------------------------------- |
| ![Admin Login](screenshots/admin_login.png) | ![Admin Dashboard](screenshots/admin_dashboard.png) |
