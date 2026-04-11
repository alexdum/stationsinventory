# Station Inventory Docker Setup

This document explains how to build, run, and develop the Station Inventory Shiny application using Docker and Docker Compose.

## Project Structure

The project follows the standard three-file Shiny app structure for better maintainability.

```text
stationinventory/
├── Dockerfile          # Image definition
├── docker-compose.yml  # Development workflow (Recommended)
├── global.R           # Shared libraries and data
├── ui.R               # UI layout
├── server.R           # Server logic
└── docs/
    └── docker-setup.md # This document
```

---

## 1. Development with Docker Compose (Recommended)

Docker Compose simplifies management and enables **live reloading** by mounting your local code directly into the container.

### Start the Application
Run the following command from the root directory:
```bash
docker compose up -d
```

If you changed the `Dockerfile` or added/updated R package dependencies, rebuild the image first:
```bash
docker compose up -d --build
```

### Live Development Mode
Changes made to `global.R`, `ui.R`, or `server.R` are synchronized instantly. **Simply refresh your browser** after saving a file to see your updates—no rebuilding required.

Code changes reload through the mounted volume, but dependency changes do not. When packages such as `arrow` are added or system libraries change, you need to rebuild the container image.

### Stop the Application
```bash
docker compose down
```

---

## 2. Manual Docker Usage (Legacy)

If you prefer not to use Docker Compose, you can build and run using standard commands:

### Build the Docker Image
```bash
docker build -t stationinventory-app .
```

### Run the Docker Container
```bash
docker run -d --name stationinventory-container -p 3838:3838 stationinventory-app
```

---

## 3. Troubleshooting and Logs

### Resolving Naming Conflicts
If you encounter an error stating `Conflict. The container name "/stationinventory-dev" is already in use`, it means an existing container is blocking the startup.
**Solution:**
```bash
docker stop stationinventory-dev && docker rm stationinventory-dev
```

### Viewing Logs
To view the main Shiny Server logs:
```bash
docker logs stationinventory-dev
```
*Tip: Use `-f` to stream logs in real-time (`docker logs -f stationinventory-dev`).*

### Accessing R-Specific Error Logs
If your app crashes, details are stored in internal logs:
```bash
# List log files
docker exec -it stationinventory-dev ls -la /var/log/shiny-server/

# View a specific log
docker exec -it stationinventory-dev cat /var/log/shiny-server/<log-file-name>.log
```
