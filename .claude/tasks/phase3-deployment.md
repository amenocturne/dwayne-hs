# Phase 3: Deployment

## Progress
- [x] Step 1: Fresh Ubuntu 24.04 install on MacBook Pro 2017
- [x] Step 2: Ansible setup for home_server in vps-config
- [x] Step 2.5: Ethernet adapter + Clash LAN bypass
- [x] Step 3: Reverse tunnel removed, direct SSH as root
- [x] Step 4: Full Ansible run (idempotent, working)
- [x] Step 5: Xray reverse proxy tunnel (VPS <-> home server)
- [x] Step 6: VPS reverse proxy (Caddy wildcard -> Authelia -> tunnel)
- [x] Step 7: Authelia integration for home domain
- [x] Step 7.5: SSH access from internet via Xray tunnel (VPS:2222 → home:22)
- [x] Step 7.6: Radicale CalDAV server on home server (htpasswd auth, Authelia bypass)
- [x] Step 7.7: Calendar import + organization into category-based calendars
- [ ] Step 8: Deploy Dwayne + end-to-end test    <-- next

## Current State
**Tunnel infrastructure complete. Home server services (Radicale CalDAV) deployed and verified. Dwayne app not yet deployed.**

### What's done
- Xray reverse proxy tunnel: bridge (home) connects outbound to portal (VPS) via VLESS Reality
- VPS Caddy: explicit per-app subdomains → Authelia → xray-portal (port 8888)
- Home Caddy: routes by Host header from `home_apps` list to local app ports
- Authelia: per-app domain rules from `home_apps` list (one_factor)
- DNS: wildcard `*.home.amenocturne.space` A record (Cloudflare, DNS only / gray cloud)
- `home_apps` moved to `all.vars` in inventory (shared between VPS and home server)
- End-to-end verified: test page loaded at `https://dwayne.home.amenocturne.space` through Authelia
- SSH from internet: VPS:2222 → Xray tunnel → home server:22 (`ssh home-remote`)

### Architecture
```
HTTP:  Internet → Cloudflare (DNS only) → VPS Caddy (per-app subdomain certs)
         → Authelia (one_factor) → xray-portal (dokodemo-door :8888, dest :8080)
         → tunnel (VLESS Reality :9443) → xray-bridge → caddy-home (:8080)
         → app (e.g., dwayne :3000)

SSH:   Internet → VPS xray-portal (dokodemo-door :2222, dest :22)
         → tunnel → xray-bridge → home server sshd (:22)
```

### Adding a new home server app
1. Add entry to `home_apps` in `all.vars` section of `ansible/inventories/hosts.yml`:
   ```yaml
   home_apps:
     - name: myapp
       subdomain: myapp
       port: 3001
   ```
2. Run both playbooks:
   - `ansible-playbook playbooks/site.yml --tags caddy,authelia` (VPS: Caddy + Authelia config)
   - `ansible-playbook playbooks/home_server.yml --tags tunnel` (Home: bridge + caddy-home)
3. DNS wildcard already covers `myapp.home.amenocturne.space`

### Key files
- `ansible/roles/xray-portal/` — VPS side (VLESS Reality inbound, dokodemo-door)
- `ansible/roles/xray-bridge/` — Home side (VLESS outbound, local Caddy)
- `ansible/roles/caddy/templates/Caddyfile.j2` — VPS Caddy (iterates `home_apps`)
- `ansible/roles/authelia/templates/configuration.yml.j2` — Authelia (iterates `home_apps`)
- `secrets.yml` (root) — all secrets, managed by `scripts/secrets.py` (gitignored)
- `ansible/inventories/hosts.yml` — `home_apps` list (in `all.vars`) and tunnel config

### Gotchas discovered
- Xray Docker image entrypoint is `xray`, so command must be `["run", "-config", "..."]` not `["xray", "run", "..."]`
- Xray Docker image runs as non-root — config files need 0644, not 0600
- Xray confdir mode reads split files from `/usr/local/etc/xray/` by default — must override with `-config` flag
- Cloudflare free Universal SSL only covers `*.domain.com`, not `*.sub.domain.com` — must use DNS-only mode and let Caddy get individual Let's Encrypt certs
- Clash config reload can reset mode from `rule` to `global` — always verify mode after reload
- Clash global mode ignores ALL rules including IP-CIDR DIRECT, breaking LAN SSH
- Bridge freedom outbound: remove `redirect` to support multiple portal destinations (HTTP + SSH)
- Radicale web UI upload is broken — use curl PUT or direct file placement for imports
- CalDAV PUT only accepts single-resource uploads — split multi-event .ics before importing
- Cloudflare proxy (orange cloud) causes loops for VPN users (VPS → Cloudflare → VPS) — use DNS only

## What's next (Step 8)
- Deploy Dwayne app on home server (Docker, port 3000)
- Create Ansible role for Dwayne
- End-to-end test: `https://dwayne.home.amenocturne.space` with real app

## Cleanup (done)
- [x] Killed reverse SSH tunnel on home server (PID 19416)
- [x] Removed home server's SSH key from VPS authorized_keys

## Notes
- Run Ansible from: `cd /Users/skril/Vault/Projects/personal/vps-config/ansible`
- Direct SSH (LAN): `ssh root@192.168.0.105` (uses SOCKS proxy via ~/.ssh/config)
- Remote SSH (internet): `ssh home-remote` (goes through VPS:2222 → Xray tunnel → home:22)
- Server IPs: WiFi 192.168.0.104, Ethernet 192.168.0.105
- `pcie_ports=native` kernel param required for USB-C devices on MacBook Pro 2017
- Clash Verge LAN bypass: SSH routes through SOCKS proxy (see knowledge base)
- Reverse SSH tunnel fallback: `ssh -fN -R 2222:localhost:22 root@168.100.11.130` from server, then `ssh -o ProxyCommand="ssh -W %h:%p -i ~/.ssh/bitlaunch root@168.100.11.130" root@127.0.0.1 -p 2222` from Mac
