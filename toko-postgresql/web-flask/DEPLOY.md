# Deployment Windows Server 2019

Target VPS: `103.189.235.77`. Sampai domain tersedia, publikasi awal memakai
HTTP. Jangan membuka port PostgreSQL 5432 ke seluruh internet.

## Komponen

- PostgreSQL berjalan lokal di VPS pada `127.0.0.1:5432`.
- Flask/Waitress berjalan lokal pada `127.0.0.1:8000`.
- IIS menerima trafik publik port 80 dan meneruskannya ke Waitress.
- Program Harbour lama tetap terpisah sampai setiap modul selesai dikonversi.

## Database

Jalankan di `psql` sebagai administrator PostgreSQL (ganti password):

```sql
CREATE ROLE toko_app LOGIN PASSWORD 'GANTI_PASSWORD_PANJANG';
CREATE DATABASE toko OWNER toko_app ENCODING 'UTF8';
```

Jangan menyimpan password produksi di source. Buat environment variable sistem:

```powershell
[Environment]::SetEnvironmentVariable(
  'DATABASE_URL',
  'postgresql://toko_app:PASSWORD_URL_ENCODED@127.0.0.1:5432/toko',
  'Machine'
)
[Environment]::SetEnvironmentVariable(
  'TOKO_SECRET_KEY',
  'SECRET_ACAK_MINIMAL_32_KARAKTER',
  'Machine'
)
```

Karakter khusus pada password di URL harus di-percent-encode.

## Aplikasi

```powershell
cd C:\Apps\TokoPostgreSQL\web-flask
py -3.12 -m venv .venv
.\.venv\Scripts\python.exe -m pip install -r requirements.txt
.\.venv\Scripts\python.exe wsgi.py
```

Cek dari VPS: `Invoke-RestMethod http://127.0.0.1:8000/healthz`.

Untuk produksi, daftarkan `wsgi.py` sebagai Windows Service memakai NSSM atau
WinSW, dengan working directory `C:\Apps\TokoPostgreSQL\web-flask`. Service
harus memakai akun khusus dengan hak minimum, bukan Administrator.

## IIS

Install IIS, URL Rewrite, dan Application Request Routing (ARR). Aktifkan proxy
ARR lalu buat reverse proxy dari port 80 ke `http://127.0.0.1:8000`. Buka hanya
port 80, 443, dan RDP yang dibatasi IP/VPN pada Windows Firewall/idCloudHost.

Setelah membeli domain, arahkan DNS A record ke `103.189.235.77`, pasang
sertifikat TLS, paksa HTTPS, lalu tutup akses HTTP langsung.

## Backup

Jadwalkan `pg_dump -Fc toko` setiap hari ke folder backup terpisah dan salin
backup keluar VPS. Uji restore secara berkala; file backup yang belum pernah
diuji restore belum dapat dianggap aman.
