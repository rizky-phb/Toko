# Deploy Toko Web Flask

Panduan ini menyiapkan auto-deploy aplikasi Flask ke Render dari GitHub.

## Yang sudah disiapkan

- `render.yaml` di root repo untuk Render Blueprint.
- `gunicorn` di `requirements.txt` untuk menjalankan Flask di production.
- Endpoint `/healthz` untuk health check Render.
- `TOKO_SECRET_KEY` dibuat otomatis oleh Render.

## Langkah deploy

1. Buat akun di Render.
2. Upload/push folder project ini ke GitHub.
3. Di Render, pilih **New > Blueprint**.
4. Connect repo GitHub yang berisi file `render.yaml`.
5. Render akan membaca service `toko-web-flask`, install dependency, lalu menjalankan:

   ```bash
   gunicorn app:app
   ```

6. Setelah deploy selesai, buka URL yang diberikan Render.

Setiap kali ada push baru ke branch utama GitHub, Render akan auto-deploy ulang.

## Catatan database

Secara default app memakai SQLite lokal:

```text
toko-web-flask/storage/toko.sqlite
```

Folder `storage/` tidak ikut Git karena berisi database runtime. Di plan free Render, file yang dibuat saat runtime bisa hilang ketika service rebuild/restart. Ini cukup untuk demo, tapi untuk data toko sungguhan pilih salah satu:

- Pakai Render persistent disk dan set environment variable:

  ```text
  TOKO_DATABASE=/var/data/toko.sqlite
  ```

- Atau migrasi ke PostgreSQL ketika data sudah perlu aman untuk produksi.

## Environment variable

Render Blueprint sudah membuat:

- `TOKO_SECRET_KEY`: otomatis dibuat Render.
- `KASSA`: default `1`.

Opsional untuk database persistent:

- `TOKO_DATABASE`: path file SQLite di disk persistent.
