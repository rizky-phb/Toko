Xbase++ build notes
===================

Folder ini disiapkan untuk Alaska Xbase++/ProjectBuilder.

Build:

  1. Install Alaska Xbase++.
  2. Pastikan xpp.exe, alink.exe, arc.exe, dan pbuild.exe ada di PATH.
  3. Jalankan:

       build-all.bat

Output ditargetkan ke:

  C:\Users\User\Downloads\Toko\toko-64-bit

Catatan:

  - Source asli disimpan di ..\legacy-src.
  - Source adaptasi ada di ..\src.
  - Program lama membaca nomor kassa dari environment variable KASSA.
  - build-all.bat mengatur KASSA=1 untuk default.
  - Ini port awal, kemungkinan masih perlu koreksi compile error khusus Xbase++.

