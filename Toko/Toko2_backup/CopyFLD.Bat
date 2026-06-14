@Echo Off
Cls
Echo. BackUp Data ke FlashDisk.
Echo. Pastikan FlashDisk sudah terpasang.
Echo. Tekan ENTER=Lanjut     CTRL+C=Batal
Echo. -----------------------------------
Pause > Nul
Md E:\Toko.Bak
Copy D:\Toko\*.Exe E:\Toko.Bak /y
Copy D:\Toko\*.Bat E:\Toko.Bak /y
Copy D:\Toko\*.Set E:\Toko.Bak /y
Copy D:\Toko\*.Dta E:\Toko.Bak /y
Echo.
Echo. BackUp Selesai.  Tekan sembarang tombol...
Echo.
Pause > Nul
