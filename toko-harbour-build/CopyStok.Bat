@Echo Off
Echo. BackUp Data ke C:\Toko.Bak
Echo. --------------------------
Md C:\Toko.Bak
Copy D:\Toko\*.Exe C:\Toko.Bak /y
Copy D:\Toko\*.Bat C:\Toko.Bak /y
Copy D:\Toko\*.Set C:\Toko.Bak /y
Copy D:\Toko\*.Dta C:\Toko.Bak /y
Echo.
Echo. BackUp Selesai...
