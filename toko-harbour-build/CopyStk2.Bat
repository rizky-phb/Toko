@Echo Off
Echo. BackUp Data ke D:\Toko.Bak
Echo. --------------------------
Md D:\Toko.Bak
Copy Z:\*.Exe D:\Toko.Bak /y
Copy Z:\*.Bat D:\Toko.Bak /y
Copy Z:\*.Set D:\Toko.Bak /y
Copy Z:\*.Dta D:\Toko.Bak /y
Echo.
Echo. BackUp Selesai...
